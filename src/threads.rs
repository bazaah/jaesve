#![allow(non_snake_case)]
use {
    crate::{
        models::{
            assets::{BlockGenerator, JsonPacket, OrDisplay, ReadKind},
            check_index,
            error::{Context, ErrContext, ErrorKind, Result},
            eval,
            field::Field,
            get_writer, unwind_json, write_formatted_output, ToBuilder, ToWriter,
        },
        with_log, CLI,
    },
    linereader::LineReader,
    std::{
        convert::TryFrom,
        io::{BufReader, BufWriter, Read as ioRead},
        str::from_utf8,
        sync::mpsc::{sync_channel as syncQueue, Receiver, SyncSender},
        thread::{Builder as Thread, JoinHandle},
    },
};

type WorkerHandle = JoinHandle<Result<()>>;

/// Spawns workers and the channels which communicate input segments.
/// Each input source "from_source" is assigned a new channel, and said channel's
/// rx sent through the "meta channels." This implementation ensures that the control
/// flow mirrors the data flow
pub(crate) fn spawn_workers(from_source: Receiver<ReadKind>) -> Result<WorkerHandle> {
    // Meta channel: |Reader -> Builder|, delivers new receivers to builder
    let (re_bu_tx, re_bu_rx) = syncQueue(0);
    // Meta channel: |Builder -> Writer|, delivers new receivers to writer
    let (bu_wr_tx, bu_wr_rx) = syncQueue(0);

    // Writer
    let writer_h = worker_writer(bu_wr_rx)?;

    // Builder
    let builder_h = worker_builder(re_bu_rx, bu_wr_tx, writer_h)?;

    // Reader
    worker_reader(re_bu_tx, builder_h, from_source)
}

/// Handles the majority of json doc unwinding and parsing.
/// The exact work it does depends on what runtime constraints exist
/// for program output
fn worker_reader(
    m_chan_tx: SyncSender<Receiver<ToBuilder>>,
    builder_h: WorkerHandle,
    read_rx: Receiver<ReadKind>,
) -> Result<WorkerHandle> {
    Thread::new()
        .name(format!("Reader"))
        .spawn(move || -> Result<()> {
            debug!("Reader initialized");
            let result = || -> Result<()> {
                let iter = eval(&Field::Identifier, lazy_eval_ident, &read_rx);
                // Hot loop
                for item in iter {
                    let (data_tx, data_rx): (SyncSender<ToBuilder>, Receiver<ToBuilder>) =
                        syncQueue(10);
                    m_chan_tx.send(data_rx).context(Context::umcc("Builder"))?;
                    match (item, &CLI.by_line()) {
                        ((i, read @ ReadKind::Stdin(_)), true) => {
                            let mut line_reader = LineReader::with_delimiter_and_capacity(
                                CLI.linereader_eol(),
                                CLI.input_buffer_size(),
                                read.into_inner(),
                            );
                            let mut index = i.as_ref().map(|_| 1);
                            // Note that this is an Option inside an Option... the outside option controls when the loop should end (i.e EOF),
                            // the inside option controls whether to store the data
                            while let Some(slice) = line_reader.next_line().map(|res| {
                                if CLI.should_calculate(Field::Value) {
                                    Some(res)
                                } else {
                                    None
                                }
                            }) {
                                if check_index(CLI.regex(), index)
                                    && index
                                        .as_ref()
                                        .map_or(true, |i| *i >= CLI.line_start_number())
                                {
                                    debug!(
                                        "Processing line {} of input {}...",
                                        index.or_untracked(),
                                        i.or_untracked()
                                    );
                                    let reader =
                                        slice.transpose()?.map(|s| s.iter().map(|&b| Ok(b)));
                                    unwind_json(&&CLI, index, reader, data_tx.clone())?;
                                    index = index.map(|i| i + 1);
                                } else {
                                    debug!(
                                        "Skipping line {} of input {}...",
                                        index.or_untracked(),
                                        i.or_untracked()
                                    );
                                    index = index.map(|i| i + 1);
                                }
                            }
                        }
                        ((index, read), _) => {
                            if check_index(CLI.regex(), index) {
                                debug!("Processing input {}...", index.or_untracked());
                                let reader = eval(
                                    &Field::Value,
                                    |b, (cap, read)| {
                                        if b {
                                            Some(BufReader::with_capacity(cap, read).bytes())
                                        } else {
                                            None
                                        }
                                    },
                                    (CLI.input_buffer_size(), read.into_inner()),
                                );
                                unwind_json(&&CLI, index, reader, data_tx)?;
                            } else {
                                debug!("Skipping input {}...", index.or_untracked());
                            }
                        }
                    }
                }

                Ok(())
            };
            // Cleanup
            match result() {
                defer => thread_cleanup(defer, builder_h, m_chan_tx),
            }
        })
        .map_err(|e| e.into())
}

/// Processes the intermediate data sent from the reader,
/// producing and packaging output blocks based on runtime constraints
fn worker_builder(
    m_chan_rx: Receiver<Receiver<ToBuilder>>,
    m_chan_tx: SyncSender<Receiver<ToWriter>>,
    writer_h: WorkerHandle,
) -> Result<WorkerHandle> {
    Thread::new()
        .name(format!("Builder"))
        .spawn(move || -> Result<()> {
            debug!("Builder initialized");
            let result = || -> Result<()> {
                // Hot loop
                while let Some(channel) = m_chan_rx.iter().next() {
                    let (data_tx, data_rx): (SyncSender<ToWriter>, Receiver<ToWriter>) =
                        syncQueue(10);
                    m_chan_tx.send(data_rx).context(Context::umcc("Writer"))?;

                    for packet in channel.iter() {
                        trace!(
                            "Current packet is: {}, {:?}, {:?}",
                            &packet.0.or_untracked(),
                            &packet.1.or_untracked(),
                            packet.2.as_ref().map(|vec| from_utf8(vec)).or_untracked()
                        );
                        let (json, ident, metadata) = JsonPacket::try_from(packet)?.into_inner();
                        let builder = BlockGenerator::new(&CLI, json.as_ref(), metadata);

                        for item in builder
                            .map(|mut output| {
                                output.store(&CLI, ident);
                                output.store(&CLI, Some(CLI.delimiter()));
                                output.store(&CLI, Some(CLI.guard()));
                                output
                            })
                            .filter_map(|output| output.check(CLI.regex()))
                        {
                            trace!("Current in-processing output item is: {:?}", &item);
                            data_tx.send(item.done()).context(Context::udcc())?;
                        }
                        trace!("Finished processing a json pointer");
                    }
                    debug!("Finished processing a json document");
                }
                Ok(())
            };
            // Cleanup
            match result() {
                defer => thread_cleanup(defer, writer_h, m_chan_tx),
            }
        })
        .map_err(|e| e.into())
}

fn worker_writer(m_chan_rx: Receiver<Receiver<ToWriter>>) -> Result<WorkerHandle> {
    Thread::new()
        .name(format!("Writer"))
        .spawn(move || -> Result<()> {
            debug!("Writer initialized");
            let opts = &CLI;
            let mut writer =
                BufWriter::with_capacity(opts.output_buffer_size(), get_writer(opts.writer()));
            info!("Buffered writer initialized");
            let mut result = || -> Result<()> {
                // Hot loop
                while let Some(channel) = m_chan_rx.iter().next() {
                    for output in channel.iter() {
                        write_formatted_output(&mut writer, output, opts.format())?;
                    }
                    debug!("Write channel closing");
                }
                Ok(())
            };
            // Cleanup
            match result() {
                Ok(_) => with_log!(Ok(()), info!("Writer closing... success")),
                Err(e) => with_log!(Err(e), warn!("Writer closing... with error")),
            }
        })
        .map_err(|e| e.into())
}

fn lazy_eval_ident(
    b: bool,
    src: &Receiver<ReadKind>,
) -> impl Iterator<Item = (Option<usize>, ReadKind)> + '_ {
    let yes = if b {
        Some(src.iter().enumerate().map(|(i, r)| (Some(i + 1), r)))
    } else {
        None
    };
    let no = if !b {
        Some(src.iter().map(|r| (None, r)))
    } else {
        None
    };

    yes.into_iter().flatten().chain(no.into_iter().flatten())
}

fn thread_cleanup<T>(deferred: Result<()>, handle: WorkerHandle, tx: SyncSender<T>) -> Result<()> {
    drop(tx);
    let current = std::thread::current();
    let handle_name = handle.thread().name().map(|s| s.to_string());
    handle
        .join()
        .map(|inner| {
            inner.map_err(|err| {
                with_log!(
                    err,
                    warn!(
                        "{} closing... with error(s) propagated by {}",
                        current.name().or_untracked(),
                        handle_name.or_untracked()
                    )
                )
            })
        })
        .map_err(|_| {
            warn!("{} closing... with error", current.name().or_untracked());
            ErrorKind::ThreadFailed
        })
        .context(Context::tp("Writer"))??;
    match deferred {
        Ok(_) => with_log!(
            Ok(()),
            info!("{} closing... success", current.name().or_untracked())
        ),
        Err(e) => with_log!(
            Err(e),
            warn!("{} closing... with error", current.name().or_untracked())
        ),
    }
}
