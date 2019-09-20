#![allow(non_snake_case)]
use {
    crate::{
        cli::ProgramArgs,
        match_with_log,
        models::{
            assets::{JsonPacket, JsonPointer, ReadKind},
            check_index,
            error::{ErrorKind, Result},
            get_writer, unwind_json, write_formatted_output, ToBuilder, ToWriter,
        },
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

/// Spawns workers and the channels which communicate input segments.
/// Each input source "from_source" is assigned a new channel, and said channel's
/// rx sent through the "meta channels." This implementation ensures that the control
/// flow mirrors the data flow
pub(crate) fn spawn_workers(
    opts: &'static ProgramArgs,
    from_source: Receiver<ReadKind>,
) -> Result<JoinHandle<Result<()>>> {
    // Meta channel: |Reader -> Builder|, delivers new receivers to builder
    let (ReBu_tx, ReBu_rx): (
        SyncSender<Receiver<ToBuilder>>,
        Receiver<Receiver<ToBuilder>>,
    ) = syncQueue(0);
    // Meta channel: |Builder -> Writer|, delivers new receivers to writer
    let (BuWr_tx, BuWr_rx): (SyncSender<Receiver<ToWriter>>, Receiver<Receiver<ToWriter>>) =
        syncQueue(0);

    // Writer
    let thWriter = Thread::new()
        .name(format!("Writer"))
        .spawn(move || -> Result<()> {
            debug!("Writer initialized");
            let rx_builder = BuWr_rx;
            let opts = &opts;
            let mut writer =
                BufWriter::with_capacity(opts.output_buffer_size(), get_writer(opts.writer()));
            info!("Buffered writer initialized");
            let mut result = || -> Result<()> {
                // Hot loop
                while let Some(channel) = rx_builder.iter().next() {
                    for output in channel.iter() {
                        trace!("before writing: {:?}", &output);
                        write_formatted_output(&mut writer, output, opts.format())?;
                    }
                    debug!("Write channel closing");
                }
                Ok(())
            };
            // Cleanup
            match result() {
                Ok(_) => match_with_log!(Ok(()), info!("Writer closing... success")),
                Err(e) => match_with_log!(Err(e), warn!("Writer closing... with error")),
            }
        })?;

    // Builder
    let thBuilder = Thread::new()
        .name(format!("Builder"))
        .spawn(move || -> Result<()> {
            debug!("Builder initialized");
            let tx_writer = BuWr_tx;
            let rx_reader = ReBu_rx;
            let opts = &opts;
            let result = || -> Result<()> {
                // Hot loop
                while let Some(channel) = rx_reader.iter().next() {
                    let (data_tx, data_rx): (SyncSender<ToWriter>, Receiver<ToWriter>) =
                        syncQueue(10);
                    tx_writer.send(data_rx).map_err(|_| {
                        ErrorKind::UnexpectedChannelClose(format!(
                            "failed to send next |builder -> writer| channel, writer has hung up"
                        ))
                    })?;

                    for packet in channel.iter() {
                        trace!(
                            "current packet is: {}, {:?}, {:?}",
                            &packet.0,
                            &packet.1,
                            from_utf8(&packet.2)
                        );
                        let (json, metadata) = JsonPacket::try_from(packet)?.into_inner();
                        let builder = JsonPointer::new(opts, &json, metadata);

                        for item in builder
                            .map(|mut output| {
                                output.store(opts, opts.delimiter());
                                output.store(opts, opts.guard());
                                output
                            })
                            .filter_map(|output| output.check(opts.regex()))
                        {
                            trace!("current in-processing output item is: {:?}", &item);
                            data_tx.send(item.done()).map_err(|_| {
                                ErrorKind::UnexpectedChannelClose(format!(
                                    "writer in |builder -> writer| channel has hung up"
                                ))
                            })?;
                        }
                        trace!("Finished processing a json pointer");
                    }
                    debug!("Finished processing a json document");
                }
                Ok(())
            };
            // Cleanup
            match result() {
                defer => {
                    drop(tx_writer);
                    thWriter
                        .join()
                        .map(|inner| {
                            inner.map_err(|err| {
                                match_with_log!(
                                    err,
                                    warn!("Builder closing... with error(s) propagated by Writer")
                                )
                            })
                        })
                        .map_err(|_| {
                            warn!("Builder closing... with error");
                            ErrorKind::ThreadFailed(format!(
                                "{}",
                                std::thread::current().name().unwrap_or("unnamed")
                            ))
                        })??;
                    match defer {
                        Ok(_) => match_with_log!(Ok(()), info!("Builder closing... success")),
                        Err(e) => match_with_log!(Err(e), warn!("Builder closing... with error")),
                    }
                }
            }
        })?;

    // Reader
    let thReader: JoinHandle<Result<()>> = Thread::new()
        .name(format!("Reader"))
        .spawn(move || -> Result<()> {
            debug!("Reader initialized");
            let tx_builder = ReBu_tx;
            let opts = &opts;

            let result = || -> Result<()> {
                // Hot loop
                for item in from_source.iter().enumerate().map(|(i, r)| (i + 1, r)) {
                    let (data_tx, data_rx): (SyncSender<ToBuilder>, Receiver<ToBuilder>) =
                        syncQueue(10);
                    tx_builder.send(data_rx).map_err(|_| {
                        ErrorKind::UnexpectedChannelClose(format!(
                            "failed to send next |reader -> builder| channel, builder has hung up"
                        ))
                    })?;
                    match (item, opts.by_line()) {
                        ((i, read @ ReadKind::Stdin(_)), true) => {
                            let mut read_line = LineReader::with_delimiter_and_capacity(
                                opts.linereader_eol(),
                                opts.input_buffer_size(),
                                read.into_inner(),
                            );
                            let mut index = 1;
                            while let Some(slice) = read_line.next_line() {
                                if check_index(opts.regex(), index) {
                                    debug!("Processing line {} of input {}...", index, i);
                                    let reader = slice?.iter().map(|&b| Ok(b));
                                    unwind_json(&opts, index, reader, data_tx.clone())?;
                                    index += 1;
                                } else {
                                    debug!("Skipping line {} of input {}...", index, i);
                                    index += 1;
                                }
                            }
                        }
                        ((index, read), _) => {
                            if check_index(opts.regex(), index) {
                                debug!("Processing input {}...", index);
                                let reader = BufReader::with_capacity(
                                    opts.input_buffer_size(),
                                    read.into_inner(),
                                )
                                .bytes();
                                unwind_json(&opts, index, reader, data_tx)?;
                            } else {
                                debug!("Skipping input {}...", index);
                            }
                        }
                    }
                }

                Ok(())
            };
            // Cleanup
            match result() {
                defer => {
                    drop(tx_builder);
                    thBuilder
                        .join()
                        .map(|inner| {
                            inner.map_err(|err| {
                                match_with_log!(
                                    err,
                                    warn!("Reader closing... with error(s) propagated by Builder")
                                )
                            })
                        })
                        .map_err(|_| {
                            warn!("Reader closing... with error");
                            ErrorKind::ThreadFailed(format!(
                                "{}",
                                std::thread::current().name().unwrap_or("unnamed")
                            ))
                        })??;
                    match defer {
                        Ok(_) => match_with_log!(Ok(()), info!("Reader closing... success")),
                        Err(e) => match_with_log!(Err(e), warn!("Reader closing... with error")),
                    }
                }
            }
        })
        .map_err(|_| {
            ErrorKind::ThreadFailed(format!(
                "{}",
                std::thread::current().name().unwrap_or("unnamed")
            ))
        })?;

    Ok(thReader)
}
