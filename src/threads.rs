#![allow(non_snake_case)]
use {
    crate::{
        cli::ProgramArgs,
        match_with_log,
        models::{
            error::ErrorKind,
            unwind_json,
            get_writer,
            assets::JsonScan,
        },
    },
    serde::{ser::SerializeSeq, Serializer},
    std::{
        io::{BufWriter, Read as ioRead, BufReader},
        sync::mpsc::{sync_channel as syncQueue, Receiver, SyncSender},
        thread::{Builder as thBuilder, JoinHandle},
    },
};

// Spawns workers and the channels which communicate input segments.
// Each input source "from_source" is assigned a new channel, and said channel's
// rx sent through the "meta channels." This implementation ensures that the control
// flow mirrors the data flow
pub(crate) fn spawn_workers(
    opts: &'static ProgramArgs,
    from_source: Receiver<Box<dyn ioRead + Send>>,
) -> Result<JoinHandle<Result<(), ErrorKind>>, ErrorKind> {
    type Parts = (Option<Vec<u8>>, Vec<u8>);
    type Output = (usize, usize);

    // Meta channel: |Reader -> Builder|, delivers new receivers to builder
    let (ReBu_tx, ReBu_rx): (
        SyncSender<Receiver<Parts>>,
        Receiver<Receiver<Parts>>,
    ) = syncQueue(0);
    // Meta channel: |Builder -> Writer|, delivers new receivers to writer
    let (BuWr_tx, BuWr_rx): (SyncSender<Receiver<Output>>, Receiver<Receiver<Output>>) =
        syncQueue(0);

    // Writer
    let thWriter =
        thBuilder::new()
            .name(format!("Writer"))
            .spawn(move || -> Result<(), ErrorKind> {
                debug!("Writer initialized");
                let rx_builder = BuWr_rx;
                let opts = &opts;
                let mut writer = BufWriter::new(get_writer(opts.writer()));
                info!("Buffered writer initialized");

                // Hot loop
                while let Some(channel) = rx_builder.iter().next() {
                    let _res: Result<(), ErrorKind> = match opts.output_type() {
                        OutputFormat::Json => match_with_log!(
                            {
                                let mut ser = serde_json::Serializer::new(&mut writer);
                                let mut seq =
                                    ser.serialize_seq(None).map_err(|e| ErrorKind::from(e))?;
                                for output in channel.iter() {
                                    seq.serialize_element(&output)
                                        .map_err(|e| ErrorKind::from(e))?;
                                }
                                seq.end().map_err(|e| ErrorKind::from(e))?;
                                Ok(())
                            },
                            info!("Using Json writer")
                        ),
                        OutputFormat::JsonPretty => match_with_log!(
                            {
                                let mut ser = serde_json::Serializer::pretty(&mut writer);
                                let mut seq =
                                    ser.serialize_seq(None).map_err(|e| ErrorKind::from(e))?;
                                for output in channel.iter() {
                                    seq.serialize_element(&output)
                                        .map_err(|e| ErrorKind::from(e))?;
                                }
                                seq.end().map_err(|e| ErrorKind::from(e))?;
                                Ok(())
                            },
                            info!("Using pretty Json writer")
                        ),
                        OutputFormat::Yaml => match_with_log!(
                            {
                                let all_output: Vec<Output> = channel.iter().collect();
                                serde_yaml::to_writer(&mut writer, &all_output)
                                    .map_err(|e| ErrorKind::from(e))?;

                                Ok(())
                            },
                            info!("Using Yaml writer")
                        ),
                    };
                }

                // Cleanup
                debug!("Writer closing");
                Ok(())
            })?;

    // Builder
    let thBuilder =
        thBuilder::new()
            .name(format!("Builder"))
            .spawn(move || -> Result<(), ErrorKind> {
                debug!("Builder initialized");
                let tx_writer = BuWr_tx;
                let rx_reader = ReBu_rx;
                let opts = &opts;

                // Hot loop
                while let Some(object) = rx_reader.iter().next() {
                    let (data_tx, data_rx): (SyncSender<Output>, Receiver<Output>) = syncQueue(10);
                    tx_writer.send(data_rx).map_err(|_| {
                        ErrorKind::UnexpectedChannelClose(format!(
                            "failed to send next |builder -> writer| channel, writer has hung up"
                        ))
                    })?;
                    let res = unimplemented!();
                    
                    for item in res {
                        data_tx.send(item).map_err(|_| {
                            ErrorKind::UnexpectedChannelClose(format!(
                                "writer in |builder -> writer| channel has hung up"
                            ))
                        })?;
                    }
                }

                // Cleanup
                drop(tx_writer);
                thWriter.join().map_err(|_| {
                    ErrorKind::ThreadFailed(format!(
                        "{}",
                        std::thread::current().name().unwrap_or("unnamed")
                    ))
                })??;
                debug!("Builder closing");
                Ok(())
            });

    // Reader
    let thReader: JoinHandle<Result<(), ErrorKind>> = thBuilder::new()
        .name(format!("Reader"))
        .spawn(move || -> Result<(), ErrorKind> {
            debug!("Reader initialized");
            let tx_builder = ReBu_tx;
            let opts = &opts;

            // Hot loop
            while let Some(src) = from_source.iter().next() {
                let (data_tx, data_rx): (
                    SyncSender<Parts>,
                    Receiver<Parts>,
                ) = syncQueue(10);
                tx_builder.send(data_rx).map_err(|_| {
                    ErrorKind::UnexpectedChannelClose(format!(
                        "failed to send next |reader -> builder| channel, builder has hung up"
                    ))
                })?;
                let scanner = JsonScan::new(BufReader::new(src).bytes());

                unwind_json(&opts, &mut scanner, data_tx, None, None)?;
            }

            // Cleanup
            drop(tx_builder);
            thBuilder?.join().map_err(|_| {
                ErrorKind::ThreadFailed(format!(
                    "{}",
                    std::thread::current().name().unwrap_or("unnamed")
                ))
            })??;
            debug!("Reader closing");
            Ok(())
        })
        .map_err(|_| {
            ErrorKind::ThreadFailed(format!(
                "{}",
                std::thread::current().name().unwrap_or("unnamed")
            ))
        })?;

    Ok(thReader)
}
