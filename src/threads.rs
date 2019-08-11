#![allow(non_snake_case)]
use {
    crate::{
        cli::ProgramArgs,
        match_with_log,
        models::{
            assets::{JsonPacket, JsonPointer, JsonScan},
            error::{ErrorKind, Result},
            get_writer, unwind_json,
        },
    },
    serde::{ser::SerializeSeq, Serializer},
    serde_json::{from_slice, value::Value as JsonValue},
    std::{
        convert::TryFrom,
        io::{BufReader, BufWriter, Read as ioRead, Write as ioWrite},
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
) -> Result<JoinHandle<Result<()>>> {
    type Parts = (Option<Vec<u8>>, Vec<u8>);
    type Output = String;

    // Meta channel: |Reader -> Builder|, delivers new receivers to builder
    let (ReBu_tx, ReBu_rx): (SyncSender<Receiver<Parts>>, Receiver<Receiver<Parts>>) = syncQueue(0);
    // Meta channel: |Builder -> Writer|, delivers new receivers to writer
    let (BuWr_tx, BuWr_rx): (SyncSender<Receiver<Output>>, Receiver<Receiver<Output>>) =
        syncQueue(0);

    // Writer
    let thWriter = thBuilder::new()
        .name(format!("Writer"))
        .spawn(move || -> Result<()> {
            debug!("Writer initialized");
            let rx_builder = BuWr_rx;
            let opts = &opts;
            let mut writer = BufWriter::new(get_writer(opts.writer()));
            info!("Buffered writer initialized");

            // Hot loop
            while let Some(channel) = rx_builder.iter().next() {
                for output in channel {
                    writeln!(&mut writer, "{}, {}", output.0, output.1)?;
                }
            }

            // Cleanup
            debug!("Writer closing");
            Ok(())
        })?;

    // Builder
    let thBuilder = thBuilder::new()
        .name(format!("Builder"))
        .spawn(move || -> Result<()> {
            debug!("Builder initialized");
            let tx_writer = BuWr_tx;
            let rx_reader = ReBu_rx;
            let opts = &opts;

            // Hot loop
            while let Some(channel) = rx_reader.iter().next() {
                let (data_tx, data_rx): (SyncSender<Output>, Receiver<Output>) = syncQueue(10);
                tx_writer.send(data_rx).map_err(|_| {
                    ErrorKind::UnexpectedChannelClose(format!(
                        "failed to send next |builder -> writer| channel, writer has hung up"
                    ))
                })?;

                for packet in channel {
                    let builder = JsonPointer::new(JsonPacket::try_from(packet)?);

                    for item in builder {
                    data_tx.send(item).map_err(|_| {
                        ErrorKind::UnexpectedChannelClose(format!(
                            "writer in |builder -> writer| channel has hung up"
                        ))
                    })?;
                    }
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
    let thReader: JoinHandle<Result<()>> = thBuilder::new()
        .name(format!("Reader"))
        .spawn(move || -> Result<()> {
            debug!("Reader initialized");
            let tx_builder = ReBu_tx;
            let opts = &opts;

            // Hot loop
            while let Some(src) = from_source.iter().next() {
                let (data_tx, data_rx): (SyncSender<Parts>, Receiver<Parts>) = syncQueue(10);
                tx_builder.send(data_rx).map_err(|_| {
                    ErrorKind::UnexpectedChannelClose(format!(
                        "failed to send next |reader -> builder| channel, builder has hung up"
                    ))
                })?;
                let mut scanner = JsonScan::new(BufReader::new(src).bytes());

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
