#![allow(clippy::useless_format, clippy::match_bool)]
#[macro_use]
extern crate log;
#[macro_use]
extern crate lazy_static;

use {
    crate::{
        cli::{generate_cli, ProgramArgs},
        models::{
            assets::ReadKind,
            error::Result,
            error::{Context, ErrContext, ErrorKind, ProgramExit},
            initialize_logging, set_reader,
        },
        threads::spawn_workers,
    },
    std::sync::mpsc::{sync_channel as syncQueue, Receiver, SyncSender},
};

mod cli;
mod models;
mod threads;

// Global immutable object with values seeded from the CLI inputs
lazy_static! {
    static ref CLI: ProgramArgs = ProgramArgs::init(generate_cli());
}

fn main() {
    // Start Pre-program code, do not place anything above these lines
    initialize_logging(&CLI);
    info!("CLI options loaded and logger started");
    // End of Pre-program block

    // Annoying inability to return arbitrary exit codes on stable requires this
    ProgramExit::from(try_main()).exit()
}

fn try_main() -> Result<()> {
    // Channel for sending open input streams (stdin/file handles)
    // number controls how many shall be open at any given time,
    // counting from 0 (i.e: 0 -> 1, 1 -> 2, etc)
    let (tx, rx): (SyncSender<ReadKind>, Receiver<ReadKind>) =
        syncQueue(CLI.input_file_handles_max());

    // Instantiates worker threads
    let reader = spawn_workers(&CLI, rx)?;

    // Hot loop
    for source in CLI.reader_list() {
        let read_from: ReadKind = set_reader(source);
        tx.send(read_from).context(Context::umcc("Reader"))?;
    }

    // Signals that that no new input sources will be sent
    drop(tx);

    // Waits for remaining threads to complete
    reader
        .join()
        .map_err(|_| ErrorKind::ThreadFailed)
        .context(Context::tp("Reader"))??;
    // Return 0
    Ok(())
}
