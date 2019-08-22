#![feature(termination_trait_lib, try_trait, bind_by_move_pattern_guards)]
#[macro_use]
extern crate log;
#[macro_use]
extern crate lazy_static;

use {
    crate::{
        cli::{generate_cli, ProgramArgs},
        models::{
            assets::ReadKind,
            error::{ErrorKind, ProgramExit},
            set_reader,
        },
        threads::spawn_workers,
    },
    simplelog::*,
    std::sync::mpsc::{sync_channel as syncQueue, Receiver, SyncSender},
};

mod cli;
mod models;
mod threads;

// Global immutable object with values seeded from the CLI inputs
lazy_static! {
    static ref CLI: ProgramArgs = ProgramArgs::init(generate_cli());
}

fn main() -> ProgramExit<ErrorKind> {
    // Start Pre-program code, do not place anything above these lines
    TermLogger::init(CLI.debug_level(), Config::default(), TerminalMode::Stderr).unwrap();
    info!("CLI options loaded and logger started");
    // End of Pre-program block

    // Channel for sending open input streams (stdin/file handles)
    // number controls how many shall be open at any given time,
    // counting from 0 (i.e: 0 -> 1, 1 -> 2, etc)
    let (tx, rx): (SyncSender<ReadKind>, Receiver<ReadKind>) = syncQueue(1);

    // Instantiates worker threads
    let reader = spawn_workers(&CLI, rx)?;

    // Hot loop
    for source in CLI.reader_list() {
        let read_from: ReadKind = set_reader(source);
        tx.send(read_from).map_err(|_| {
            ErrorKind::UnexpectedChannelClose(format!(
                "reader in |main -> reader| channel has hung up"
            ))
        })?;
    }

    // Signals that that no new input sources will be sent
    drop(tx);

    // Waits for remaining threads to complete
    reader.join().map_err(|_| {
        ErrorKind::ThreadFailed(format!(
            "{}",
            std::thread::current().name().unwrap_or("unnamed")
        ))
        // join() returns a Result<Result<(), Errorkind>, ErrorKind>, hence the double question mark
    })??;
    // Return 0
    ProgramExit::Success
}
