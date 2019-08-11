#![feature(termination_trait_lib, try_trait)]
#[macro_use]
extern crate log;
#[macro_use]
extern crate lazy_static;

use {
    crate::{
        cli::{generate_cli, ProgramArgs},
        models::error::{ErrorKind, ProgramExit},
    },
    simplelog::*,
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

    // Return 0
    ProgramExit::Success
}

/*
// Processes any files in the order they were inputted to the CLI, skipping on a failed open
// If a "-" is set as an input option will read from stdin
// If input is omitted completely will read from stdin
match matches.values_of("input") {
    Some(files) => {
        let mut file_list: Vec<_> = files.collect();
        file_list.dedup_by_key(|f| *f == "-");
        for file in file_list {
            let input = get_reader(Some(file));
            if input.is_ok() {
                let status = match to_csv(&options, input.unwrap(), writer.by_ref()) {
                    Ok(res) => res,
                    Err(e) => json!({ "Error(s) encountered": formated_error(&e) }),
                };
                if *options.get_debug_level() >= 2 {
                    eprintln!(
                        "\n--- Finished input: {}, with status: {} ---\n==>",
                        file, status
                    );
                }
            } else {
                if *options.get_debug_level() >= 1 {
                    eprintln!(
                        "\n--- Error: {} could not be opened, skipping... ---\n",
                        file
                    )
                }
                continue;
            }
        }
    }
    None => {
        let input = ReadFrom::Stdin(io::stdin());
        let status = match to_csv(&options, input, writer.by_ref()) {
            Ok(res) => res,
            Err(e) => json!({ "Error(s) encountered": formated_error(&e) }),
        };
        if *options.get_debug_level() >= 2 {
            eprintln!("\n--- Finished stdin with status: {} ---\n==>", &status)
        }
    }
}
*/
