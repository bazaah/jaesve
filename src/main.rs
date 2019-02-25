use crate::models::{formated_error, get_reader, get_writer, to_csv, Options};
use clap::{crate_authors, crate_version, App, Arg};
use serde_json::json;
use std::io::{self, BufWriter, Write};

mod models;

fn main() {
    let matches = App::new("jaesve")
        .about("Utility for converting JSON into a CSV-like format")
        .author(crate_authors!("\n"))
        .version(crate_version!())
        .arg(Arg::with_name("verbosity")
            .short("v")
            .multiple(true)
            .max_values(3)
            .takes_value(false)
            .help("Sets level of debug output")
        )
        .arg(
            Arg::with_name("separator")
                .short("s")
                .takes_value(true)
                .default_value("c")
                .possible_values(&["c", "t"])
                .help("c => comma, t => tab"),
        )
        .arg(Arg::with_name("type")
            .short("t")
            .long("type")
            .help("Print without json object type")
        )
        .arg(
            Arg::with_name("input")
                .short("i")
                .long("input")
                .value_name("FILE")
                .takes_value(true)
                .multiple(true)
                .help("Input an arbitrary number of file path(s), with a '-' representing stdin")
                .long_help("Input an arbitrary number of file path(s), separated by a space \ni.e: -i path1 - ~/path3")
        )
        .arg(
            Arg::with_name("output")
                .short("o")
                .long("output")
                .value_name("FILE")
                .takes_value(true)
                .help("Specify an output file path, defaults to stdout")
        )
        .get_matches();

    let show_type = !matches.is_present("type");
    let separator = match matches.value_of("separator") {
        Some("c") => ",",
        Some("t") => "\t",
        _ => panic!("Separator missing"),
    };
    let debug_level = match matches.occurrences_of("verbosity") {
        0 => 0,
        1 => 1,
        2 => 2,
        3 => 3,
        _ => 3,
    };

    // Place CLI options into a central location
    let options = Options::new(show_type, separator.to_owned(), debug_level);

    // Set up the writer: either to stdout or a file
    let mut writer = BufWriter::new(get_writer(matches.value_of("output"), &options));

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
            let input = io::stdin();
            let status = match to_csv(&options, input, writer.by_ref()) {
                Ok(res) => res,
                Err(e) => json!({ "Error(s) encountered": formated_error(&e) }),
            };
            if *options.get_debug_level() >= 2 {
                eprintln!("\n--- Finished stdin with status: {} ---\n==>", &status)
            }
        }
    }
}
