use {
    crate::{match_with_log, models::assets::ReadFrom},
    std::{
        fs::{File, OpenOptions},
        io::{stdin as cin, stdout as cout, BufRead, Read as ioRead, Write as ioWrite},
        path::PathBuf,
    },
};

pub mod assets;

// Determines write destination from runtime args
// w: (_, bool), true => append, false => create
pub fn get_writer(w: &(Option<String>, bool)) -> Box<dyn ioWrite> {
    match w {
        (Some(file_name), false) => match_with_log!(
            match File::create(file_name).ok() {
                Some(file) => match_with_log!(Box::new(file), info!("Success!")),
                None => match_with_log!(Box::new(cout()), warn!("Failed! Switching to stdout...")),
            },
            info!("Attempting to create {}...", file_name)
        ),
        (Some(file_name), true) => match_with_log!(
            match OpenOptions::new().append(true).open(file_name) {
                Ok(file) => match_with_log!(Box::new(file), info!("Success!")),
                Err(e) => match_with_log!(
                    Box::new(cout()),
                    warn!("Unable to open file: {}, switching to stdout...", e)
                ),
            },
            info!("Attempting to append to {}...", file_name)
        ),
        (None, _) => match_with_log!(
            Box::new(cout()),
            info!("No file detected, defaulting to stdout...")
        ),
    }
}

// Helper function for generating a list of read sources at runtime
pub fn get_reader(r: Option<&str>) -> Option<ReadFrom> {
    match r {
        Some("-") => Some(ReadFrom::Stdin),
        Some(file_name) => {
            let path = PathBuf::from(file_name);
            if path.is_file() {
                Some(ReadFrom::File(path))
            } else {
                None
            }
        }
        None => Some(ReadFrom::Stdin),
    }
}

// Opens a read source, defaults to stdin if source errors
pub fn set_reader(src: &Option<ReadFrom>) -> Box<dyn ioRead + Send> {
    match src {
        Some(s) => match s {
            ReadFrom::File(path) => match_with_log!(
                match File::open(path) {
                    Ok(f) => match_with_log!(Box::new(f), info!("Success!")),
                    Err(e) => match_with_log!(
                        Box::new(cin()),
                        warn!("Failed! {}, switching to stdin...", e)
                    ),
                },
                info!("Attempting to read from {:?}...", path)
            ),
            ReadFrom::Stdin => match_with_log!(Box::new(cin()), info!("Reading CSV from stdin...")),
        },
        None => match_with_log!(
            Box::new(cin()),
            info!("No input source found, defaulting to stdin...")
        ),
    }
}

/*
// Puts all the pieces together
pub fn to_csv<W: Write>(
    options: &Options,
    input: ReadFrom,
    mut output: W,
) -> FailureResult<JsonValue> {
    match input {
        ReadFrom::File(f) => {
            let data: JsonValue = serde_json::from_reader(f)?;
            let packet = JsonPacket::new(data);
            packet.print(options, &mut output);
        }
        ReadFrom::Stdin(s) => {
            if *options.single_line_object() {
                s.lock()
                    .lines()
                    .filter_map(|r| r.ok())
                    .filter_map(|line| {
                        let data = serde_json::from_str(line.as_str());
                        data.ok()
                    })
                    .for_each(|value: JsonValue| {
                        let packet = JsonPacket::new(value);
                        packet.print(options, &mut output);
                    })
            } else {
                let data: JsonValue = serde_json::from_reader(s)?;
                let packet = JsonPacket::new(data);
                packet.print(options, &mut output);
            }
        }
    }

    Ok(json!(0))
}

// Function that writes the formatted output to the writer
// The work-horse of the rebel fleet
// If something goes wrong, writes the error to stderr and moves on
fn write<W: Write>(options: &Options, mut output: W, entry: &str, val: Option<&JsonValue>) {
    let regex_opts = options.get_regex_opts();
    let separator = options.get_separator();
    let show_type = options.type_status();
    let value = match val {
        Some(jObject(_)) => "".to_string(),
        Some(jArray(_)) => "".to_string(),
        Some(jString(s)) => s.to_string(),
        Some(jNumber(n)) => n.to_string(),
        Some(jBool(b)) => b.to_string(),
        Some(jNull) => "NULL".to_string(),
        None => "NO_VALUE".to_string(),
    };
    let mut formated_output = String::new();

    if *show_type {
        let type_of = match val {
            Some(val) => match val {
                jObject(_) => "Map",
                jArray(_) => "Array",
                jString(_) => "String",
                jNumber(_) => "Number",
                jBool(_) => "Bool",
                jNull => "Null",
            },
            None => "NO_TYPE",
        };
        let fmt = format!(
            "\"{}\"{}\"{}\"{}\"{}\"",
            entry, separator, type_of, separator, value
        );
        formated_output.push_str(&fmt);
    } else {
        let fmt = format!("\"{}\"{}\"{}\"", entry, separator, value);
        formated_output.push_str(&fmt);
    }
    match regex_opts.get_regex() {
        Some(r) => {
            let column = match regex_opts.get_column() {
                Some(RegexOn::Entry) => entry,
                Some(RegexOn::Value) => value.as_str(),
                Some(RegexOn::Type) => {
                    match val {
                        Some(val) => match val {
                            jObject(_) => "Map",
                            jArray(_) => "Array",
                            jString(_) => "String",
                            jNumber(_) => "Number",
                            jBool(_) => "Bool",
                            jNull => "Null",
                        },
                        None => "NO_TYPE",
                    }
                }
                Some(RegexOn::Separator) => separator,
                None => panic!("Error: Need a column to regex match on"),
            };

            if r.is_match(column) {
                writeln!(output.by_ref(), "{}", formated_output.as_str())
                    .map_err(|e| eprintln!("An error occurred while writing: {}", e))
                    .unwrap_or(())
            }
        }
        None => writeln!(output.by_ref(), "{}", formated_output.as_str())
            .map_err(|e| eprintln!("An error occurred while writing: {}", e))
            .unwrap_or(()),
    }
}
*/
