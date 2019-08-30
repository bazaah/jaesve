#![allow(deprecated)]
use {
    crate::models::{
        assets::{Delimiter, Field, Guard, ReadFrom, RegexOptions},
        get_reader,
    },
    clap::{crate_authors, crate_version, App, Arg, ArgMatches as Matches, SubCommand},
    regex::Regex,
    simplelog::LevelFilter,
    std::collections::HashSet,
};

pub fn generate_cli<'a>() -> Matches<'a> {
    App::new("jaesve")
        .about("Utility for converting JSON into a CSV-like format")
        .author(crate_authors!("\n"))
        .version(crate_version!())
        .after_help("Use --help for more detailed messages")
        .arg(
            Arg::with_name("verbosity")
                .short("v")
                .multiple(true)
                .max_values(64)
                .takes_value(false)
                .help("Sets level of debug output"),
        )
        .arg(Arg::with_name("quiet")
                .short("q")
                .long("quiet")
                .takes_value(false)
                .help("Silences error messages")
        )
        .arg(Arg::with_name("line")
            .short("l")
            .long("line")
            .help("Set stdin to read a JSON string from each line")
        )
        .arg(Arg::with_name("append")
                .short("a")
                .long("append")
                .takes_value(false)
                .help("Append to output file, instead of overwriting")
                .long_help("Append to output file, instead of overwriting... has no effect if writing to stdout")
        )
        .arg(
            Arg::with_name("delimiter")
                .short("d")
                .long("delim")
                .takes_value(true)
                .value_name("STRING")
                .default_value(",")
                .help("Sets delimiter between output fields"),
        )
        .arg(
            Arg::with_name("guard")
            .short("g")
            .long("guard")
            .takes_value(true)
            .default_value("\"")
            .value_name("CHAR")
            .validator(|s| match s {
                    ref s if s.is_empty() => Ok(()),
                    ref s => match s.parse::<char>() {
                            Ok(_) => Ok(()),
                            Err(e) => Err(format!("Couldn't parse '{}' into a char: {}", s, e))
                        }
                    }
            )
            .help("Set field quote character")
        )
        .arg(Arg::with_name("regex")
            .short("E")
            .long("regex")
            .value_name("PATTERN")
            .takes_value(true)
            .validator(|regex| {
                match Regex::new(&regex) {
                    Ok(_) => Ok(()),
                    Err(e) => Err(format!("{}", e))
                }
            })
            .help("Set a regex to filter output")
        )
        .arg(Arg::with_name("regex_column")
            .short("c")
            .long("column")
            .takes_value(true)
            .value_name("STRING")
            .requires("regex")
            .default_value_if("regex", None, Field::Pointer.into())
            .possible_values(&[Field::Identifier.into(), Field::Pointer.into(), Field::Type.into(), Field::Value.into()])
            .help("Sets column to match regex on")
        )
        .arg(
            Arg::with_name("input")
                .value_name("FILE")
                .takes_value(true)
                .multiple(true)
                .allow_hyphen_values(true)
                .value_terminator(":")
                .help("Input file path(s) with a '-' representing stdin")
                .long_help("Input file path(s) with a '-' representing stdin, MUST be terminated by a ':' if it is not the final arg... i.e 'jaesve file1 file2 - file4 : --flag --option...")
        )
        .arg(
            Arg::with_name("output")
                .short("o")
                .long("output")
                .value_name("FILE")
                .takes_value(true)
                .help("Specify an output file path, defaults to stdout")
        )
        .arg(
            Arg::with_name("format")
                .short("f")
                .long("format")
                .value_name("STRING")
                .takes_value(true)
                // TODO: Figure out to pass &str made from Fields
                .default_value("ident.jptr.type.value")
                .validator(|fmt| {
                    let res: Result<Vec<_>, _> = fmt.split('.').map(|sub| Field::try_from_whitelist(
                            sub,
                            &[Field::Identifier, Field::Pointer, Field::Type, Field::Value]
                        )
                    ).collect();
                    match res {
                        Ok(_) => Ok(()),
                        Err(e) => Err(format!("{}", e))
                    }
                })
                .help("A dot '.' separated list of fields describing how output is formatted")
        )
        .subcommand(
            SubCommand::with_name("config")
                .name("config")
                .about("Configure various program intrinsics")
                .version("0.1.0")
                .after_help("These commands are unstable and may change in the future... use with care")
                .arg(
                    Arg::with_name("logger_output")
                        .long("log_to")
                        .value_name("FILE")
                        .takes_value(true)
                        .multiple(true)
                        .allow_hyphen_values(true)
                        .value_terminator(":")
                        .help("Log file path(s) with a '-' representing stderr")
                        .long_help("Log file path(s) with a '-' representing stderr, MUST be terminated by a ':' if it is not the final arg... i.e 'jaesve config logfile1 - logfile3 : --option...")
                )
                .arg(
                    Arg::with_name("max_open_file_handles")
                        .long("file_limit")
                        .value_name("UINT")
                        .takes_value(true)
                        .default_value("1")
                        .validator(|s| match s {
                            ref s => match s.parse::<usize>() {
                                Ok(_) => Ok(()),
                                Err(e) => Err(format!("Couldn't parse '{}' into a uint: {}", s, e))
                                }
                            }
                        )
                        .help("Maximum number of open file handles")
                )
                .arg(
                    Arg::with_name("output_buffer_size")
                    .long("buf_out")
                    .value_name("UINT")
                    .takes_value(true)
                    .default_value("16")
                    .validator(|s| match s {
                        ref s => match s.parse::<usize>() {
                            Ok(_) => Ok(()),
                            Err(e) => Err(format!("Couldn't parse '{}' into a uint: {}", s, e))
                            }
                        }
                    )
                    .help("Output buffer size multiplied by value of '--factor'")
                )
                .arg(
                    Arg::with_name("input_buffer_size")
                        .long("buf_in")
                        .value_name("UINT")
                        .takes_value(true)
                        .default_value("64")
                        .validator(|s| match s {
                            ref s => match s.parse::<usize>() {
                                Ok(_) => Ok(()),
                                Err(e) => Err(format!("Couldn't parse '{}' into a uint: {}", s, e))
                                }
                            }
                        )
                        .help("Input buffer size multiplied by value of '--factor'")
                )
                .arg(
                    Arg::with_name("byte_multiplier")
                        .long("factor")
                        .value_name("CHAR")
                        .takes_value(true)
                        .default_value("K")
                        .possible_values(&["B", "K", "M"])
                        .help("Multiplier used by other config args")
                        .long_help("Multiplier used by other config args: B: 1, K: 1024, M: 1024 * 1024")
                )
                .arg(
                    Arg::with_name("eol_char_linereader")
                        .long("linereader_eol")
                        .value_name("CHAR")
                        .takes_value(true)
                        .default_value("\n")
                        .hide_default_value(true)
                        .validator(|s| match s {
                            ref s => match s.parse::<u8>() {
                                Ok(_) => Ok(()),
                                Err(e) => Err(format!("Couldn't parse '{}' into a char: {}", s, e))
                                }
                            }
                        )
                        .help("Set stdin linereader's EOL character, ignored if '--lines' is not set")
                )
        )
        .get_matches()
}

pub struct ProgramArgs {
    delimiter: Delimiter,
    guard: Guard,
    debug_level: LevelFilter,
    by_line: bool,
    regex: Option<RegexOptions>,
    format: Vec<Field>,
    reader: Vec<Option<ReadFrom>>,
    writer: (Option<String>, bool),
    subcommand_config: SubConfig,
}

impl<'a> ProgramArgs {
    pub fn init(store: Matches<'a>) -> Self {
        let debug_level = match (store.occurrences_of("verbosity"), store.is_present("quiet")) {
            (_, true) => LevelFilter::Off,
            (0, false) => LevelFilter::Warn,
            (1, false) => LevelFilter::Info,
            (2, false) => LevelFilter::Debug,
            (_, false) => LevelFilter::Trace,
        };

        let reader = match store.values_of("input") {
            Some(inputs) => inputs
                .scan(false, |acc, item| match item {
                    "-" if *acc => Some((*acc, item)),
                    "-" => {
                        *acc = true;
                        Some((false, item))
                    }
                    _ => Some((false, item)),
                })
                .filter(|(dupe, _)| !dupe)
                .map(|(_, s)| get_reader(Some(s)))
                .collect::<Vec<Option<ReadFrom>>>(),
            None => {
                let mut vec: Vec<Option<ReadFrom>> = Vec::new();
                let i = get_reader(None);
                vec.push(i);
                vec
            }
        };

        let writer = match (store.value_of("output"), store.is_present("append")) {
            (Some(s), false) => (Some(s.to_string()), false),
            (Some(s), true) => (Some(s.to_string()), true),
            (None, _) => (None, false),
        };

        // Unwrap is safe because of default value set + validated by clap
        let format: Vec<Field> = store
            .value_of("format")
            .unwrap()
            .split('.')
            .map(Field::from)
            .collect();

        let regex: Option<RegexOptions> =
            match (store.value_of("regex"), store.value_of("regex_column")) {
                // Checks to make sure 'column' is being used in the output
                (Some(_), Some(column)) if Field::try_from_whitelist(column, &format).is_err() => {
                    None
                }
                (Some(pattern), Some(column)) => Some(RegexOptions::new(pattern, column.into())),
                (Some(_), None) => unreachable!("Default value should be supplied by clap"),
                (None, _) => None,
            };

        let by_line = store.is_present("line");

        let delimiter: Delimiter = match store.value_of("delimiter") {
            Some(s) => s.into(),
            _ => panic!("delimiter missing"),
        };

        let guard: Guard = match store.value_of("guard") {
            Some(s) if s.is_empty() => Guard::None,
            Some(c) => Guard::Some(c.parse::<char>().unwrap()),
            None => unreachable!("Default value should be supplied by clap"),
        };

        let subcommand_config = SubConfig::from_matches(store.subcommand_matches("config"));

        Self {
            delimiter,
            guard,
            debug_level,
            by_line,
            regex,
            format,
            reader,
            writer,
            subcommand_config,
        }
    }

    pub fn debug_level(&self) -> LevelFilter {
        self.debug_level
    }

    pub fn reader_list(&self) -> &Vec<Option<ReadFrom>> {
        &self.reader
    }

    pub fn writer(&self) -> &(Option<String>, bool) {
        &self.writer
    }

    pub fn by_line(&self) -> bool {
        self.by_line
    }

    pub fn delimiter(&self) -> Delimiter {
        self.delimiter.clone()
    }

    pub fn guard(&self) -> Guard {
        self.guard
    }

    pub fn format(&self) -> &[Field] {
        &self.format
    }

    pub fn regex(&self) -> Option<&RegexOptions> {
        self.regex.as_ref()
    }

    pub fn logger(&self) -> Option<&HashSet<String>> {
        self.subcommand_config.logger.as_ref()
    }

    pub fn input_file_handles_max(&self) -> usize {
        self.subcommand_config.max_handles
    }

    pub fn output_buffer_size(&self) -> usize {
        self.subcommand_config.output_buffer_size
    }

    pub fn input_buffer_size(&self) -> usize {
        self.subcommand_config.input_buffer_size
    }

    pub fn linereader_eol(&self) -> u8 {
        self.subcommand_config.linereader_eol
    }
}

struct SubConfig {
    logger: Option<HashSet<String>>,
    max_handles: usize,
    output_buffer_size: usize,
    input_buffer_size: usize,
    linereader_eol: u8,
}

impl SubConfig {
    pub fn from_matches<'a>(substore: Option<&Matches<'a>>) -> Self {
        match substore {
            Some(substore) => {
                let logger = substore.values_of("logger_output").map(|args| {
                    let mut set: HashSet<String> = HashSet::new();
                    // Dedups log write list, incase the user did something stupid
                    for path in args {
                        if !set.contains(path) && !path.is_empty() {
                            set.insert(path.to_string());
                        }
                    }
                    set
                });
                // Unwraps are safe because of default values set + validated by clap
                let multi: usize = match substore.value_of("byte_multiplier") {
                    Some("B") => 1,
                    Some("K") => 1024,
                    Some("M") => 1024 * 1024,
                    _ => unreachable!("Default value should be supplied by clap"),
                };
                let max_handles = substore
                    .value_of("max_open_file_handles")
                    .unwrap()
                    .parse::<usize>()
                    .unwrap();
                let output_buffer_size = substore
                    .value_of("output_buffer_size")
                    .unwrap()
                    .parse::<usize>()
                    .unwrap()
                    * multi;
                let input_buffer_size = substore
                    .value_of("input_buffer_size")
                    .unwrap()
                    .parse::<usize>()
                    .unwrap()
                    * multi;
                let linereader_eol = substore
                    .value_of("eol_char_linereader")
                    .unwrap()
                    .parse::<u8>()
                    .unwrap();

                SubConfig {
                    logger,
                    max_handles,
                    output_buffer_size,
                    input_buffer_size,
                    linereader_eol,
                }
            }
            None => Self::default(),
        }
    }
}

impl Default for SubConfig {
    fn default() -> Self {
        SubConfig {
            // None = Default logging, not no logging
            logger: None,
            max_handles: 1,
            output_buffer_size: 16 * 1024,
            input_buffer_size: 64 * 1024,
            linereader_eol: b'\n',
        }
    }
}
