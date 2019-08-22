#![allow(deprecated)]
use {
    crate::models::{
        assets::{Delimiter, Field, ReadFrom, RegexOptions},
        get_reader,
    },
    clap::{crate_authors, crate_version, App, Arg, ArgMatches as Matches},
    regex::Regex,
    simplelog::LevelFilter,
};

pub fn generate_cli<'a>() -> Matches<'a> {
    let matches = App::new("jaesve")
        .about("Utility for converting JSON into a CSV-like format")
        .author(crate_authors!("\n"))
        .version(crate_version!())
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
            .validator(|c| match c.parse::<char>() {
                    Ok(_) => Ok(()),
                    Err(e) => Err(format!("Couldn't parse '{}' into a char: {}", c, e))
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
                .help("Input file path(s) with a '-' representing stdin, MUST be terminated by a ':'")
                .long_help("Input file path(s) with a '-' representing stdin, MUST be terminated by a ':'... i.e 'file1 file2 - file3 :")
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
                    let res: Result<Vec<_>, _> = fmt.split(".").map(|sub| Field::try_from_whitelist(
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
        .get_matches();

    matches
}

pub struct ProgramArgs {
    delimiter: Delimiter,
    guard: Delimiter,
    debug_level: LevelFilter,
    by_line: bool,
    regex: Option<RegexOptions>,
    format: Vec<Field>,
    reader: Vec<Option<ReadFrom>>,
    writer: (Option<String>, bool),
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
            .split(".")
            .map(|sub| Field::from(sub))
            .collect();

        let regex: Option<RegexOptions> =
            match (store.value_of("regex"), store.value_of("regex_column")) {
                // Checks to make sure 'column' is being used in the output
                (Some(_), Some(column)) if Field::try_from_whitelist(column, &format).is_err() => {
                    None
                }
                (Some(pattern), Some(column)) => Some(RegexOptions::new(pattern, column.into())),
                (Some(_), None) => unreachable!("Default value supplied by clap"),
                (None, _) => None,
            };

        let by_line = store.is_present("line");

        let delimiter: Delimiter = match store.value_of("delimiter") {
            Some(s) => s.into(),
            _ => panic!("delimiter missing"),
        };

        let guard: Delimiter = match store.value_of("guard") {
            Some(c) => c.into(),
            _ => unreachable!("guard missing"),
        };

        Self {
            delimiter,
            guard,
            debug_level,
            by_line,
            regex,
            format,
            reader,
            writer,
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

    pub fn guard(&self) -> Delimiter {
        self.guard.clone()
    }

    pub fn format(&self) -> &[Field] {
        &self.format
    }

    pub fn regex(&self) -> Option<&RegexOptions> {
        self.regex.as_ref()
    }
}
