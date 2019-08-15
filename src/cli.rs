use {
    crate::{
        match_with_log,
        models::{
            assets::{Delimiter, Field, ReadFrom, RegexOptions},
            get_reader,
        },
    },
    clap::{crate_authors, crate_version, App, Arg, ArgMatches as Matches},
    regex::Regex,
    simplelog::LevelFilter,
    std::convert::TryFrom,
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
        .arg(Arg::with_name("append")
                .short("a")
                .long("append")
                .takes_value(false)
                .help("Append to output file, instead of overwriting")
                .long_help("Append to output file, instead of overwriting... has no effect if writing to stdout")
        )
        .arg(
            Arg::with_name("delimiter")
                .short("s")
                .long("delim")
                .takes_value(true)
                .value_name("DELIMITER")
                .default_value(",")
                .help("Sets delimiter between output fields"),
        )
        .arg(Arg::with_name("type")
            .short("t")
            .long("type")
            .help("Print without json object type")
        )
        .arg(Arg::with_name("line")
            .short("l")
            .long("line")
            .help("Set stdin to read a JSON string from each line")
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
            .value_name("TYPE")
            .requires("regex")
            .possible_values(&["jptr", "type", "value", "delim"])
            .help("Sets column to match regex on")
        )
        .arg(
            Arg::with_name("input")
                .short("i")
                .long("input")
                .value_name("FILE")
                .takes_value(true)
                .multiple(true)
                .require_delimiter(true)
                .help("Input file path(s) separated by commas, with a '-' representing stdin"),
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
                .default_value("ident.jptr.type.value")
                .validator(|fmt| {
                    let res: Result<Vec<_>, _> = fmt.split(".").map(|sub| Field::try_from(sub)).collect();
                    match res {
                        Ok(_) => Ok(()),
                        Err(e) => Err(format!("{}", e))
                    }
                })
                .help("A dot ('.') separated list of identifiers describing how output is formatted")
        )
        .get_matches();

    matches
}

pub struct ProgramArgs {
    show_type: bool,
    delimiter: Delimiter,
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
            Some(inputs) => {
                let mut list: Vec<_> = inputs.collect();
                list.dedup_by_key(|f| *f == "-");
                list.iter()
                    .map(|s| get_reader(Some(s)))
                    .collect::<Vec<Option<ReadFrom>>>()
            }
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

        let regex: Option<RegexOptions> =
            match (store.value_of("regex"), store.value_of("regex_column")) {
                (Some(_), Some(column))
                    if store.is_present("type") && Field::from(column) == Field::Type =>
                {
                    match_with_log!(
                        None,
                        warn!("Attempting to regex nonexistent field: 'type', ignoring...")
                    )
                }
                (Some(pattern), Some(column)) => Some(RegexOptions::new(pattern, column.into())),
                (Some(pattern), None) => Some(RegexOptions::new(pattern, Field::default())),
                (None, _) => None,
            };

        // Unwrap is safe because of default value set in clap
        let format: Vec<Field> = store
            .value_of("format")
            .unwrap()
            .split(".")
            .map(|sub| Field::from(sub))
            .collect();

        let by_line = store.is_present("line");

        let show_type = !store.is_present("type");

        let delimiter: Delimiter = match store.value_of("delimiter") {
            Some(s) => s.into(),
            _ => panic!("delimiter missing"),
        };

        Self {
            show_type,
            delimiter,
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

    pub fn delimiter(&self) -> Delimiter {
        self.delimiter.clone()
    }

    pub fn format(&self) -> &[Field] {
        &self.format
    }
}
