use {
    crate::{
        match_with_log,
        models::{
            assets::{ReadFrom, RegexOn, RegexOptions},
            get_reader,
        },
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
        .arg(Arg::with_name("append")
                .short("a")
                .long("append")
                .takes_value(false)
                .help("Append to output file, instead of overwriting")
                .long_help("Append to output file, instead of overwriting... has no effect if writing to stdout")
        )
        .arg(
            Arg::with_name("separator")
                .short("s")
                .long("delimiter")
                .takes_value(true)
                .value_name("DELIMITER")
                .default_value("c")
                .help("c => ',' cs => ', ' t => tab, _ => _"),
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
            .possible_values(&["key", "type", "value", "sep"])
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
        .get_matches();

    matches
}

pub struct ProgramArgs {
    show_type: bool,
    separator: String,
    debug_level: LevelFilter,
    by_line: bool,
    regex: Option<RegexOptions>,
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
                    if store.is_present("type") && RegexOn::from(column) == RegexOn::Type =>
                {
                    match_with_log!(
                        None,
                        warn!("Attempting to regex nonexistent field: 'type', ignoring...")
                    )
                }
                (Some(pattern), Some(column)) => Some(RegexOptions::new(pattern, column.into())),
                (Some(pattern), None) => Some(RegexOptions::new(pattern, RegexOn::default())),
                (None, _) => None,
            };

        let by_line = store.is_present("line");

        let show_type = !store.is_present("type");

        let separator = String::from(match store.value_of("separator") {
            Some("c") => ",",
            Some("t") => "\t",
            Some("cs") => ", ",
            Some(s) => s,
            _ => panic!("Separator missing"),
        });

        Self {
            show_type,
            separator,
            debug_level,
            by_line,
            regex,
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
}
