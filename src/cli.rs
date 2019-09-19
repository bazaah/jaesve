#![allow(deprecated)]
use {
    crate::models::{
        assets::{Delimiter, Field, Guard, ReadFrom, RegexOptions},
        get_reader,
    },
    clap::{crate_authors, crate_version, App, Arg, ArgMatches as Matches, SubCommand},
    regex::Regex,
    simplelog::LevelFilter,
    std::collections::{HashMap, HashSet},
};

// Subset of all Field variants that can be used for valuable output
const VALID_FIELDS: [Field; 5] = [
    Field::Identifier,
    Field::Pointer,
    Field::Type,
    Field::Value,
    Field::JmesPath,
];

const FIELDS: [Field; 7] = [
    Field::Delimiter,
    Field::Guard,
    Field::Identifier,
    Field::JmesPath,
    Field::Pointer,
    Field::Type,
    Field::Value,
];

struct DependencyTree {
    /// Field and its dependencies, if any
    map: HashMap<Field, Option<Vec<Field>>>,
}

impl DependencyTree {
    /// Initialize the base relations between Fields
    fn init() -> Self {
        let mut map = HashMap::with_capacity(FIELDS.len());
        FIELDS
            .iter()
            .map(|field| match field {
                f @ Field::Delimiter => (f, None),
                f @ Field::Guard => (f, None),
                f @ Field::Identifier => (f, Some(vec![Field::Guard, Field::Delimiter])),
                f @ Field::JmesPath => (f, Some(vec![Field::Pointer])),
                f @ Field::Pointer => (f, Some(vec![Field::Value])),
                f @ Field::Type => (f, Some(vec![Field::Value])),
                f @ Field::Value => (f, Some(vec![Field::Guard, Field::Delimiter])),
            })
            .for_each(|(field, dependencies)| {
                map.insert(*field, dependencies);
            });
        DependencyTree { map }
    }

    /// Generate a dependency map, used for determining what work this instance of the program needs to do
    fn generate_list<F: AsRef<[Field]>>(&self, relevant: F) -> HashMap<Field, bool> {
        let mut set = HashMap::<Field, bool>::with_capacity(FIELDS.len());
        // Populate the dependency map with with all secondary Fields
        relevant
            .as_ref()
            .iter()
            .scan(Vec::<(Field, bool)>::new(), |buffer, field| {
                match self.map.get(field).unwrap() {
                    Some(deps) => buffer.extend(deps.iter().map(|f| (*f, false))),
                    None => {}
                }

                buffer.pop()
            })
            .for_each(|(field, is_output)| {
                set.insert(field, is_output);
            });
        // Populate the dependency map with the primary Fields, overwriting dependencies
        relevant
            .as_ref()
            .iter()
            .map(|field| (*field, true))
            .for_each(|(field, is_output)| {
                set.insert(field, is_output);
            });

        set
    }
}

pub fn generate_cli<'a, 'b>() -> App<'a, 'b> {
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
            .help("Set stdin to read a JSON doc from each line")
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
            .possible_values(VALID_FIELDS.iter().map(|f| <Field as Into<&str>>::into(*f)).collect::<Vec<&str>>().as_slice())
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
                            &VALID_FIELDS
                        )
                    ).collect();
                    match res {
                        Ok(_) => Ok(()),
                        Err(e) => Err(format!("{}", e))
                    }
                })
                .help("A dot '.' separated list of fields describing how output is formatted [possible values: ident, jptr, type, value, jmes]")
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
                            ref s => match s.parse::<char>() {
                                Ok(_) => Ok(()),
                                Err(e) => Err(format!("Couldn't parse '{}' into a char: {}", s, e))
                                }
                            }
                        )
                        .help("Set stdin linereader's EOL character, ignored if '--lines' is not set")
                )
        )
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

impl<'a, 'b> ProgramArgs {
    pub fn init(cli: App<'a, 'b>) -> Self {
        let store = cli.get_matches();
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
                (Some(pattern), Some(column)) => Some(RegexOptions::new(pattern, column.into())),
                (Some(_), None) => unreachable!("Column default value should be supplied by clap"),
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

    pub fn relevant_fields(&self) -> HashSet<Field> {
        self.format()
            .iter()
            .map(|f| -> Option<Field> { Some(*f) })
            .chain(
                [self.regex().map(|regex| regex.on_field())]
                    .iter()
                    .map(|f| *f),
            )
            .filter_map(|o: Option<Field>| o.map(|f| f))
            .collect()
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
                    .parse::<char>()
                    .unwrap() as u8;

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

#[cfg(test)]
mod tests {
    use {
        super::*,
        clap::{AppSettings, ErrorKind as ClapError},
        itertools::Itertools,
    };
    macro_rules! test_cli {
        () => {
            generate_cli().setting(AppSettings::NoBinaryName)
        };
    }

    #[test]
    fn validate_opt_guard_success() {
        let app = test_cli!().get_matches_from_safe(&["--guard", "'"]);

        assert!(app.is_ok())
    }

    #[test]
    fn validate_opt_guard_failure() {
        let app = test_cli!().get_matches_from_safe(&["--guard", "not a char"]);

        assert!(app.is_err());
        assert_eq!(app.unwrap_err().kind, ClapError::ValueValidation)
    }

    #[test]
    fn validate_opt_regex_success() {
        let app = test_cli!().get_matches_from_safe(&["--regex", "j[a-zA-Z]+e"]);

        assert!(app.is_ok())
    }

    #[test]
    fn validate_opt_regex_failure() {
        let app = test_cli!().get_matches_from_safe(&["--regex", "j[a-zA-Z+e"]);

        assert!(app.is_err());
        assert_eq!(app.unwrap_err().kind, ClapError::ValueValidation)
    }

    #[test]
    fn possible_opt_column() {
        for field in &VALID_FIELDS {
            let col: &str = field.clone().into();
            let app = test_cli!().get_matches_from_safe(&["--column", col, "-E", "j[a-zA-Z]+e"]);
            assert!(app.is_ok());
        }
    }

    #[test]
    fn requires_opt_column() {
        let app = test_cli!().get_matches_from_safe(&["--column", "value"]);

        assert!(app.is_err());
        assert_eq!(app.unwrap_err().kind, ClapError::MissingRequiredArgument)
    }

    #[test]
    fn syntax_arg_input() {
        let app = test_cli!().get_matches_from_safe(&["path1", "path2", "-", "path4"]);
        assert!(app.is_ok());

        let app =
            test_cli!().get_matches_from_safe(&["path1", "path2", "-", "path4", ":", "--quiet"]);

        assert!(app.is_ok());
        assert_eq!(app.unwrap().is_present("quiet"), true);

        // Test for changes in the edge behavior of clap
        let app = test_cli!().get_matches_from_safe(&["path1", "path2", "-", "path4", "config"]);

        assert!(app.is_ok());
        assert_eq!(app.unwrap().is_present("config"), true);
    }

    #[test]
    fn validate_opt_format_combinations() {
        VALID_FIELDS
            .iter()
            .combinations(VALID_FIELDS.len())
            .map(|outer| {
                let fmt = outer
                    .iter()
                    .map(|f| {
                        let whatever = *f.clone();
                        let col: &str = whatever.into();
                        col
                    })
                    .collect::<Vec<&str>>();
                fmt.join(".")
            })
            .for_each(|fmt| {
                let app = test_cli!().get_matches_from_safe(&["--format", &fmt]);

                assert!(app.is_ok());
            })
    }

    #[test]
    fn syntax_opt_format() {
        let fmt: &str = VALID_FIELDS[0].clone().into();
        let app = test_cli!().get_matches_from_safe(&["--format", fmt]);

        assert!(app.is_ok());

        let trailing_dot = format!("{}.", fmt);
        let app = test_cli!().get_matches_from_safe(&["--format", &trailing_dot]);

        assert!(app.is_err());
        assert_eq!(app.unwrap_err().kind, ClapError::ValueValidation);

        let leading_dot = format!(".{}", fmt);
        let app = test_cli!().get_matches_from_safe(&["--format", &leading_dot]);

        assert!(app.is_err());
        assert_eq!(app.unwrap_err().kind, ClapError::ValueValidation);
    }

    #[test]
    fn syntax_subcommand_opt_log_to() {
        let app =
            test_cli!().get_matches_from_safe(&["config", "--log_to", "log1", "log2", "-", "log4"]);
        assert!(app.is_ok());

        let app = test_cli!().get_matches_from_safe(&[
            "config",
            "--log_to",
            "log1",
            ":",
            "--file_limit",
            "5",
        ]);

        assert!(app.is_ok());
        assert_eq!(
            app.unwrap()
                .subcommand_matches("config")
                .unwrap()
                .value_of("max_open_file_handles"),
            Some("5")
        );

        // Test for changes in the edge behavior of clap
        let app =
            test_cli!().get_matches_from_safe(&["config", "--log_to", "log1", "--file_limit", "5"]);

        assert!(app.is_ok());
        // note the ne here... '--file_limit' is treated as a 'log_to' value
        assert_ne!(
            app.unwrap()
                .subcommand_matches("config")
                .unwrap()
                .value_of("max_open_file_handles"),
            Some("5")
        );
    }

    #[test]
    fn validate_subcommand_opt_file_limit_success() {
        let app = test_cli!().get_matches_from_safe(&["config", "--file_limit", "5"]);

        assert!(app.is_ok())
    }

    #[test]
    fn validate_subcommand_opt_file_limit_failure() {
        let app = test_cli!().get_matches_from_safe(&["config", "--file_limit", "not a usize"]);

        assert!(app.is_err());
        assert_eq!(app.unwrap_err().kind, ClapError::ValueValidation)
    }

    #[test]
    fn validate_subcommand_opt_buf_out_success() {
        let app = test_cli!().get_matches_from_safe(&["config", "--buf_out", "5"]);

        assert!(app.is_ok())
    }

    #[test]
    fn validate_subcommand_opt_buf_out_failure() {
        let app = test_cli!().get_matches_from_safe(&["config", "--buf_out", "not a usize"]);

        assert!(app.is_err());
        assert_eq!(app.unwrap_err().kind, ClapError::ValueValidation)
    }

    #[test]
    fn validate_subcommand_opt_buf_in_success() {
        let app = test_cli!().get_matches_from_safe(&["config", "--buf_in", "5"]);

        assert!(app.is_ok())
    }

    #[test]
    fn validate_subcommand_opt_buf_in_failure() {
        let app = test_cli!().get_matches_from_safe(&["config", "--buf_in", "not a usize"]);

        assert!(app.is_err());
        assert_eq!(app.unwrap_err().kind, ClapError::ValueValidation)
    }

    #[test]
    fn validate_subcommand_opt_linereader_eol_success() {
        let app = test_cli!().get_matches_from_safe(&["config", "--linereader_eol", ":"]);

        assert!(app.is_ok())
    }

    #[test]
    fn validate_subcommand_opt_linereader_eol_failure() {
        let app = test_cli!().get_matches_from_safe(&["config", "--linereader_eol", "not a char"]);

        assert!(app.is_err());
        assert_eq!(app.unwrap_err().kind, ClapError::ValueValidation)
    }

    #[test]
    fn possible_subcommand_opt_factor() {
        for id in &["B", "K", "M"] {
            let mult: &str = id.clone().into();
            let app = test_cli!().get_matches_from_safe(&["config", "--factor", mult]);
            assert!(app.is_ok());
        }
    }
}
