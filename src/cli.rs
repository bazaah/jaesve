#![allow(deprecated)]
use {
    self::{
        config::{finalize_args, ConfigMerge, ProtoArgs},
        deptree::DependencyTree,
    },
    crate::models::{
        assets::{ReadFrom, RegexOptions},
        block::{Delimiter, Guard},
        error::Result,
        field::Field,
    },
    clap::{crate_authors, crate_version, App, Arg, ArgMatches as Matches, SubCommand},
    regex::Regex,
    simplelog::LevelFilter,
    std::collections::{HashMap, HashSet},
};

mod config;
mod deptree;

// Subset of all Field variants that can be used for valuable output
const VALID_FIELDS: [Field; 5] = [
    Field::Identifier,
    Field::Pointer,
    Field::Type,
    Field::Value,
    Field::JmesPath,
];

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
        .arg(Arg::with_name("append")
                .short("a")
                .long("append")
                .takes_value(false)
                .help("Append to output file, instead of overwriting")
                .long_help("Append to output file, instead of overwriting... has no effect if writing to stdout")
        )
        .arg(Arg::with_name("line")
            .short("l")
            .long("line")
            .takes_value(true)
            .default_value("0")
            .hide_default_value(true)
            .value_name("UINT")
            .validator(|s| match s {
                ref s => match s.parse::<usize>() {
                    Ok(_) => Ok(()),
                    Err(e) => Err(format!("Couldn't parse '{}' into a uint: {}", s, e))
                    }
                }
            )
            .help("Set stdin to read a JSON doc from each line")
            .long_help("Set stdin to read a JSON doc from each line, and begin processing at line <UNIT>")
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
                    let res: Result<Vec<_>> = fmt.split('.').map(|sub| Field::try_from_whitelist(sub, &VALID_FIELDS).map(|_| ())
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
        .subcommand(
            SubCommand::with_name("completions")
                .about("Autocompletion script generator")
                .version("0.1.0")
                .after_help("Note that running this subcommand overrides normal program behavior: all other args, flags, options and subcommands will be ignored. It generates the autocompletion script and exits, nothing more")
                .arg(
                    Arg::with_name("completions_file")
                    .requires("shell")
                    .value_name("FILE")
                    .takes_value(true)
                    .help("Output file for completions, defaulting to stdout")
                    .long_help("Output file for completions, note that this command will overwrite, not append")
                )
                .arg(
                    Arg::with_name("shell")
                        .required(true)
                        .possible_values(&["bash", "zsh", "fish"])
                        .last(true)
                        .value_name("SHELL")
                        .help("Shell to generate completion script for")
                )
        )
}

#[derive(Debug)]
pub struct ProgramArgs {
    delimiter: Delimiter,
    guard: Guard,
    debug_level: LevelFilter,
    by_line: (bool, usize),
    regex: Option<RegexOptions>,
    format: Vec<Field>,
    reader: Vec<Option<ReadFrom>>,
    writer: (Option<String>, bool),
    dependency_map: HashMap<Field, bool>,
    subcommand_config: SubConfig,
}

impl<'a, 'b> ProgramArgs {
    pub fn init(cli: App<'a, 'b>) -> Self {
        let store = &cli.get_matches();

        // Early exit if completions are called
        ProgramArgs::if_completions_exit(store.subcommand_matches("completions"));

        let mut proto = ProtoArgs::new(finalize_args::<&str>(&[]));

        let debug_level = proto.debug_level(store);

        let reader = proto.reader(store);

        let writer = proto.writer(store);

        let format = proto.format(store);

        let regex = proto.regex(store);

        let by_line = proto.by_line(store);

        let delimiter = proto.delimiter(store);

        let guard: Guard = proto.guard(store);

        let dependency_map = DependencyTree::init().generate_list(match format.len() {
            0 => unreachable!("Clap should validate output fields >= 1"),
            1 => format
                .iter()
                .chain(
                    [
                        Some(Field::Guard),
                        regex.as_ref().map(|regex| regex.on_field()),
                    ]
                    .iter()
                    .filter_map(|i| i.as_ref()),
                )
                .copied()
                .collect::<Vec<Field>>(),
            _ => format
                .iter()
                .chain(
                    [
                        Some(Field::Guard),
                        Some(Field::Delimiter),
                        regex.as_ref().map(|regex| regex.on_field()),
                    ]
                    .iter()
                    .filter_map(|i| i.as_ref()),
                )
                .copied()
                .collect::<Vec<Field>>(),
        });

        let subcommand_config =
            SubConfig::from_matches(store.subcommand_matches("config"), &mut proto);

        Self {
            delimiter,
            guard,
            debug_level,
            by_line,
            regex,
            format,
            reader,
            writer,
            dependency_map,
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
        self.by_line.0
    }

    pub fn line_start_number(&self) -> usize {
        self.by_line.1
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

    pub fn should_calculate<F: AsRef<Field>>(&self, field: F) -> bool {
        self.dependency_map.contains_key(field.as_ref())
    }

    pub fn should_store<F: AsRef<Field>>(&self, field: F) -> bool {
        self.dependency_map
            .get(field.as_ref())
            .map_or(false, |b| *b)
    }

    /// Checks if the 'completion' subcommand is active
    /// if it is, generate completion script and exit
    fn if_completions_exit(subcommand: Option<&Matches<'a>>) {
        use std::{fs::File, io::stdout as cout, io::Write as ioWrite};
        let writer = |path: Option<&str>| -> (Box<dyn ioWrite>, i32) {
            match path {
                Some(path) => match File::create(path) {
                    Ok(file) => (Box::from(file), 0),
                    // Return exit status 1 to indicate an error
                    Err(_) => (Box::from(cout()), 1),
                },
                None => (Box::from(cout()), 0),
            }
        };

        if let Some(sub_store) = subcommand {
            sub_store
                .value_of("completions_file")
                .and_then(|path| {
                    let mut writer = writer(Some(path));
                    generate_cli().gen_completions_to(
                        "jaesve",
                        sub_store.value_of("shell").unwrap().parse().unwrap(),
                        &mut writer.0,
                    );
                    Some(writer.1)
                })
                .or_else(|| {
                    let mut writer = writer(None);
                    generate_cli().gen_completions_to(
                        "jaesve",
                        sub_store.value_of("shell").unwrap().parse().unwrap(),
                        &mut writer.0,
                    );
                    Some(writer.1)
                })
                .map(|code| std::process::exit(code))
                .unwrap()
        }
    }

    /* <=== SubCommands ===> */
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

#[derive(Debug)]
struct SubConfig {
    logger: Option<HashSet<String>>,
    max_handles: usize,
    output_buffer_size: usize,
    input_buffer_size: usize,
    linereader_eol: u8,
}

impl SubConfig {
    pub fn from_matches<'a, T>(substore: Option<&Matches<'a>>, proto: &mut ProtoArgs<T>) -> Self
    where
        T: ConfigMerge,
    {
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
                let factor = proto.factor(substore);
                let max_handles = substore
                    .value_of("max_open_file_handles")
                    .unwrap()
                    .parse::<usize>()
                    .unwrap();
                let output_buffer_size = proto.output_buffer_size(substore) * factor;
                let input_buffer_size = proto.input_buffer_size(substore) * factor;
                let linereader_eol = proto.linereader_eol(substore);

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
