use {
    super::config::env::{Env, EnvArgs},
    crate::models::{
        assets::{ReadFrom, RegexOptions},
        block::{Delimiter, Guard},
        field::Field,
        get_reader,
    },
    clap::ArgMatches,
    simplelog::LevelFilter,
    std::path::Path,
};

pub(in crate::cli) use merge::ConfigMerge;

#[cfg(feature = "config-file")]
use self::file::merge_config_files;

#[cfg(feature = "config-file")]
mod file;

mod env;
mod merge;

#[cfg(feature = "config-file")]
pub(in crate::cli) fn finalize_args<P: AsRef<Path>>(extra: &[P]) -> EnvArgs {
    let mut env_config = Env::default().collect();
    let file_config = merge_config_files(extra);

    env_config.merge(file_config);

    env_config
}

#[cfg(not(feature = "config-file"))]
pub(in crate::cli) fn finalize_args<P: AsRef<Path>>(_extra: &[P]) -> EnvArgs {
    Env::default().collect()
}

#[derive(Debug)]
pub(in crate::cli) struct ProtoArgs<T> {
    config: T,
}

impl<T> ProtoArgs<T>
where
    T: ConfigMerge,
{
    pub(in crate::cli) fn new(config: T) -> Self {
        Self { config }
    }

    pub(in crate::cli) fn debug_level(&mut self, store: &ArgMatches<'_>) -> LevelFilter {
        if store.is_present("quiet") || self.config.quiet().unwrap_or(false) {
            LevelFilter::Off
        } else {
            let high = store.occurrences_of("verbosity");
            let low = self.config.debug_level();
            let level = if high > 0 {
                high as usize
            } else {
                low.unwrap_or(0)
            };

            match level {
                0 => LevelFilter::Warn,
                1 => LevelFilter::Info,
                2 => LevelFilter::Debug,
                _ => LevelFilter::Trace,
            }
        }
    }

    pub(in crate::cli) fn reader(&mut self, store: &ArgMatches<'_>) -> Vec<Option<ReadFrom>> {
        match store.values_of("input") {
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
        }
    }

    pub(in crate::cli) fn writer(&mut self, store: &ArgMatches<'_>) -> (Option<String>, bool) {
        let append = store.is_present("append") || self.config.append().unwrap_or(false);

        match (store.value_of("output"), append) {
            (Some(s), false) => (Some(s.to_string()), false),
            (Some(s), true) => (Some(s.to_string()), true),
            (None, _) => (None, false),
        }
    }

    pub(in crate::cli) fn format(&mut self, store: &ArgMatches<'_>) -> Vec<Field> {
        match (store.occurrences_of("format"), store.value_of("format")) {
            (0, Some(fmt)) => {
                let format = self.config.format();

                || -> Option<Vec<Field>> { format?.ok() }()
                    .unwrap_or_else(|| fmt.split('.').map(Field::from).collect())
            }
            (_, Some(fmt)) => fmt.split('.').map(Field::from).collect(),
            (_, _) => unreachable!(),
        }
    }

    pub(in crate::cli) fn regex(&mut self, store: &ArgMatches<'_>) -> Option<RegexOptions> {
        match (store.value_of("regex"), store.value_of("regex_column")) {
            (Some(pattern), Some(column)) => Some(RegexOptions::new(pattern, column.into())),
            (Some(_), None) => unreachable!("Column default value should be supplied by clap"),
            (None, _) => None,
        }
    }

    pub(in crate::cli) fn by_line(&mut self, store: &ArgMatches<'_>) -> (bool, usize) {
        match (
            store.occurrences_of("line"),
            store.value_of("line"),
            self.config.line(),
        ) {
            (0, _, Some(num)) => (true, num),
            (0, Some(num), None) => (false, num.parse::<usize>().unwrap()),
            (_, Some(num), _) => (true, num.parse::<usize>().unwrap()),
            (_, _, _) => unreachable!("Start from line default should be set by clap"),
        }
    }

    pub(in crate::cli) fn delimiter(&mut self, store: &ArgMatches<'_>) -> Delimiter {
        match (
            store.occurrences_of("delimiter"),
            store.value_of("delimiter"),
            self.config.delimiter(),
        ) {
            (0, _, Some(delim)) => delim,
            (0, Some(s), None) => s.into(),
            (_, Some(s), _) => s.into(),
            (_, _, _) => unreachable!("Default should be set by clap"),
        }
    }

    pub(in crate::cli) fn guard(&mut self, store: &ArgMatches<'_>) -> Guard {
        match (
            store.occurrences_of("guard"),
            store.value_of("guard"),
            self.config.guard(),
        ) {
            (0, _, Some(guard)) => guard,
            (0, Some(s), None) => Guard::Some(s.parse::<char>().unwrap()),
            (_, Some(s), _) if s.is_empty() => Guard::None,
            (_, Some(s), _) => Guard::Some(s.parse::<char>().unwrap()),
            (_, _, _) => unreachable!("Default should be set by clap"),
        }
    }

    pub(in crate::cli) fn output_buffer_size(&mut self, substore: &ArgMatches<'_>) -> usize {
        match (
            substore.occurrences_of("output_buffer_size"),
            substore.value_of("output_buffer_size"),
            self.config.output_buffer_size(),
        ) {
            (0, _, Some(u)) => u,
            (0, Some(s), None) => s.parse::<usize>().unwrap(),
            (_, Some(s), _) => s.parse::<usize>().unwrap(),
            (_, _, _) => unreachable!("Default should be set by clap"),
        }
    }

    pub(in crate::cli) fn input_buffer_size(&mut self, substore: &ArgMatches<'_>) -> usize {
        match (
            substore.occurrences_of("input_buffer_size"),
            substore.value_of("input_buffer_size"),
            self.config.input_buffer_size(),
        ) {
            (0, _, Some(u)) => u,
            (0, Some(s), None) => s.parse::<usize>().unwrap(),
            (_, Some(s), _) => s.parse::<usize>().unwrap(),
            (_, _, _) => unreachable!("Default should be set by clap"),
        }
    }

    pub(in crate::cli) fn linereader_eol(&mut self, substore: &ArgMatches<'_>) -> u8 {
        match (
            substore.occurrences_of("eol_char_linereader"),
            substore.value_of("eol_char_linereader"),
            self.config.linereader_eol(),
        ) {
            (0, _, Some(c)) => c as u8,
            (0, Some(s), None) => s.parse::<char>().unwrap() as u8,
            (_, Some(s), _) => s.parse::<char>().unwrap() as u8,
            (_, _, _) => unreachable!("Default should be set by clap"),
        }
    }

    pub(in crate::cli) fn factor(&mut self, substore: &ArgMatches<'_>) -> usize {
        let parse = |s: &str| match s {
            "B" => 1,
            "K" => 1024,
            "M" => 1024 * 1024,
            _ => unreachable!("Clap validates this is one of: 'B','K','M'"),
        };

        match (
            substore.occurrences_of("byte_multiplier"),
            substore.value_of("byte_multiplier"),
            self.config.factor(),
        ) {
            (0, _, Some(fac)) => fac,
            (0, Some(s), None) => parse(s),
            (_, Some(s), _) => parse(s),
            (_, _, _) => unreachable!("Default should be set by clap"),
        }
    }
}

mod args {
    use crate::models::{
        block::{Delimiter, Guard},
        error::Result,
        field::Field,
    };
    pub(super) type OptDelim = Option<Delimiter>;
    pub(super) type OptGuard = Option<Guard>;
    pub(super) type OptDebug = Option<usize>;
    pub(super) type OptLine = Option<usize>;
    pub(super) type OptFormat = Option<Result<Vec<Field>>>;
    pub(super) type OptBufIn = Option<usize>;
    pub(super) type OptBufOut = Option<usize>;
    pub(super) type OptEOL = Option<char>;
    pub(super) type OptQuiet = Option<bool>;
    pub(super) type OptAppend = Option<bool>;
    pub(super) type OptFactor = Option<usize>;
}

#[cfg(test)]
mod tests {
    use {
        super::{
            env::{Env, Kind, Mock},
            *,
        },
        crate::cli::generate_cli,
        clap::AppSettings,
        simplelog::LevelFilter,
        std::{error, result},
    };

    #[cfg(feature = "config-file")]
    use super::file::{deserialize_str, FileArgs};

    type Result<T> = result::Result<T, Box<dyn error::Error>>;

    macro_rules! mock {
        (env $( $ek:expr, $ev:expr ),* ;file $fkv:expr) => {{
            #[allow(unused_mut)]
            let mut env = Env::with_environment([ $( ($ek, $ev) )* ].into_iter().cloned().collect::<Mock>()).collect();

            #[cfg(feature = "config-file")]
            {
                let file = deserialize_str::<FileArgs>($fkv).unwrap();
                env.merge(file);
            }

            ProtoArgs::new(env)
        }};
        (env $( $ek:expr, $ev:expr ),* ;file $fkv:expr => $table:expr) => {{
            #[allow(unused_mut)]
            let mut env = Env::with_environment([ $( ($ek, $ev) )* ].into_iter().cloned().collect::<Mock>()).collect();

            #[cfg(feature = "config-file")]
            {
                let file = deserialize_str::<FileArgs>(concat!("[", $table, "]\n", $fkv)).unwrap();
                env.merge(file);
            }

            ProtoArgs::new(env)
        }};
        (file $fkv:expr) => {{
            #[allow(unused_mut)]
            let mut env = Env::with_environment(Mock::default()).collect();

            #[cfg(feature = "config-file")]
            {
                let file = deserialize_str::<FileArgs>($fkv).unwrap();
                env.merge(file);
            }

            ProtoArgs::new(env)
        }};
        (file $fkv:expr => $table:expr) => {{
            #[allow(unused_mut)]
            let mut env = Env::with_environment(Mock::default()).collect();

            #[cfg(feature = "config-file")]
            {
                let file = deserialize_str::<FileArgs>(concat!("[", $table, "]\n", $fkv)).unwrap();
                env.merge(file);
            }

            ProtoArgs::new(env)
        }};
    }

    macro_rules! cli {
        () => {
            generate_cli().setting(AppSettings::NoBinaryName).get_matches_from_safe::<&[&str], &&str>(&[])
        };
        ( $( $cli:expr ),* ) => {
            generate_cli().setting(AppSettings::NoBinaryName).get_matches_from_safe(&[$( $cli ),*])
        };
    }

    #[test]
    fn merge_debug_level_cli() -> Result<()> {
        let cli = cli!("-v")?;
        let mut proto = mock!(env Kind::Debug, "2" ;file "debug = 3");

        assert_eq!(proto.debug_level(&cli), LevelFilter::Info);
        Ok(())
    }

    #[test]
    fn merge_debug_level_env() -> Result<()> {
        let cli = cli!()?;
        let mut proto = mock!(env Kind::Debug, "2" ;file "debug = 3");

        assert_eq!(proto.debug_level(&cli), LevelFilter::Debug);
        Ok(())
    }

    #[test]
    #[cfg(feature = "config-file")]
    fn merge_debug_level_file() -> Result<()> {
        let cli = cli!()?;
        let mut proto = mock!(file "debug = 3");

        assert_eq!(proto.debug_level(&cli), LevelFilter::Trace);
        Ok(())
    }

    #[test]
    fn merge_quiet_cli() -> Result<()> {
        let cli = cli!("--quiet")?;
        let mut proto = mock!(env Kind::Quiet, "false" ;file "quiet = 'no'");

        assert_eq!(proto.debug_level(&cli), LevelFilter::Off);
        Ok(())
    }

    #[test]
    fn merge_quiet_env() -> Result<()> {
        let cli = cli!()?;
        let mut proto = mock!(env Kind::Quiet, "yes" ;file "quiet = false");

        assert_eq!(proto.debug_level(&cli), LevelFilter::Off);
        Ok(())
    }

    #[test]
    #[cfg(feature = "config-file")]
    fn merge_quiet_file() -> Result<()> {
        let cli = cli!()?;
        let mut proto = mock!(file "quiet = 'Yes'");

        assert_eq!(proto.debug_level(&cli), LevelFilter::Off);
        Ok(())
    }

    #[test]
    fn merge_append_cli() -> Result<()> {
        let cli = cli!("--append", "--output", ".dummy")?;
        let mut proto = mock!(env Kind::Append, "false" ;file "append = 'NO'");

        assert_eq!(proto.writer(&cli), (Some(format!(".dummy")), true));
        Ok(())
    }

    #[test]
    fn merge_append_env() -> Result<()> {
        let cli = cli!("--output", ".dummy")?;
        let mut proto = mock!(env Kind::Append, "Yes" ;file "append = 'False'");

        assert_eq!(proto.writer(&cli), (Some(format!(".dummy")), true));
        Ok(())
    }

    #[test]
    #[cfg(feature = "config-file")]
    fn merge_append_file() -> Result<()> {
        let cli = cli!("--output", ".dummy")?;
        let mut proto = mock!(file "append = '1'");

        assert_eq!(proto.writer(&cli), (Some(format!(".dummy")), true));
        Ok(())
    }

    #[test]
    fn merge_format_cli() -> Result<()> {
        let cli = cli!("--format", "ident")?;
        let mut proto = mock!(env Kind::Format, "jptr" ;file "format = 'type'");

        assert_eq!(proto.format(&cli), vec![Field::Identifier]);
        Ok(())
    }

    #[test]
    fn merge_format_env() -> Result<()> {
        let cli = cli!()?;
        let mut proto = mock!(env Kind::Format, "jptr" ;file "format = 'type'");

        assert_eq!(proto.format(&cli), vec![Field::Pointer]);
        Ok(())
    }

    #[test]
    #[cfg(feature = "config-file")]
    fn merge_format_file() -> Result<()> {
        let cli = cli!()?;
        let mut proto = mock!(file "format = 'type'");

        assert_eq!(proto.format(&cli), vec![Field::Type]);
        Ok(())
    }

    #[test]
    fn merge_line_cli() -> Result<()> {
        let cli = cli!("--line", "10")?;
        let mut proto = mock!(env Kind::Line, "20" ;file "line = 30");

        assert_eq!(proto.by_line(&cli), (true, 10));
        Ok(())
    }

    #[test]
    fn merge_line_env() -> Result<()> {
        let cli = cli!()?;
        let mut proto = mock!(env Kind::Line, "20" ;file "line = 30");

        assert_eq!(proto.by_line(&cli), (true, 20));
        Ok(())
    }

    #[test]
    #[cfg(feature = "config-file")]
    fn merge_line_file() -> Result<()> {
        let cli = cli!()?;
        let mut proto = mock!(file "line = 30");

        assert_eq!(proto.by_line(&cli), (true, 30));
        Ok(())
    }

    #[test]
    fn merge_delimiter_cli() -> Result<()> {
        let cli = cli!("--delim", "?")?;
        let mut proto = mock!(env Kind::Delim, "*" ;file "delimiter = '+'");

        assert_eq!(
            format!("{:?}", proto.delimiter(&cli)),
            format!("{:?}", Delimiter::from("?"))
        );
        Ok(())
    }

    #[test]
    fn merge_delimiter_env() -> Result<()> {
        let cli = cli!()?;
        let mut proto = mock!(env Kind::Delim, "*" ;file "delimiter = '+'");

        assert_eq!(
            format!("{:?}", proto.delimiter(&cli)),
            format!("{:?}", Delimiter::from("*"))
        );
        Ok(())
    }

    #[test]
    #[cfg(feature = "config-file")]
    fn merge_delimiter_file() -> Result<()> {
        let cli = cli!()?;
        let mut proto = mock!(file "delimiter = '+'");

        assert_eq!(
            format!("{:?}", proto.delimiter(&cli)),
            format!("{:?}", Delimiter::from("+"))
        );
        Ok(())
    }

    #[test]
    fn merge_guard_cli() -> Result<()> {
        let cli = cli!("--guard", "")?;
        let mut proto = mock!(env Kind::Guard, "." ;file "guard = '!'");

        assert_eq!(
            format!("{:?}", proto.guard(&cli)),
            format!("{:?}", Guard::from(""))
        );
        Ok(())
    }

    #[test]
    fn merge_guard_env() -> Result<()> {
        let cli = cli!()?;
        let mut proto = mock!(env Kind::Guard, "." ;file "guard = '!'");

        assert_eq!(
            format!("{:?}", proto.guard(&cli)),
            format!("{:?}", Guard::from("."))
        );
        Ok(())
    }

    #[test]
    #[cfg(feature = "config-file")]
    fn merge_guard_file() -> Result<()> {
        let cli = cli!()?;
        let mut proto = mock!(file "guard = '!'");

        assert_eq!(
            format!("{:?}", proto.guard(&cli)),
            format!("{:?}", Guard::from("!"))
        );
        Ok(())
    }

    #[test]
    fn merge_output_buffer_size_cli() -> Result<()> {
        let cli = cli!("config", "--buf_out", "128")?;
        let mut proto = mock!(env Kind::BufOut, "64" ;file "buf-out = 32");

        assert_eq!(
            proto.output_buffer_size(&cli.subcommand_matches("config").unwrap()),
            128
        );
        Ok(())
    }

    #[test]
    fn merge_output_buffer_size_env() -> Result<()> {
        let cli = cli!("config")?;
        let mut proto = mock!(env Kind::BufOut, "64" ;file "buf-out = 32");

        assert_eq!(
            proto.output_buffer_size(&cli.subcommand_matches("config").unwrap()),
            64
        );
        Ok(())
    }

    #[test]
    #[cfg(feature = "config-file")]
    fn merge_output_buffer_size_file() -> Result<()> {
        let cli = cli!("config")?;
        let mut proto = mock!(file "buf-out = 32" => "config");

        assert_eq!(
            proto.output_buffer_size(&cli.subcommand_matches("config").unwrap()),
            32
        );
        Ok(())
    }

    #[test]
    fn merge_input_buffer_size_cli() -> Result<()> {
        let cli = cli!("config", "--buf_in", "128")?;
        let mut proto = mock!(env Kind::BufIn, "64" ;file "buf_in = 32");

        assert_eq!(
            proto.input_buffer_size(&cli.subcommand_matches("config").unwrap()),
            128
        );
        Ok(())
    }

    #[test]
    fn merge_input_buffer_size_env() -> Result<()> {
        let cli = cli!("config")?;
        let mut proto = mock!(env Kind::BufIn, "64" ;file "buf_in = 32");

        assert_eq!(
            proto.input_buffer_size(&cli.subcommand_matches("config").unwrap()),
            64
        );
        Ok(())
    }

    #[test]
    #[cfg(feature = "config-file")]
    fn merge_input_buffer_size_file() -> Result<()> {
        let cli = cli!("config")?;
        let mut proto = mock!(file "buf_in = 32" => "config");

        assert_eq!(
            proto.input_buffer_size(&cli.subcommand_matches("config").unwrap()),
            32
        );
        Ok(())
    }

    #[test]
    fn merge_linereader_eol_cli() -> Result<()> {
        let cli = cli!("config", "--linereader_eol", ":")?;
        let mut proto = mock!(env Kind::RdrEol, "=" ;file "linereader_eol = '-'");

        assert_eq!(
            proto.linereader_eol(&cli.subcommand_matches("config").unwrap()),
            b':'
        );
        Ok(())
    }

    #[test]
    fn merge_linereader_eol_env() -> Result<()> {
        let cli = cli!("config")?;
        let mut proto = mock!(env Kind::RdrEol, "=" ;file "linereader_eol = '-'");

        assert_eq!(
            proto.linereader_eol(&cli.subcommand_matches("config").unwrap()),
            b'='
        );
        Ok(())
    }

    #[test]
    #[cfg(feature = "config-file")]
    fn merge_linereader_eol_file() -> Result<()> {
        let cli = cli!("config")?;
        let mut proto = mock!(file "linereader_eol = '-'" => "config");

        assert_eq!(
            proto.linereader_eol(&cli.subcommand_matches("config").unwrap()),
            b'-'
        );
        Ok(())
    }

    #[test]
    fn merge_factor_cli() -> Result<()> {
        let cli = cli!("config", "--factor", "M")?;
        let mut proto = mock!(env Kind::Factor, "K" ;file "factor = 'B'");

        assert_eq!(
            proto.factor(&cli.subcommand_matches("config").unwrap()),
            1024 * 1024
        );
        Ok(())
    }

    #[test]
    fn merge_factor_env() -> Result<()> {
        let cli = cli!("config")?;
        let mut proto = mock!(env Kind::Factor, "K" ;file "factor = 'B'");

        assert_eq!(
            proto.factor(&cli.subcommand_matches("config").unwrap()),
            1024
        );
        Ok(())
    }

    #[test]
    #[cfg(feature = "config-file")]
    fn merge_factor_file() -> Result<()> {
        let cli = cli!("config")?;
        let mut proto = mock!(file "factor = 'B'" => "config");

        assert_eq!(proto.factor(&cli.subcommand_matches("config").unwrap()), 1);
        Ok(())
    }
}
