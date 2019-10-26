use {
    super::config::env::EnvArgs,
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
    let mut env_config = EnvArgs::from_env();
    let file_config = merge_config_files(extra);

    env_config.merge(file_config);

    env_config
}

#[cfg(not(feature = "config-file"))]
pub(in crate::cli) fn finalize_args<P: AsRef<Path>>(_extra: &[P]) -> EnvArgs {
    EnvArgs::from_env()
}

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
}
