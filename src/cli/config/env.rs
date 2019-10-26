use {
    super::{args::*, merge::ConfigMerge},
    crate::{
        cli::VALID_FIELDS,
        models::{
            block::{Delimiter, Guard},
            error::Result,
            field::Field,
        },
        with_log,
    },
    std::{
        collections::HashMap,
        env::{var as get_env, VarError},
        error, result,
    },
};

const ENVIRONMENT_VARIABLES: [&str; 10] = [
    "JSV_DEBUG",
    "JSV_QUIET",
    "JSV_APPEND",
    "JSV_LINE",
    "JSV_DELIM",
    "JSV_GUARD",
    "JSV_FORMAT",
    "JSV_BUF_OUT",
    "JSV_BUF_IN",
    "JSV_LINEREADER_EOL",
];

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Kind {
    Debug,
    Quiet,
    Append,
    Line,
    Delim,
    Guard,
    Format,
    BufIn,
    BufOut,
    RdrEol,
}

impl<S: AsRef<str>> From<S> for Kind {
    fn from(s: S) -> Self {
        match s.as_ref() {
            "JSV_DEBUG" => Kind::Debug,
            "JSV_QUIET" => Kind::Quiet,
            "JSV_APPEND" => Kind::Append,
            "JSV_LINE" => Kind::Line,
            "JSV_DELIM" => Kind::Delim,
            "JSV_GUARD" => Kind::Guard,
            "JSV_FORMAT" => Kind::Format,
            "JSV_BUF_OUT" => Kind::BufIn,
            "JSV_BUF_IN" => Kind::BufOut,
            "JSV_LINEREADER_EOL" => Kind::RdrEol,
            _ => panic!("Tried to convert bad &str to env Kind"),
        }
    }
}

#[derive(Debug, Default)]
pub(in crate::cli) struct EnvArgs {
    debug: OptDebug,
    quiet: OptQuiet,
    append: OptAppend,
    line: OptLine,
    delimiter: OptDelim,
    guard: OptGuard,
    format: OptFormat,
    output_buffer_size: OptBufOut,
    input_buffer_size: OptBufIn,
    linereader_eol: OptEOL,
}

impl EnvArgs {
    pub(in crate::cli) fn from_env() -> Self {
        let vars: HashMap<Kind, String> = ENVIRONMENT_VARIABLES
            .iter()
            .map(|s| (Kind::from(s), get_env(s)))
            .filter_map(|(k, res)| match res {
                Ok(s) => Some((k, s)),
                Err(e) => match e {
                    VarError::NotUnicode(val) => {
                        with_log!(None, warn!("Invalid unicode: {:?}, ignoring", val))
                    }
                    VarError::NotPresent => None,
                },
            })
            .collect();

        Self {
            debug: vars
                .get(&Kind::Debug)
                .and_then(|s| log_err(s.parse::<usize>(), &s)),
            quiet: vars
                .get(&Kind::Quiet)
                .and_then(|s| log_err(parse_wide_bool(s), &s)),
            append: vars
                .get(&Kind::Append)
                .and_then(|s| log_err(parse_wide_bool(s), &s)),
            line: vars
                .get(&Kind::Line)
                .and_then(|s| log_err(s.parse::<usize>(), &s)),
            delimiter: vars.get(&Kind::Delim).map(|s| Delimiter::from(s.as_str())),
            guard: vars.get(&Kind::Guard).and_then(|s| {
                if s.is_empty() {
                    Some(Guard::None)
                } else {
                    log_err(s.parse::<char>(), &s).map(|c| Guard::new(Some(c)))
                }
            }),
            format: vars.get(&Kind::Format).map(|s| {
                s.split('.')
                    .map(|field| Field::try_from_whitelist(field, &VALID_FIELDS))
                    .collect()
            }),
            output_buffer_size: vars
                .get(&Kind::BufOut)
                .and_then(|s| log_err(s.parse::<usize>(), &s)),
            input_buffer_size: vars
                .get(&Kind::BufIn)
                .and_then(|s| log_err(s.parse::<usize>(), &s)),
            linereader_eol: vars
                .get(&Kind::RdrEol)
                .and_then(|s| log_err(s.parse::<char>(), &s)),
        }
    }
}

impl ConfigMerge for EnvArgs {
    fn merge<T: ConfigMerge>(&mut self, mut other: T) {
        EnvArgs::priority_merge(&mut self.debug, other.debug_level());
        EnvArgs::priority_merge(&mut self.quiet, other.quiet());
        EnvArgs::priority_merge(&mut self.append, other.append());
        EnvArgs::priority_merge(&mut self.line, other.line());
        EnvArgs::priority_merge(&mut self.delimiter, other.delimiter());
        EnvArgs::priority_merge(&mut self.guard, other.guard());
        EnvArgs::priority_merge(&mut self.format, other.format());
        EnvArgs::priority_merge(&mut self.output_buffer_size, other.output_buffer_size());
        EnvArgs::priority_merge(&mut self.input_buffer_size, other.input_buffer_size());
        EnvArgs::priority_merge(&mut self.linereader_eol, other.linereader_eol());
    }

    fn debug_level(&mut self) -> Option<usize> {
        self.debug.take()
    }

    fn quiet(&mut self) -> Option<bool> {
        self.quiet.take()
    }

    fn append(&mut self) -> Option<bool> {
        self.append.take()
    }

    fn line(&mut self) -> Option<usize> {
        self.line.take()
    }

    fn delimiter(&mut self) -> Option<Delimiter> {
        self.delimiter.take()
    }

    fn guard(&mut self) -> Option<Guard> {
        self.guard.take()
    }

    fn format(&mut self) -> Option<Result<Vec<Field>>> {
        self.format.take()
    }

    fn input_buffer_size(&mut self) -> Option<usize> {
        self.input_buffer_size.take()
    }

    fn output_buffer_size(&mut self) -> Option<usize> {
        self.output_buffer_size.take()
    }

    fn linereader_eol(&mut self) -> Option<char> {
        self.linereader_eol.take()
    }
}

fn parse_wide_bool<S: AsRef<str>>(s: S) -> result::Result<bool, String> {
    match s.as_ref().to_ascii_lowercase().as_str() {
        "true" | "yes" | "1" => Ok(true),
        "false" | "no" | "0" => Ok(false),
        _ => Err(format!("Unable to parse into a bool")),
    }
}

fn log_err<T, E: std::fmt::Display, S: AsRef<str>>(
    result: result::Result<T, E>,
    var: S,
) -> Option<T> {
    match result {
        Ok(val) => Some(val),
        Err(e) => with_log!(None, debug!("Couldn't parse env '{}': {}", var.as_ref(), e)),
    }
}
