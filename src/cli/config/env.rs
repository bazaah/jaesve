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
        ffi::OsStr,
        iter::FromIterator,
        result,
    },
};

/// List of possible variables
const ENVIRONMENT_VARIABLES: [&str; 11] = [
    "JAESVE_DEBUG",
    "JAESVE_QUIET",
    "JAESVE_APPEND",
    "JAESVE_LINE",
    "JAESVE_DELIM",
    "JAESVE_GUARD",
    "JAESVE_FORMAT",
    "JAESVE_BUF_OUT",
    "JAESVE_BUF_IN",
    "JAESVE_LINEREADER_EOL",
    "JAESVE_FACTOR",
];

/// Marker for cheap var representation
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum Kind {
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
    Factor,
}

impl<S: AsRef<str>> From<S> for Kind {
    fn from(s: S) -> Self {
        match s.as_ref() {
            "JAESVE_DEBUG" => Kind::Debug,
            "JAESVE_QUIET" => Kind::Quiet,
            "JAESVE_APPEND" => Kind::Append,
            "JAESVE_LINE" => Kind::Line,
            "JAESVE_DELIM" => Kind::Delim,
            "JAESVE_GUARD" => Kind::Guard,
            "JAESVE_FORMAT" => Kind::Format,
            "JAESVE_BUF_OUT" => Kind::BufIn,
            "JAESVE_BUF_IN" => Kind::BufOut,
            "JAESVE_LINEREADER_EOL" => Kind::RdrEol,
            "JAESVE_FACTOR" => Kind::Factor,
            _ => panic!("Tried to convert bad &str to env Kind"),
        }
    }
}

/// Container for args collected from the environment
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
    factor: OptFactor,
}

impl EnvArgs {
    fn get_args<F>(env: F) -> Self
    where
        F: FromEnv,
    {
        let vars: HashMap<Kind, String> = ENVIRONMENT_VARIABLES
            .iter()
            .map(|s| (Kind::from(s), env.from_env(s)))
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
            factor: vars
                .get(&Kind::Factor)
                .and_then(|s| log_err(parse_factor(&s), &s)),
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
        EnvArgs::priority_merge(&mut self.factor, other.factor());
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

    fn factor(&mut self) -> Option<usize> {
        self.factor.take()
    }
}

/// Environment arg builder, collecting from the selected environment
#[derive(Debug)]
pub(in crate::cli) struct Env<E = Live> {
    environment: E,
}

impl<E: FromEnv> Env<E> {
    pub(in crate::cli) fn collect(self) -> EnvArgs {
        EnvArgs::get_args(self.environment)
    }

    /// Pass in a custom environment, useful when mocking
    #[allow(dead_code)]
    pub(super) fn with_environment(env: E) -> Self {
        Env { environment: env }
    }
}

impl Default for Env {
    fn default() -> Self {
        Env { environment: Live }
    }
}

/// Controls how Env returns environment variables,
/// defaulting to the actual program environment
pub(in crate::cli) trait FromEnv {
    fn from_env(&self, key: &dyn AsRef<OsStr>) -> result::Result<String, VarError> {
        get_env(key.as_ref())
    }
}

pub(in crate::cli) struct Live;

impl FromEnv for Live {}

/// Helper for allowing a wide variety of commonly used boolean indicators
fn parse_wide_bool<S: AsRef<str>>(s: S) -> result::Result<bool, String> {
    match s.as_ref().to_ascii_lowercase().as_str() {
        "true" | "yes" | "1" => Ok(true),
        "false" | "no" | "0" => Ok(false),
        _ => Err(format!("Unable to parse into a bool")),
    }
}

/// Helper for parsing byte multiplier indicators, with a fallback to
/// parsing a number
fn parse_factor<S: AsRef<str>>(s: S) -> result::Result<usize, String> {
    match s.as_ref() {
        "B" => Ok(1),
        "K" => Ok(1024),
        "M" => Ok(1024 * 1024),
        _ => Err(format!("Unable to parse into usize")),
    }
    .or_else(|e| s.as_ref().parse::<usize>().map_err(|_| e))
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

/// Mock environment generator used in tests
#[derive(Debug, Default)]
pub(super) struct Mock<'a> {
    map: HashMap<Kind, &'a str>,
}

impl<'a> FromIterator<(Kind, &'a str)> for Mock<'a> {
    fn from_iter<I: IntoIterator<Item = (Kind, &'a str)>>(iter: I) -> Self {
        let map: HashMap<Kind, &str> = iter.into_iter().collect();

        Mock { map }
    }
}

impl<'a> FromEnv for Mock<'a> {
    fn from_env(&self, key: &dyn AsRef<OsStr>) -> result::Result<String, VarError> {
        self.map
            .get(&Kind::from(key.as_ref().to_str().unwrap()))
            .map(|s| Ok((*s).to_string()))
            .unwrap_or(Err(VarError::NotPresent))
    }
}

#[cfg(test)]
mod tests {
    use {
        super::*,
        crate::models::{
            block::{Delimiter, Guard},
            field::Field,
        },
    };

    macro_rules! mock {
        ( $( $key:expr, $val:expr ),* ) => {{
            let iter = [ $( ($key, $val) )* ];

            let mock: Mock = iter.iter().cloned().collect();

            Env::with_environment(mock).collect()
        }}
    }

    const ALLOWED_TRUE: [&str; 7] = ["yes", "Yes", "YES", "true", "True", "TRUE", "1"];
    const ALLOWED_FALSE: [&str; 7] = ["no", "No", "NO", "false", "False", "FALSE", "0"];

    #[test]
    fn arg_debug_level() {
        let mut data = mock!(Kind::Debug, "3");

        assert_eq!(data.debug_level(), Some(3));
    }

    #[test]
    fn arg_quiet_true() {
        for arg in ALLOWED_TRUE.iter() {
            let mut data = mock!(Kind::Quiet, *arg);

            assert_eq!(data.quiet(), Some(true));
        }
    }

    #[test]
    fn arg_quiet_false() {
        for arg in ALLOWED_FALSE.iter() {
            let mut data = mock!(Kind::Quiet, *arg);

            assert_eq!(data.quiet(), Some(false));
        }
    }

    #[test]
    fn arg_append_true() {
        for arg in ALLOWED_TRUE.iter() {
            let mut data = mock!(Kind::Append, *arg);

            assert_eq!(data.append(), Some(true));
        }
    }

    #[test]
    fn arg_append_false() {
        for arg in ALLOWED_FALSE.iter() {
            let mut data = mock!(Kind::Append, *arg);

            assert_eq!(data.append(), Some(false));
        }
    }

    #[test]
    fn arg_line() {
        let mut data = mock!(Kind::Line, "42");

        assert_eq!(data.line(), Some(42));
    }

    #[test]
    fn arg_delimiter_single() {
        let mut data = mock!(Kind::Delim, ",");

        assert_eq!(
            format!("{:?}", data.delimiter()),
            format!("{:?}", Some(Delimiter::from(",")))
        );
    }

    #[test]
    fn arg_delimiter_multiple() {
        let mut data = mock!(Kind::Delim, "||");

        assert_eq!(
            format!("{:?}", data.delimiter()),
            format!("{:?}", Some(Delimiter::from("||")))
        );
    }

    #[test]
    fn arg_guard_single() {
        let mut data = mock!(Kind::Guard, "_");

        assert_eq!(
            format!("{:?}", data.guard()),
            format!("{:?}", Some(Guard::from("_")))
        );
    }

    #[test]
    fn arg_guard_none() {
        let mut data = mock!(Kind::Guard, "");

        assert_eq!(
            format!("{:?}", data.guard()),
            format!("{:?}", Some(Guard::from("")))
        );
    }

    #[test]
    fn arg_format_multiple() -> result::Result<(), Box<dyn std::error::Error>> {
        let mut data = mock!(Kind::Format, "ident.jptr.type.value");
        let format = data.format().transpose()?;

        assert_eq!(
            format.as_ref().map(|v| v.as_slice()),
            Some([Field::Identifier, Field::Pointer, Field::Type, Field::Value].as_ref())
        );

        Ok(())
    }

    #[test]
    fn arg_format_single() -> result::Result<(), Box<dyn std::error::Error>> {
        let mut data = mock!(Kind::Format, "ident");
        let format = data.format().transpose()?;

        assert_eq!(
            format.as_ref().map(|v| v.as_slice()),
            Some([Field::Identifier].as_ref())
        );

        Ok(())
    }

    #[test]
    fn arg_buf_in() {
        let mut data = mock!(Kind::BufIn, "16");

        assert_eq!(data.input_buffer_size(), Some(16))
    }

    #[test]
    fn arg_buf_out() {
        let mut data = mock!(Kind::BufOut, "64");

        assert_eq!(data.output_buffer_size(), Some(64))
    }

    #[test]
    fn arg_linereader_eol() {
        let mut data = mock!(Kind::RdrEol, ".");

        assert_eq!(data.linereader_eol(), Some('.'))
    }

    #[test]
    fn arg_factor() {
        let mut data = mock!(Kind::Factor, "B");

        assert_eq!(data.factor(), Some(1))
    }
}
