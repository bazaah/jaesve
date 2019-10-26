use {
    super::{args::*, merge::ConfigMerge},
    crate::{
        cli::VALID_FIELDS,
        models::{
            block::{Delimiter, Guard},
            error::Result as CrateResult,
            field::Field,
        },
        with_log,
    },
    dirs::{config_dir, home_dir},
    serde::{
        de::{Error as deError, Visitor},
        {Deserialize, Deserializer},
    },
    std::{
        fmt,
        fs::read,
        iter::FromIterator,
        marker::PhantomData,
        path::{Path, PathBuf},
    },
    toml::from_slice as toml_from,
};

/// Merges all config files including potential user defined locations
/// Ordering is important here: files (values) processed earlier have a higher priority
/// over later files
///
/// Correct ordering is: [USERFILES..., HOME DIRECTORY, CONFIG DIRECTORY]
pub(super) fn merge_config_files<P: AsRef<Path>>(extra: &[P]) -> FileArgs {
    // Note that ordering is important here
    let config_locations = [
        home_dir().map(|mut p| {
            p.push("jaesve.conf");
            p
        }),
        config_dir().map(|mut p| {
            p.push("jaesve.conf");
            p
        }),
        Some(PathBuf::from("/etc/jaesve.conf")),
    ];

    extra
        .iter()
        .map(|p| p.as_ref())
        .chain(
            config_locations
                .iter()
                .filter_map(|opt| opt.as_ref().map(|p| p.as_path())),
        )
        .map(|path| try_parse_file(path))
        .filter_map(|res| match res {
            Ok(file) => Some(file),
            Err(e) => with_log!(None, warn!("Unable to open config path: {}", e)),
        })
        .collect()
}

fn try_parse_file<P: AsRef<Path>>(path: P) -> CrateResult<FileArgs> {
    toml_from(&read(path)?).map_err(|e| e.into())
}
#[derive(Deserialize, Default, Debug)]
#[serde(from = "ArgsBuilder")]
pub(in crate::cli) struct FileArgs {
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

impl ConfigMerge for FileArgs {
    fn merge<T: ConfigMerge>(&mut self, mut other: T) {
        FileArgs::priority_merge(&mut self.debug, other.debug_level());
        FileArgs::priority_merge(&mut self.line, other.line());
        FileArgs::priority_merge(&mut self.quiet, other.quiet());
        FileArgs::priority_merge(&mut self.append, other.append());
        FileArgs::priority_merge(&mut self.delimiter, other.delimiter());
        FileArgs::priority_merge(&mut self.guard, other.guard());
        FileArgs::priority_merge(&mut self.format, other.format());
        FileArgs::priority_merge(&mut self.output_buffer_size, other.output_buffer_size());
        FileArgs::priority_merge(&mut self.input_buffer_size, other.input_buffer_size());
        FileArgs::priority_merge(&mut self.linereader_eol, other.linereader_eol());
    }

    fn debug_level(&mut self) -> Option<usize> {
        self.debug.take()
    }

    fn line(&mut self) -> Option<usize> {
        self.line.take()
    }

    fn quiet(&mut self) -> Option<bool> {
        self.quiet.take()
    }

    fn append(&mut self) -> Option<bool> {
        self.append.take()
    }

    fn delimiter(&mut self) -> Option<Delimiter> {
        self.delimiter.take()
    }

    fn guard(&mut self) -> Option<Guard> {
        self.guard.take()
    }

    fn format(&mut self) -> Option<CrateResult<Vec<Field>>> {
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

impl Extend<FileArgs> for FileArgs {
    fn extend<I: IntoIterator<Item = FileArgs>>(&mut self, iter: I) {
        for other in iter {
            self.merge(other)
        }
    }
}

impl FromIterator<FileArgs> for FileArgs {
    fn from_iter<I: IntoIterator<Item = FileArgs>>(iter: I) -> Self {
        let mut args = FileArgs::default();
        args.extend(iter);

        args
    }
}

impl From<ArgsBuilder> for FileArgs {
    fn from(build: ArgsBuilder) -> Self {
        match build {
            ArgsBuilder {
                debug,
                line,
                quiet,
                append,
                delimiter,
                guard,
                format,
                subconfig,
            } => match subconfig {
                Some(sub) => match sub {
                    SubConfigBuilder {
                        output_buffer_size,
                        input_buffer_size,
                        linereader_eol,
                    } => Self {
                        debug,
                        line,
                        quiet,
                        append,
                        delimiter,
                        guard,
                        format,
                        output_buffer_size,
                        input_buffer_size,
                        linereader_eol,
                    },
                },
                None => Self {
                    debug,
                    line,
                    quiet,
                    append,
                    delimiter,
                    guard,
                    format,
                    output_buffer_size: None,
                    input_buffer_size: None,
                    linereader_eol: None,
                },
            },
        }
    }
}

#[derive(Deserialize, Default, Debug)]
struct ArgsBuilder {
    debug: Option<usize>,
    #[serde(deserialize_with = "deserialize_wide_bool", default)]
    quiet: Option<bool>,
    #[serde(deserialize_with = "deserialize_wide_bool", default)]
    append: Option<bool>,
    line: Option<usize>,
    delimiter: Option<Delimiter>,
    guard: Option<Guard>,
    #[serde(deserialize_with = "deserialize_format", default)]
    format: Option<CrateResult<Vec<Field>>>,
    #[serde(rename(deserialize = "config"))]
    subconfig: Option<SubConfigBuilder>,
}

#[derive(Deserialize, Default, Debug)]
struct SubConfigBuilder {
    output_buffer_size: Option<usize>,
    input_buffer_size: Option<usize>,
    linereader_eol: Option<char>,
}

fn deserialize_format<'de, D>(deserializer: D) -> Result<Option<CrateResult<Vec<Field>>>, D::Error>
where
    D: Deserializer<'de>,
{
    struct FormatVisitor(PhantomData<fn() -> Option<CrateResult<Vec<Field>>>>);

    impl<'de> Visitor<'de> for FormatVisitor {
        type Value = Option<CrateResult<Vec<Field>>>;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("a dot separated list of fields")
        }

        fn visit_none<E: deError>(self) -> Result<Self::Value, E> {
            Ok(None)
        }

        fn visit_str<E: deError>(self, v: &str) -> Result<Self::Value, E> {
            Ok(Some(
                v.split('.')
                    .map(|field| Field::try_from_whitelist(field, &VALID_FIELDS))
                    .collect(),
            ))
        }
    }

    deserializer.deserialize_str(FormatVisitor(PhantomData))
}

fn deserialize_wide_bool<'de, D>(deserializer: D) -> Result<Option<bool>, D::Error>
where
    D: Deserializer<'de>,
{
    struct BoolVisitor(PhantomData<fn() -> Option<bool>>);

    impl<'de> Visitor<'de> for BoolVisitor {
        type Value = Option<bool>;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("one of, case insensitive: true, false, yes, no, 0, 1")
        }

        fn visit_none<E: deError>(self) -> Result<Self::Value, E> {
            Ok(None)
        }

        fn visit_bool<E: deError>(self, v: bool) -> Result<Self::Value, E> {
            Ok(Some(v))
        }

        fn visit_i64<E: deError>(self, v: i64) -> Result<Self::Value, E> {
            Ok(match v {
                0 => Some(false),
                1 => Some(true),
                _ => None,
            })
        }

        fn visit_string<E: deError>(self, mut v: String) -> Result<Self::Value, E> {
            v.make_ascii_lowercase();
            Ok(match v.as_str() {
                "true" | "yes" | "1" => Some(true),
                "false" | "no" | "0" => Some(false),
                _ => None,
            })
        }

        fn visit_str<E: deError>(self, v: &str) -> Result<Self::Value, E> {
            Ok(match v.to_ascii_lowercase().as_str() {
                "true" | "yes" | "1" => Some(true),
                "false" | "no" | "0" => Some(false),
                _ => None,
            })
        }
    }

    deserializer.deserialize_any(BoolVisitor(PhantomData))
}

#[cfg(test)]
mod tests {
    use {super::*, crate::cli::ConfigMerge, std::error, toml::from_str as deserialize_str};

    type Result<T> = std::result::Result<T, Box<dyn error::Error>>;

    macro_rules! mock {
        (
            $kv:expr
        ) => {
            deserialize_str::<FileArgs>($kv)
        };
        (
            $kv:expr => $table:expr
        ) => {
            deserialize_str::<FileArgs>(concat!("[", $table, "]\n", $kv))
        };
    }

    macro_rules! allowed_true {
        (
            $key:expr
        ) => {
            &[
                concat!($key, " = 'yes'"),
                concat!($key, " = 'Yes'"),
                concat!($key, " = 'YES'"),
                concat!($key, " = true"),
                concat!($key, " = 'true'"),
                concat!($key, " = 'True'"),
                concat!($key, " = 'TRUE'"),
                concat!($key, " = '1'"),
                concat!($key, " = 1"),
            ]
        };
    }

    macro_rules! allowed_false {
        (
            $key:expr
        ) => {
            &[
                concat!($key, " = 'no'"),
                concat!($key, " = 'No'"),
                concat!($key, " = 'NO'"),
                concat!($key, " = false"),
                concat!($key, " = 'false'"),
                concat!($key, " = 'False'"),
                concat!($key, " = 'FALSE'"),
                concat!($key, " = '0'"),
                concat!($key, " = 0"),
            ]
        };
    }

    #[test]
    fn arg_debug_level() -> Result<()> {
        let mut data = mock!("debug = 1")?;

        assert_eq!(data.debug_level(), Some(1));
        Ok(())
    }

    #[test]
    fn arg_quiet_true() -> Result<()> {
        for arg in allowed_true!("quiet") {
            let mut data: FileArgs = mock!(arg)?;
            assert_eq!(data.quiet(), Some(true));
        }

        Ok(())
    }

    #[test]
    fn arg_quiet_false() -> Result<()> {
        for arg in allowed_false!("quiet") {
            let mut data: FileArgs = mock!(arg)?;
            assert_eq!(data.quiet(), Some(false));
        }

        Ok(())
    }

    #[test]
    fn arg_append_true() -> Result<()> {
        for arg in allowed_true!("append") {
            let mut data: FileArgs = mock!(arg)?;
            assert_eq!(data.append(), Some(true));
        }

        Ok(())
    }

    #[test]
    fn arg_append_false() -> Result<()> {
        for arg in allowed_false!("append") {
            let mut data: FileArgs = mock!(arg)?;
            assert_eq!(data.append(), Some(false));
        }

        Ok(())
    }

    #[test]
    fn arg_line() -> Result<()> {
        let mut data: FileArgs = mock!("line = 100")?;

        assert_eq!(data.line(), Some(100));
        Ok(())
    }

    #[test]
    fn arg_delimiter_single() -> Result<()> {
        let mut data: FileArgs = mock!("delimiter = ','")?;

        assert_eq!(
            format!("{:?}", data.delimiter()),
            format!("{:?}", Some(Delimiter::from(",")))
        );
        Ok(())
    }

    #[test]
    fn arg_delimiter_multiple() -> Result<()> {
        let mut data: FileArgs = mock!("delimiter = '::'")?;

        assert_eq!(
            format!("{:?}", data.delimiter()),
            format!("{:?}", Some(Delimiter::from("::")))
        );
        Ok(())
    }

    #[test]
    fn arg_guard_single() -> Result<()> {
        let mut data: FileArgs = mock!("guard = '~'")?;

        assert_eq!(
            format!("{:?}", data.guard()),
            format!("{:?}", Some(Guard::from("~")))
        );
        Ok(())
    }

    #[test]
    fn arg_guard_none() -> Result<()> {
        let mut data: FileArgs = mock!("guard = ''")?;

        assert_eq!(
            format!("{:?}", data.guard()),
            format!("{:?}", Some(Guard::None))
        );
        Ok(())
    }

    #[test]
    fn arg_format_multiple() -> Result<()> {
        let mut data: FileArgs = mock!("format = 'ident.jptr.type.value'")?;
        let format = data.format().transpose()?;

        assert_eq!(
            format.as_ref().map(|v| v.as_slice()),
            Some([Field::Identifier, Field::Pointer, Field::Type, Field::Value].as_ref())
        );

        Ok(())
    }

    #[test]
    fn arg_format_single() -> Result<()> {
        let mut data: FileArgs = mock!("format = 'ident'")?;
        let format = data.format().transpose()?;

        assert_eq!(
            format.as_ref().map(|v| v.as_slice()),
            Some([Field::Identifier].as_ref())
        );

        Ok(())
    }

    #[test]
    fn arg_input_buffer_size() -> Result<()> {
        let mut data = mock!("input_buffer_size = 16" => "config")?;

        assert_eq!(data.input_buffer_size(), Some(16));
        Ok(())
    }

    #[test]
    fn arg_output_buffer_size() -> Result<()> {
        let mut data = mock!("output_buffer_size = 64" => "config")?;

        assert_eq!(data.output_buffer_size(), Some(64));
        Ok(())
    }

    #[test]
    fn arg_linereader_eol() -> Result<()> {
        let mut data = mock!("linereader_eol = ';'" => "config")?;

        assert_eq!(data.linereader_eol(), Some(';'));
        Ok(())
    }
}
