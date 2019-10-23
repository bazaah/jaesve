use {
    crate::models::{
        assets::IdentifyFirstLast,
        block::BlockKind,
        error::{ErrorKind, Result},
    },
    std::{
        collections::{hash_map::RandomState, HashSet},
        fmt::Write as fmtWrite,
        iter::FromIterator,
    },
};

#[cfg(feature = "config-file")]
use serde::Deserialize;

/// Provides an bridge for moving from some kind of type to a BlockKind
/// using a Field as a marker for which type belongs to which BlockKind
pub trait AsField: Into<BlockKind> {
    fn as_field(&self) -> Field;
}

/// Functions as a cheap marker for representing
/// an abstract BlockKind
#[cfg_attr(feature = "config-file", derive(Deserialize))]
#[derive(Debug, PartialEq, Clone, Copy, Hash, Eq)]
pub enum Field {
    Identifier,
    Delimiter,
    Guard,
    Type,
    Pointer,
    Value,
    JmesPath,
}

impl Field {
    /// Associated fn emulating TryFrom, workaround
    /// for the annoying implicit impl deriving from From
    pub fn try_from(s: &str) -> Result<Self> {
        match s {
            "ident" => Ok(Field::Identifier),
            "delim" => Ok(Field::Delimiter),
            "grd" => Ok(Field::Guard),
            "type" => Ok(Field::Type),
            "jptr" => Ok(Field::Pointer),
            "value" => Ok(Field::Value),
            "jmes" => Ok(Field::JmesPath),
            _ => Err(ErrorKind::Message(format!("'{}' is not a valid field", s)).into()),
        }
    }

    /// Convenience wrapper around try_from, further
    /// restricting the cast to the allowed variants in whitelist
    pub fn try_from_whitelist(s: &str, whitelist: &[Field]) -> Result<Self> {
        let res = Field::try_from(s)?;

        match HashSet::<&Field, RandomState>::from_iter(whitelist).contains(&res) {
            true => Ok(res),
            false => Err(ErrorKind::Message(format!(
                "'{}' is not a valid field: {}",
                s,
                Field::print_slice(whitelist)?
            ))
            .into()),
        }
    }

    /// Private display impl to avoid unnecessary foreign wrappers
    fn print_slice(slice: &[Field]) -> Result<String> {
        let mut buffer = String::with_capacity(slice.len() * 6);
        let iter = slice.iter().identify_first_last();
        for item in iter {
            match item {
                (true, _, field) => write!(&mut buffer, "['{}' ", field)
                    .map_err(|e| ErrorKind::Message(format!("{}", e)))?,
                (_, true, field) => write!(&mut buffer, "'{}']", field)
                    .map_err(|e| ErrorKind::Message(format!("{}", e)))?,
                (false, false, field) => write!(&mut buffer, "{} ", field)
                    .map_err(|e| ErrorKind::Message(format!("{}", e)))?,
            }
        }

        Ok(buffer)
    }
}

// Unchecked conversion, should only be used on
// otherwise (clap) validated conversions
impl From<&str> for Field {
    fn from(s: &str) -> Self {
        match s {
            "ident" => Field::Identifier,
            "delim" => Field::Delimiter,
            "grd"   => Field::Guard,
            "type" => Field::Type,
            "jptr" => Field::Pointer,
            "value" => Field::Value,
            "jmes" => Field::JmesPath,
            _ => unreachable!("Called infallible conversion to Field on a fallible conversion, use Field::try_from instead"),
        }
    }
}

impl From<Field> for &str {
    fn from(f: Field) -> Self {
        match f {
            Field::Identifier => "ident",
            Field::Delimiter => "delim",
            Field::Guard => "grd",
            Field::Type => "type",
            Field::Pointer => "jptr",
            Field::Value => "value",
            Field::JmesPath => "jmes",
        }
    }
}

impl<T: AsRef<BlockKind>> From<T> for Field {
    fn from(kind: T) -> Self {
        match kind.as_ref() {
            BlockKind::Ident(_) => Field::Identifier,
            BlockKind::Delimiter(_) => Field::Delimiter,
            BlockKind::Guard(_) => Field::Guard,
            BlockKind::Type(_) => Field::Type,
            BlockKind::Pointer(_) => Field::Pointer,
            BlockKind::Value(_) => Field::Value,
            BlockKind::Jmes(_) => Field::JmesPath,
        }
    }
}

impl std::fmt::Display for Field {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Field::Identifier => write!(f, "ident"),
            Field::Delimiter => write!(f, "delim"),
            Field::Guard => write!(f, "grd"),
            Field::Type => write!(f, "type"),
            Field::Pointer => write!(f, "jptr"),
            Field::Value => write!(f, "value"),
            Field::JmesPath => write!(f, "jmes"),
        }
    }
}

impl AsRef<Field> for Field {
    fn as_ref(&self) -> &Field {
        self
    }
}

impl Default for Field {
    fn default() -> Self {
        Field::Pointer
    }
}
