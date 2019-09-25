use {
    crate::models::{
        assets::IdentifyFirstLast,
        builder::BlockKind,
        error::ErrorKind,
        pointer::{Pointer, PointerParts},
    },
    serde_json::{
        Value as JsonValue,
        Value::{
            Array as jArray, Bool as jBool, Null as jNull, Number as jNumber, Object as jObject,
            String as jString,
        },
    },
    std::{
        collections::{hash_map::RandomState, HashSet},
        fmt::Write as fmtWrite,
        iter::FromIterator,
    },
};

/// Provides an bridge for moving from some kind of type to a BlockKind
/// using a Field as a marker for which type belongs to which BlockKind
pub trait AsField: Into<BlockKind> {
    fn as_field(&self) -> Field;
}

// TODO: Turn these into wrapper types
impl AsField for usize {
    fn as_field(&self) -> Field {
        Field::Identifier
    }
}

impl AsField for String {
    fn as_field(&self) -> Field {
        Field::Pointer
    }
}

impl AsField for Option<String> {
    fn as_field(&self) -> Field {
        Field::Value
    }
}

/// Functions as a cheap marker for representing
/// an abstract BlockKind
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
    pub fn try_from(s: &str) -> Result<Self, ErrorKind> {
        match s {
            "ident" => Ok(Field::Identifier),
            "delim" => Ok(Field::Delimiter),
            "grd" => Ok(Field::Guard),
            "type" => Ok(Field::Type),
            "jptr" => Ok(Field::Pointer),
            "value" => Ok(Field::Value),
            "jmes" => Ok(Field::JmesPath),
            _ => Err(ErrorKind::Message(format!("'{}' is not a valid field", s))),
        }
    }

    /// Convenience wrapper around try_from, further
    /// restricting the cast to the allowed variants in whitelist
    pub fn try_from_whitelist(s: &str, whitelist: &[Field]) -> Result<Self, ErrorKind> {
        let res = Field::try_from(s)?;

        match HashSet::<&Field, RandomState>::from_iter(whitelist).contains(&res) {
            true => Ok(res),
            false => Err(ErrorKind::Message(format!(
                "'{}' is not a valid field: {}",
                s,
                Field::print_slice(whitelist)?
            ))),
        }
    }

    /// Private display impl to avoid unnecessary foreign wrappers
    fn print_slice(slice: &[Field]) -> Result<String, ErrorKind> {
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

/* Output block types */

/// Economical wrapper for storing the Json Value type
#[derive(Debug, Clone, Copy)]
pub enum JType {
    Object,
    Array,
    String,
    Number,
    Bool,
    Null,
}

impl From<JsonValue> for JType {
    fn from(json: JsonValue) -> Self {
        match json {
            jObject(_) => JType::Object,
            jArray(_) => JType::Array,
            jString(_) => JType::String,
            jNumber(_) => JType::Number,
            jBool(_) => JType::Bool,
            jNull => JType::Null,
        }
    }
}

impl From<&JsonValue> for JType {
    fn from(json: &JsonValue) -> Self {
        match json {
            jObject(_) => JType::Object,
            jArray(_) => JType::Array,
            jString(_) => JType::String,
            jNumber(_) => JType::Number,
            jBool(_) => JType::Bool,
            jNull => JType::Null,
        }
    }
}

impl std::fmt::Display for JType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let kind = match self {
            JType::Object => "Object",
            JType::Array => "Array",
            JType::String => "String",
            JType::Number => "Number",
            JType::Bool => "Bool",
            JType::Null => "Null",
        };

        write!(f, "{}", kind)
    }
}

impl AsField for JType {
    fn as_field(&self) -> Field {
        Field::Type
    }
}

impl Default for JType {
    fn default() -> Self {
        Self::Null
    }
}

/// Type 'char' wrapper specialized for
/// output delimiters, avoids locking the delimiter
/// to a single char, and avoids unnecessary
/// cloning on a single char
#[derive(Debug)]
pub enum Delimiter {
    Char(char),
    Multiple(Vec<char>),
}

impl From<&str> for Delimiter {
    fn from(s: &str) -> Self {
        let len = |hint: (usize, Option<usize>)| -> usize { hint.1.unwrap_or(hint.0) };
        if len(s.chars().size_hint()) > 1 {
            let ch_buf: Vec<char> = s.chars().collect();
            Delimiter::Multiple(ch_buf)
        } else {
            let ch = s.chars().take(1).next().unwrap_or_else(|| {
                warn!("no delimiter detected, using default");
                ','
            });
            Delimiter::Char(ch)
        }
    }
}

impl Clone for Delimiter {
    fn clone(&self) -> Self {
        match self {
            Delimiter::Char(c) => Delimiter::Char(*c),
            Delimiter::Multiple(vec) => Delimiter::Multiple(vec.clone()),
        }
    }
}

impl std::fmt::Display for Delimiter {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Delimiter::Char(c) => write!(f, "{}", c),
            Delimiter::Multiple(vec) => {
                for c in vec {
                    write!(f, "{}", c)?;
                }
                Ok(())
            }
        }
    }
}

impl AsField for Delimiter {
    fn as_field(&self) -> Field {
        Field::Delimiter
    }
}

impl Default for Delimiter {
    fn default() -> Self {
        Delimiter::Char(',')
    }
}

/// Type 'char' wrapper specialized for
/// output field guards, allowing for
/// no guard or 1 guard char
#[derive(Debug, Clone, Copy)]
pub enum Guard {
    Some(char),
    None,
}

impl Guard {
    pub fn new(grd: Option<char>) -> Self {
        match grd {
            Some(c) => Guard::Some(c),
            None => Guard::None,
        }
    }
}

impl std::fmt::Display for Guard {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Guard::Some(c) => write!(f, "{}", c),
            Guard::None => write!(f, ""),
        }
    }
}

impl AsField for Guard {
    fn as_field(&self) -> Field {
        Field::Guard
    }
}

impl Default for Guard {
    fn default() -> Self {
        Guard::None
    }
}

#[derive(Debug, Clone)]
pub struct JmesPath {
    inner: String,
}

impl JmesPath {
    pub fn from<P: Pointer>(p: &P) -> Self {
        match p.as_parts() {
            Ok(parts) => JmesPath {
                inner: format!("{}", JmesDisplay::from(parts)),
            },
            Err(_) => {
                warn!("Could not assemble jmespath... skipping");
                JmesPath {
                    inner: String::default(),
                }
            }
        }
    }
}

/// Effectively implements a specialized Display for Pointer
struct JmesDisplay<'a>(&'a Vec<PointerParts>);

impl<'a> From<&'a Vec<PointerParts>> for JmesDisplay<'a> {
    fn from(p: &'a Vec<PointerParts>) -> JmesDisplay<'a> {
        JmesDisplay(p)
    }
}

impl<'a> std::fmt::Display for JmesDisplay<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // Check for unsupported JSON, i.e JSON literals
        if !(self.0.len() == 0) {
            for (first, _, item) in self
                .0
                .iter()
                .filter(|part| match part {
                    PointerParts::Slash => false,
                    _ => true,
                })
                .identify_first_last()
            {
                match item {
                    PointerParts::Object(s) => {
                        if first {
                            write!(f, "{}", s)?
                        } else {
                            write!(f, ".{}", s)?
                        }
                    }
                    PointerParts::Array(u) => write!(f, "[{}]", u)?,
                    PointerParts::Slash => {}
                }
            }

            Ok(())
        } else {
            // If unsupported, return what appears to be the standard response
            write!(f, "null")
        }
    }
}

impl AsRef<str> for JmesPath {
    fn as_ref(&self) -> &str {
        &self.inner
    }
}

impl std::fmt::Display for JmesPath {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.inner)
    }
}

impl AsField for JmesPath {
    fn as_field(&self) -> Field {
        Field::JmesPath
    }
}

impl Default for JmesPath {
    fn default() -> Self {
        JmesPath {
            inner: String::default(),
        }
    }
}
