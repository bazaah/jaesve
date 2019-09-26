use {
    crate::models::{
        field::{AsField, Field},
        pointer::{Pointer, PointerParts},
        IdentifyFirstLast,
    },
    serde_json::{
        Value as Json,
        Value::{
            Array as jArray, Bool as jBool, Null as jNull, Number as jNumber, Object as jObject,
            String as jString,
        },
    },
};

/// Enum containing every valid output kind
/// used by OutputBuilder / Output
#[derive(Debug, Clone)]
pub enum BlockKind {
    Ident(Identifier),
    Delimiter(Delimiter),
    Guard(Guard),
    Type(JType),
    Pointer(JsonPointer),
    Value(JsonValue),
    Jmes(JmesPath),
}

impl std::fmt::Display for BlockKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            BlockKind::Ident(i) => write!(f, "{}", i),
            BlockKind::Delimiter(d) => write!(f, "{}", d),
            BlockKind::Guard(g) => write!(f, "{}", g),
            BlockKind::Type(t) => write!(f, "{}", t),
            BlockKind::Pointer(p) => write!(f, "{}", p),
            BlockKind::Value(v) => write!(f, "{}", v),
            BlockKind::Jmes(j) => write!(f, "{}", j),
        }
    }
}

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

impl From<Json> for JType {
    fn from(json: Json) -> Self {
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

impl From<&Json> for JType {
    fn from(json: &Json) -> Self {
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

impl Into<BlockKind> for JType {
    fn into(self) -> BlockKind {
        BlockKind::Type(self)
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

/// Wrapper around the file identifier type
#[derive(Debug, Clone, Copy)]
pub struct Identifier {
    inner: usize,
}

impl From<usize> for Identifier {
    fn from(inner: usize) -> Self {
        Identifier { inner }
    }
}

impl AsRef<usize> for Identifier {
    fn as_ref(&self) -> &usize {
        &self.inner
    }
}

impl Into<BlockKind> for Identifier {
    fn into(self) -> BlockKind {
        BlockKind::Ident(self)
    }
}

impl AsField for Identifier {
    fn as_field(&self) -> Field {
        Field::Identifier
    }
}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.inner)
    }
}

/// Wrapper around the jptr type
#[derive(Debug, Clone)]
pub struct JsonPointer {
    inner: String,
}

impl From<String> for JsonPointer {
    fn from(inner: String) -> Self {
        JsonPointer { inner }
    }
}

impl AsRef<String> for JsonPointer {
    fn as_ref(&self) -> &String {
        &self.inner
    }
}

impl Into<BlockKind> for JsonPointer {
    fn into(self) -> BlockKind {
        BlockKind::Pointer(self)
    }
}

impl AsField for JsonPointer {
    fn as_field(&self) -> Field {
        Field::Pointer
    }
}

impl std::fmt::Display for JsonPointer {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.inner)
    }
}

#[derive(Debug, Clone)]
pub struct JsonValue {
    inner: Option<String>,
}

impl From<Option<String>> for JsonValue {
    fn from(inner: Option<String>) -> Self {
        JsonValue { inner }
    }
}

impl AsRef<Option<String>> for JsonValue {
    fn as_ref(&self) -> &Option<String> {
        &self.inner
    }
}

impl Into<BlockKind> for JsonValue {
    fn into(self) -> BlockKind {
        BlockKind::Value(self)
    }
}

impl AsField for JsonValue {
    fn as_field(&self) -> Field {
        Field::Value
    }
}

impl std::fmt::Display for JsonValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.inner {
            Some(ref s) => write!(f, "{}", s),
            None => write!(f, ""),
        }
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

impl Into<BlockKind> for Delimiter {
    fn into(self) -> BlockKind {
        BlockKind::Delimiter(self)
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

impl Into<BlockKind> for Guard {
    fn into(self) -> BlockKind {
        BlockKind::Guard(self)
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
        if !self.0.is_empty() {
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

impl Into<BlockKind> for JmesPath {
    fn into(self) -> BlockKind {
        BlockKind::Jmes(self)
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
