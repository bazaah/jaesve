use {
    crate::models::{
        builder::{BlockKind, OutputBuilder},
        error::ErrorKind,
        pointer::{Pointer, PointerKind, PointerParts},
    },
    serde_json::{
        from_slice, Value as JsonValue,
        Value::{
            Array as jArray, Bool as jBool, Null as jNull, Number as jNumber, Object as jObject,
            String as jString,
        },
    },
    std::{
        collections::{hash_map::RandomState, HashSet, VecDeque},
        convert::TryFrom,
        fmt::Write as fmtWrite,
        fs::File,
        io::{Read as ioRead, Stdin},
        iter::FromIterator,
        path::PathBuf,
        str::{from_utf8, FromStr},
    },
};

/// Convenience macro for logging match arms
#[macro_export]
macro_rules! match_with_log {
    ( $val:expr, $log:expr) => {{
        $log;
        $val
    }};
}

/// Supported read source options
#[derive(Debug)]
pub enum ReadFrom {
    File(PathBuf),
    Stdin,
}

// Displays either 'Stdin' or a file name, if file name contains non ASCII
// characters, they are replaced with � (U+FFFD)
impl std::fmt::Display for ReadFrom {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ReadFrom::File(path) => write!(
                f,
                "File: {}",
                path.file_name().unwrap_or_default().to_string_lossy()
            ),
            ReadFrom::Stdin => write!(f, "Stdin"),
        }
    }
}

/// Wrapper around supported Read types
/// avoiding dynamic dispatch
pub enum ReadKind {
    File(File),
    Stdin(Stdin),
}

impl ReadKind {
    pub fn into_inner(self) -> Box<dyn ioRead> {
        match self {
            ReadKind::File(f) => Box::new(f),
            ReadKind::Stdin(s) => Box::new(s),
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

impl From<BlockKind> for Field {
    fn from(kind: BlockKind) -> Self {
        match kind {
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

impl Default for Field {
    fn default() -> Self {
        Field::Pointer
    }
}

/// Contains the regex and the field
/// upon which to match against
#[derive(Debug)]
pub struct RegexOptions {
    regex: regex::Regex,
    field: Field,
}

impl RegexOptions {
    pub fn new(pattern: &str, field: Field) -> Self {
        // Checked by clap, unwrap here is safe
        let regex = regex::Regex::from_str(pattern).unwrap();
        RegexOptions { regex, field }
    }

    pub fn pattern(&self) -> &regex::Regex {
        &self.regex
    }

    pub fn on_field(&self) -> Field {
        self.field
    }

    pub fn is_ident(&self) -> bool {
        match self.field {
            Field::Identifier => true,
            _ => false,
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

#[derive(Debug, Clone)]
pub struct JmesPath {
    inner: String,
}

impl JmesPath {
    pub fn try_from<P: Pointer>(p: &P) -> Result<Self, ErrorKind> {
        let parts = p.as_parts()?;
        let mut inner = String::with_capacity(
            parts
                .iter()
                .map(|kind| match kind {
                    PointerParts::Slash => 1,
                    PointerParts::Array(_) => 3,
                    PointerParts::Object(s) => s.len(),
                })
                .fold(0, |acc, num| acc + num),
        );

        for (first, _, item) in parts
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
                        write!(&mut inner, "{}", s)?;
                    } else {
                        write!(&mut inner, ".{}", s)?;
                    }
                }
                PointerParts::Array(u) => {
                    write!(&mut inner, "[{}]", u)?;
                }
                PointerParts::Slash => {}
            }
        }

        Ok(JmesPath { inner })
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

/// Struct responsible for turning each unwound
/// JSON object into the components that Output / Builder
/// will use
pub struct JsonPointer<'j> {
    ident: usize,
    queue: VecDeque<(&'j JsonValue, PointerKind)>,
    pbuf: Vec<OutputBuilder>,
}

impl<'j> JsonPointer<'j> {
    pub fn new(json: &'j JsonValue, meta: (usize, PointerKind, Option<usize>)) -> Self {
        let (mut queue, pbuf) = match meta.2 {
            Some(hint) => (VecDeque::with_capacity(hint), Vec::with_capacity(hint)),
            None => (VecDeque::new(), Vec::new()),
        };

        queue.push_back((json, meta.1));

        Self {
            ident: meta.0,
            queue,
            pbuf,
        }
    }

    pub fn parse_next(&mut self) -> Option<OutputBuilder> {
        loop {
            let value = self.queue.pop_front();
            match value {
                Some((jObject(map), ref ptr)) => {
                    for (k, v) in map.iter() {
                        let new_path = ptr.clone_extend(k.as_str());
                        if v.is_object() {
                            self.pbuf.push(
                                OutputBuilder::new()
                                    .ident(self.ident)
                                    .pointer(new_path.as_complete())
                                    .value(None)
                                    .type_of(v.into())
                                    .jmes_path(JmesPath::try_from(&new_path)),
                            );
                        }
                        if v.is_array() {
                            self.pbuf.push(
                                OutputBuilder::new()
                                    .ident(self.ident)
                                    .pointer(new_path.as_complete())
                                    .value(None)
                                    .type_of(v.into())
                                    .jmes_path(JmesPath::try_from(&new_path)),
                            );
                        }
                        self.queue.push_back((v, new_path));
                    }
                }
                Some((jArray(a), ref ptr)) => {
                    for (i, v) in a.iter().enumerate() {
                        let new_path = ptr.clone_extend(i);
                        self.queue.push_back((v, new_path));
                    }
                }
                Some((jString(val), ref ptr)) => {
                    self.pbuf.push(
                        OutputBuilder::new()
                            .ident(self.ident)
                            .pointer(ptr.as_complete())
                            .value(Some(val.to_string()))
                            .type_of(value.as_ref().unwrap().0.into())
                            .jmes_path(JmesPath::try_from(ptr)),
                    );
                    break;
                }
                Some((jNumber(val), ref ptr)) => {
                    self.pbuf.push(
                        OutputBuilder::new()
                            .ident(self.ident)
                            .pointer(ptr.as_complete())
                            .value(Some(val.to_string()))
                            .type_of(value.as_ref().unwrap().0.into())
                            .jmes_path(JmesPath::try_from(ptr)),
                    );
                    break;
                }
                Some((jBool(val), ref ptr)) => {
                    self.pbuf.push(
                        OutputBuilder::new()
                            .ident(self.ident)
                            .pointer(ptr.as_complete())
                            .value(Some(val.to_string()))
                            .type_of(value.as_ref().unwrap().0.into())
                            .jmes_path(JmesPath::try_from(ptr)),
                    );
                    break;
                }
                Some((tp @ jNull, ptr)) => {
                    self.pbuf.push(
                        OutputBuilder::new()
                            .ident(self.ident)
                            .pointer(ptr.as_complete())
                            .value(Some(String::from("null")))
                            .type_of(tp.into())
                            .jmes_path(JmesPath::try_from(&ptr)),
                    );
                    break;
                }
                None => break,
            }
        }
        self.pbuf.pop()
    }
}

impl<'j> Iterator for JsonPointer<'j> {
    type Item = OutputBuilder;

    fn next(&mut self) -> Option<Self::Item> {
        self.parse_next()
    }
}

/// Convenience intermediate struct for
/// turning the raw parts that the scanner / unwinding
/// sends to JsonPointer
pub struct JsonPacket {
    ident: usize,
    base_path: PointerKind,
    json: JsonValue,
}

impl JsonPacket {
    fn size_hint(&self) -> Option<usize> {
        match self.json {
            jObject(ref val) => match val.iter().size_hint() {
                (_, Some(ub)) => Some(ub),
                (lb, None) => Some(lb),
            },
            jArray(ref val) => Some(val.len()),
            _ => None,
        }
    }

    pub fn into_inner(self) -> (JsonValue, (usize, PointerKind, Option<usize>)) {
        let hint = self.size_hint();
        (self.json, (self.ident, self.base_path, hint))
    }
}

impl TryFrom<(usize, PointerKind, Vec<u8>)> for JsonPacket {
    type Error = ErrorKind;

    fn try_from(packet: (usize, PointerKind, Vec<u8>)) -> std::result::Result<Self, Self::Error> {
        trace!(
            "trying to convert: {:?} {:?}",
            &packet.1,
            from_utf8(&packet.2)
        );
        let json: JsonValue = from_slice(packet.2.as_slice())?;

        Ok(JsonPacket {
            ident: packet.0,
            base_path: packet.1,
            json,
        })
    }
}

/// Custom iterator interface for checking if an item
/// is the first or last item in an iterator
/// returns a tuple -> (is_first: bool, is_last: bool, item)
pub trait IdentifyFirstLast: Iterator + Sized {
    fn identify_first_last(self) -> FirstLast<Self>;
}

impl<I> IdentifyFirstLast for I
where
    I: Iterator,
{
    /// (is_first: bool, is_last: bool, item)
    fn identify_first_last(self) -> FirstLast<Self> {
        FirstLast(true, self.peekable())
    }
}

pub struct FirstLast<I>(bool, std::iter::Peekable<I>)
where
    I: Iterator;

impl<I> Iterator for FirstLast<I>
where
    I: Iterator,
{
    type Item = (bool, bool, I::Item);

    fn next(&mut self) -> Option<Self::Item> {
        let first = std::mem::replace(&mut self.0, false);
        self.1.next().map(|e| (first, self.1.peek().is_none(), e))
    }
}
