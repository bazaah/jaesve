use {
    crate::models::error::ErrorKind,
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
        io::{Read as ioRead, Result as ioResult, Stdin},
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

// Displays either 'Stdin' or a file name, if file name contains non ASCII
// characters, they are replaced with � (U+FFFD)
impl std::fmt::Display for ReadFrom {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let display = match self {
            ReadFrom::File(path) => format!(
                "File: {}",
                path.file_name().unwrap_or_default().to_string_lossy()
            ),
            ReadFrom::Stdin => format!("Stdin"),
        };

        write!(f, "{}", display)
    }
}

pub trait Builder<D>
where
    D: Into<Field>,
{
    type Block: std::fmt::Display;
    type Error;

    fn build_with(&self, d: D) -> Result<Self::Block, Self::Error>;

    fn identifer(&self) -> Result<Self::Block, Self::Error>;

    fn delimiter(&self) -> Result<Self::Block, Self::Error>;

    fn guard(&self) -> Result<Self::Block, Self::Error>;

    fn type_of(&self) -> Result<Self::Block, Self::Error>;

    fn pointer(&self) -> Result<Self::Block, Self::Error>;

    fn value(&self) -> Result<Self::Block, Self::Error>;
}

#[derive(Debug)]
pub enum BlockKind {
    Ident(usize),
    Delimiter(Delimiter),
    Guard(Delimiter),
    Type(JType),
    Pointer(String),
    Value(Option<String>),
}

impl std::fmt::Display for BlockKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            BlockKind::Ident(i) => write!(f, "{}", i),
            BlockKind::Delimiter(d) => write!(f, "{}", d),
            BlockKind::Guard(g) => write!(f, "{}", g),
            BlockKind::Type(t) => write!(f, "{}", t),
            BlockKind::Pointer(p) => write!(f, "{}", p),
            BlockKind::Value(v) => write!(f, "{}", v.as_ref().unwrap_or(&String::default())),
        }
    }
}

#[derive(Debug)]
pub struct Output {
    blocks: Vec<BlockKind>,
}

impl Output {
    fn get_ident(&self) -> Option<BlockKind> {
        self.blocks.iter().find_map(|kind| match kind {
            BlockKind::Ident(i) => Some(BlockKind::Ident(*i)),
            _ => None,
        })
    }

    fn get_delimiter(&self) -> Option<BlockKind> {
        self.blocks.iter().find_map(|kind| match kind {
            BlockKind::Delimiter(d) => Some(BlockKind::Delimiter(d.clone())),
            _ => None,
        })
    }

    fn get_guard(&self) -> Option<BlockKind> {
        self.blocks.iter().find_map(|kind| match kind {
            BlockKind::Guard(g) => Some(BlockKind::Guard(g.clone())),
            _ => None,
        })
    }

    fn get_type(&self) -> Option<BlockKind> {
        self.blocks.iter().find_map(|kind| match kind {
            BlockKind::Type(t) => Some(BlockKind::Type(*t)),
            _ => None,
        })
    }

    fn get_pointer(&self) -> Option<BlockKind> {
        self.blocks.iter().find_map(|kind| match kind {
            BlockKind::Pointer(p) => Some(BlockKind::Pointer(p.clone())),
            _ => None,
        })
    }

    fn get_value(&self) -> Option<BlockKind> {
        self.blocks.iter().find_map(|kind| match kind {
            BlockKind::Value(v) => Some(BlockKind::Value(v.clone())),
            _ => None,
        })
    }
}

impl<D> Builder<D> for Output
where
    D: Into<Field>,
{
    type Block = BlockKind;
    type Error = ErrorKind;
    fn build_with(&self, d: D) -> Result<Self::Block, Self::Error> {
        match d.into() {
            f @ Field::Identifier => self
                .get_ident()
                .ok_or(ErrorKind::MissingField(format!("{}", f))),
            f @ Field::Type => self
                .get_type()
                .ok_or(ErrorKind::MissingField(format!("{}", f))),
            f @ Field::Pointer => self
                .get_pointer()
                .ok_or(ErrorKind::MissingField(format!("{}", f))),
            f @ Field::Value => self
                .get_value()
                .ok_or(ErrorKind::MissingField(format!("{}", f))),
            _ => unreachable!("Make sure clap only allows valid fields to hit this"),
        }
    }

    fn identifer(&self) -> Result<Self::Block, Self::Error> {
        self.get_ident()
            .ok_or(ErrorKind::MissingField(format!("{}", Field::Identifier)))
    }

    fn delimiter(&self) -> Result<Self::Block, Self::Error> {
        self.get_delimiter()
            .ok_or(ErrorKind::MissingField(format!("{}", Field::Delimiter)))
    }

    fn guard(&self) -> Result<Self::Block, Self::Error> {
        self.get_guard()
            .ok_or(ErrorKind::MissingField(format!("{}", Field::Guard)))
    }

    fn type_of(&self) -> Result<Self::Block, Self::Error> {
        self.get_type()
            .ok_or(ErrorKind::MissingField(format!("{}", Field::Type)))
    }

    fn pointer(&self) -> Result<Self::Block, Self::Error> {
        self.get_pointer()
            .ok_or(ErrorKind::MissingField(format!("{}", Field::Pointer)))
    }

    fn value(&self) -> Result<Self::Block, Self::Error> {
        self.get_value()
            .ok_or(ErrorKind::MissingField(format!("{}", Field::Value)))
    }
}

#[derive(Debug)]
pub struct OutputBuilder {
    blocks: [Option<BlockKind>; 6],
}

impl OutputBuilder {
    pub fn new() -> Self {
        let blocks: [Option<BlockKind>; 6] = Default::default();
        Self { blocks }
    }

    pub fn done(mut self) -> Output {
        let mut blocks = Vec::with_capacity(self.blocks.len());
        for opt in &mut self.blocks {
            if opt.is_some() {
                let block = std::mem::replace(opt, None);
                blocks.push(block.unwrap())
            }
        }

        Output { blocks }
    }

    pub fn ident(mut self, id: usize) -> Self {
        self.blocks[0] = Some(BlockKind::Ident(id));
        self
    }

    pub fn delim(mut self, delim: Delimiter) -> Self {
        self.blocks[1] = Some(BlockKind::Delimiter(delim));
        self
    }

    pub fn guard(mut self, guard: Delimiter) -> Self {
        self.blocks[2] = Some(BlockKind::Guard(guard));
        self
    }

    pub fn type_of(mut self, jtype: JType) -> Self {
        self.blocks[3] = Some(BlockKind::Type(jtype));
        self
    }

    pub fn pointer(mut self, jptr: String) -> Self {
        self.blocks[4] = Some(BlockKind::Pointer(jptr));
        self
    }

    pub fn value(mut self, val: Option<String>) -> Self {
        self.blocks[5] = Some(BlockKind::Value(val));
        self
    }

    pub fn check(self, regex: Option<&RegexOptions>) -> Option<Self> {
        match regex {
            Some(regex) => match regex.on_field() {
                Field::Identifier => match self.blocks[0] {
                    Some(BlockKind::Ident(ref i)) if !regex.pattern().is_match(&i.to_string()) => {
                        None
                    }
                    _ => Some(self),
                },
                Field::Delimiter => match self.blocks[1] {
                    Some(BlockKind::Delimiter(ref d))
                        if !regex.pattern().is_match(&d.to_string()) =>
                    {
                        None
                    }
                    _ => Some(self),
                },
                Field::Guard => match self.blocks[2] {
                    Some(BlockKind::Guard(ref g)) if !regex.pattern().is_match(&g.to_string()) => {
                        None
                    }
                    _ => Some(self),
                },
                Field::Type => match self.blocks[3] {
                    Some(BlockKind::Type(ref t)) if !regex.pattern().is_match(&t.to_string()) => {
                        None
                    }
                    _ => Some(self),
                },
                Field::Pointer => match &self.blocks[4] {
                    Some(BlockKind::Pointer(ref p)) if !regex.pattern().is_match(p) => None,
                    _ => Some(self),
                },
                Field::Value => match self.blocks[5] {
                    Some(BlockKind::Value(ref o)) => match o {
                        Some(v) if !regex.pattern().is_match(v) => None,
                        None => None,
                        _ => Some(self),
                    },
                    _ => Some(self),
                },
            },
            None => Some(self),
        }
    }
}

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

#[derive(Debug, PartialEq, Clone, Copy, Hash, Eq)]
pub enum Field {
    Identifier,
    Delimiter,
    Guard,
    Type,
    Pointer,
    Value,
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
// otherwise validated conversions
impl From<&str> for Field {
    fn from(s: &str) -> Self {
        match s {
            "ident" => Field::Identifier,
            "delim" => Field::Delimiter,
            "grd"   => Field::Guard,
            "type" => Field::Type,
            "jptr" => Field::Pointer,
            "value" => Field::Value,
            _ => unreachable!("Called infallible conversion to Field on a fallible conversion, use try_from instead"),
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

    pub fn on_ident(&self) -> bool {
        match self.field {
            Field::Identifier => true,
            _ => false,
        }
    }
}

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

pub struct JsonScan<I> {
    iter: I,
    prev: Option<u8>,
    state: ScanState,
    /// (InQuotes, OutQuotes)
    offsets: (usize, usize),
}

impl<I> JsonScan<I>
where
    I: Iterator<Item = ioResult<u8>>,
{
    pub fn new(iter: I) -> JsonScan<I> {
        JsonScan {
            iter,
            prev: None,
            state: ScanState::OutQuotes,
            offsets: (0, 0),
        }
    }

    pub fn outside_quotes(&self) -> bool {
        match self.state {
            ScanState::OutQuotes => true,
            ScanState::InQuotes => false,
        }
    }

    pub fn offsets(&self) -> (usize, usize) {
        self.offsets
    }

    fn handle_state(&mut self) {
        match self.prev {
            Some(b'\\') => (),
            _ => match self.state {
                ScanState::InQuotes => {
                    self.offsets.1 = 0; // Reset OutQuotes counter
                    self.state = ScanState::OutQuotes
                }
                ScanState::OutQuotes => {
                    self.offsets.0 = 0; // Reset InQuotes counter
                    self.state = ScanState::InQuotes
                }
            },
        }
    }

    fn increment_offset(&mut self) {
        match self.state {
            ScanState::InQuotes => self.offsets.0 += 1,
            ScanState::OutQuotes => self.offsets.1 += 1,
        }
    }
}

impl<I> Iterator for JsonScan<I>
where
    I: Iterator<Item = ioResult<u8>>,
{
    type Item = ioResult<u8>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.iter.next() {
            Some(Ok(b @ b'"')) => {
                self.handle_state();
                //self.offset(); Should starting a new offset be 0 or 1?
                self.prev = Some(b);
                Some(Ok(b))
            }
            Some(Ok(b)) => {
                self.increment_offset();
                self.prev = Some(b);
                Some(Ok(b))
            }
            Some(Err(e)) => {
                self.increment_offset();
                self.prev = None;
                Some(Err(e))
            }
            None => None,
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
enum ScanState {
    InQuotes,
    OutQuotes,
}

pub struct JsonPointer<'j> {
    ident: usize,
    queue: VecDeque<(&'j JsonValue, String)>,
    pbuf: Vec<OutputBuilder>,
}

impl<'j> JsonPointer<'j> {
    pub fn new(json: &'j JsonValue, meta: (usize, String, Option<usize>)) -> Self {
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
                Some((jObject(map), ref s)) => {
                    for (k, v) in map.iter() {
                        let new_path = s.clone() + "/" + k;
                        if v.is_object() {
                            self.pbuf.push(
                                OutputBuilder::new()
                                    .ident(self.ident)
                                    .pointer(new_path.clone())
                                    .value(None)
                                    .type_of(v.into()),
                            );
                        }
                        if v.is_array() {
                            self.pbuf.push(
                                OutputBuilder::new()
                                    .ident(self.ident)
                                    .pointer(new_path.clone())
                                    .value(None)
                                    .type_of(v.into()),
                            );
                        }
                        self.queue.push_back((v, new_path));
                    }
                }
                Some((jArray(a), ref s)) => {
                    for (i, v) in a.iter().enumerate() {
                        let new_path = s.clone() + "/" + &i.to_string();
                        self.queue.push_back((v, new_path));
                    }
                }
                Some((jString(val), ref jptr)) => {
                    self.pbuf.push(
                        OutputBuilder::new()
                            .ident(self.ident)
                            .pointer(String::from(jptr))
                            .value(Some(val.to_string()))
                            .type_of(value.as_ref().unwrap().0.into()),
                    );
                    break;
                }
                Some((jNumber(val), ref jptr)) => {
                    self.pbuf.push(
                        OutputBuilder::new()
                            .ident(self.ident)
                            .pointer(String::from(jptr))
                            .value(Some(val.to_string()))
                            .type_of(value.as_ref().unwrap().0.into()),
                    );
                    break;
                }
                Some((jBool(val), ref jptr)) => {
                    self.pbuf.push(
                        OutputBuilder::new()
                            .ident(self.ident)
                            .pointer(String::from(jptr))
                            .value(Some(val.to_string()))
                            .type_of(value.as_ref().unwrap().0.into()),
                    );
                    break;
                }
                Some((tp @ jNull, jptr)) => {
                    self.pbuf.push(
                        OutputBuilder::new()
                            .ident(self.ident)
                            .pointer(jptr)
                            .value(Some(String::from("null")))
                            .type_of(tp.into()),
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

pub struct JsonPacket {
    ident: usize,
    base_path: String,
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

    pub fn into_inner(self) -> (JsonValue, (usize, String, Option<usize>)) {
        let hint = self.size_hint();
        (self.json, (self.ident, self.base_path, hint))
    }
}

impl TryFrom<(usize, String, Vec<u8>)> for JsonPacket {
    type Error = ErrorKind;

    fn try_from(packet: (usize, String, Vec<u8>)) -> std::result::Result<Self, Self::Error> {
        trace!(
            "trying to convert: {} {:?}",
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
