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
        collections::VecDeque,
        convert::TryFrom,
        error::Error,
        io::{Result as ioResult, Write as ioWrite},
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
    type Error: Error;

    fn build_with(&self, d: D) -> Result<Self::Block, Box<dyn self::Error>>;

    fn identifer(&self) -> Result<Self::Block, Box<dyn self::Error>>;

    fn delimiter(&self) -> Result<Self::Block, Box<dyn self::Error>>;

    fn r#type(&self) -> Result<Self::Block, Box<dyn self::Error>>;

    fn pointer(&self) -> Result<Self::Block, Box<dyn self::Error>>;

    fn value(&self) -> Result<Self::Block, Box<dyn self::Error>>;
}

#[derive(Debug)]
pub enum BlockKind {
    Ident(usize),
    Delimiter(Delimiter),
    Type(JType),
    Pointer(String),
    Value(Option<String>),
}

impl std::fmt::Display for BlockKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            BlockKind::Ident(i) => write!(f, "{}", i),
            BlockKind::Delimiter(d) => write!(f, "{}", d),
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
    fn build_with(&self, d: D) -> Result<Self::Block, Box<dyn self::Error>> {
        match d.into() {
            Field::Identifier => self.get_ident().ok_or(Box::new(ErrorKind::Generic)),
            Field::Type => self.get_type().ok_or(Box::new(ErrorKind::Generic)),
            Field::Pointer => self.get_pointer().ok_or(Box::new(ErrorKind::Generic)),
            Field::Value => self.get_value().ok_or(Box::new(ErrorKind::Generic)),
            _ => Err(Box::new(ErrorKind::Generic)),
        }
    }

    fn identifer(&self) -> Result<Self::Block, Box<dyn self::Error>> {
        self.get_ident().ok_or(Box::new(ErrorKind::Generic))
    }

    fn delimiter(&self) -> Result<Self::Block, Box<dyn self::Error>> {
        self.get_delimiter().ok_or(Box::new(ErrorKind::Generic))
    }

    fn r#type(&self) -> Result<Self::Block, Box<dyn self::Error>> {
        self.get_type().ok_or(Box::new(ErrorKind::Generic))
    }

    fn pointer(&self) -> Result<Self::Block, Box<dyn self::Error>> {
        self.get_pointer().ok_or(Box::new(ErrorKind::Generic))
    }

    fn value(&self) -> Result<Self::Block, Box<dyn self::Error>> {
        self.get_value().ok_or(Box::new(ErrorKind::Generic))
    }
}

#[derive(Debug)]
pub struct OutputBuilder {
    blocks: [Option<BlockKind>; 5],
}

impl OutputBuilder {
    pub fn new() -> Self {
        let blocks: [Option<BlockKind>; 5] = Default::default();
        Self { blocks }
    }

    pub fn done(mut self) -> Output {
        let mut blocks = Vec::new();
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

    pub fn type_of(mut self, jtype: JType) -> Self {
        self.blocks[2] = Some(BlockKind::Type(jtype));
        self
    }

    pub fn pointer(mut self, jptr: String) -> Self {
        self.blocks[3] = Some(BlockKind::Pointer(jptr));
        self
    }

    pub fn value(mut self, val: Option<String>) -> Self {
        self.blocks[4] = Some(BlockKind::Value(val));
        self
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

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Field {
    Identifier,
    Delimiter,
    Type,
    Pointer,
    Value,
}

impl From<&str> for Field {
    fn from(s: &str) -> Self {
        match s {
            "ident" => Field::Identifier,
            "delim" => Field::Delimiter,
            "type" => Field::Type,
            "jptr" => Field::Pointer,
            "value" => Field::Value,
            _ => panic!("Called infallible conversion to Field on a fallible conversion, use try_from instead"),
        }
    }
}

impl Default for Field {
    fn default() -> Self {
        Field::Pointer
    }
}

pub struct RegexOptions {
    regex: regex::Regex,
    column: Field,
}

impl RegexOptions {
    pub fn new(pattern: &str, column: Field) -> Self {
        // Checked by clap, unwrap here is safe
        let regex = regex::Regex::from_str(pattern).unwrap();
        RegexOptions { regex, column }
    }

    pub fn get_regex(&self) -> &regex::Regex {
        &self.regex
    }

    pub fn get_column(&self) -> Field {
        self.column
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
    ch: Option<ioResult<u8>>,
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
            ch: None,
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

    pub fn peak(&mut self) -> Option<&ioResult<u8>> {
        match self.ch {
            Some(ref ok @ Ok(_)) => Some(&ok),
            Some(ref err @ Err(_)) => Some(&err),
            None => match self.iter.next() {
                ch @ Some(_) => {
                    self.ch = ch;
                    self.ch.as_ref()
                }
                None => None,
            },
        }
    }

    pub fn discard(&mut self) {
        self.ch = None
    }

    pub fn return_error(self) -> ErrorKind {
        match self.ch.unwrap() {
            Err(e) => e.into(),
            _ => panic!("this should never happen"),
        }
    }

    fn internal_next(&mut self) -> Option<ioResult<u8>> {
        match self.ch.take() {
            ch @ Some(_) => ch,
            None => match self.iter.next() {
                ch @ Some(_) => {
                    self.ch = ch;
                    self.internal_next()
                }
                None => None,
            },
        }
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
        match self.internal_next() {
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

#[derive(Debug)]
pub enum PointerKind {
    Array(usize),
    Object(String),
}

impl From<usize> for PointerKind {
    fn from(u: usize) -> Self {
        PointerKind::Array(u)
    }
}

impl From<String> for PointerKind {
    fn from(s: String) -> Self {
        PointerKind::Object(s)
    }
}

impl From<&str> for PointerKind {
    fn from(s: &str) -> Self {
        PointerKind::Object(s.to_owned())
    }
}

impl std::fmt::Display for PointerKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            PointerKind::Array(u) => write!(f, "{}", u),
            PointerKind::Object(s) => write!(f, "{}", s),
        }
    }
}

// #[derive(Debug)]
// pub struct PointerStack<D, S>
// where
//     D: std::fmt::Display,
//     S: std::fmt::Display,
// {
//     list: Vec<D>,
//     split: S,
// }

// impl<D, S> PointerStack<D, S>
// where
//     D: std::fmt::Display,
//     S: std::fmt::Display,
// {
//     pub fn new(list: Vec<D>, split: S) -> Self {
//         Self { list, split }
//     }

//     pub fn push(&mut self, item: D) {
//         self.list.push(item)
//     }

//     pub fn pop(&mut self) -> Option<D> {
//         self.list.pop()
//     }
// }

// impl<D, S> AsRef<Vec<D>> for PointerStack<D, S>
// where
//     D: std::fmt::Display,
//     S: std::fmt::Display,
// {
//     fn as_ref(&self) -> &Vec<D> {
//         &self.list
//     }
// }

// impl<D, S> AsMut<Vec<D>> for PointerStack<D, S>
// where
//     D: std::fmt::Display,
//     S: std::fmt::Display,
// {
//     fn as_mut(&mut self) -> &mut Vec<D> {
//         &mut self.list
//     }
// }

// impl<D, S> std::fmt::Display for PointerStack<D, S>
// where
//     D: std::fmt::Display,
//     S: std::fmt::Display,
// {
//     fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
//         write!(f, "{}", self.split)?;
//         for (_, last, item) in self.list.iter().identify_first_last() {
//             if !last {
//                 write!(f, "{}{}", item, self.split)?;
//             } else {
//                 write!(f, "{}", item)?;
//             }
//         }
//         Ok(())
//     }
// }

// pub fn parse_json(&'j mut self) {
//     match self.item.size_hint() {
//         Some(hint) => self.queue.reserve(hint),
//         None => (),
//     }
//     let path = self.item.base_path.clone();
//     self.queue.push_back((&self.item.json, path));

//     loop {
//         let value = self.queue.pop_front();
//         match value {
//             Some((jObject(map), ref s)) => {
//                 for (k, v) in map.iter() {
//                     let new_path = s.clone() + "/" + k;
//                     if v.is_object() {
//                         self.pbuf.push(new_path.clone());
//                     }
//                     if v.is_array() {
//                         self.pbuf.push(new_path.clone());
//                     }
//                     self.queue.push_back((v, new_path));
//                 }
//             }
//             Some((jArray(a), ref s)) => {
//                 for (i, v) in a.iter().enumerate() {
//                     let new_path = s.clone() + "/" + &i.to_string();
//                     self.queue.push_back((v, new_path));
//                 }
//             }
//             Some((jString(_), s)) => self.pbuf.push(s),
//             Some((jNumber(_), s)) => self.pbuf.push(s),
//             Some((jBool(_), s)) => self.pbuf.push(s),
//             Some((jNull, s)) => self.pbuf.push(s),
//             None => break,
//         }
//     }
// }
