use {
    crate::{
        cli::ProgramArgs,
        models::{
            block::{Identifier, JType, JmesPath, JsonPointer, JsonValue},
            builder::OutputBuilder,
            error::{Context, ErrContext, Error},
            field::Field,
            pointer::{Pointer, PointerKind, PointerParts},
        },
        CLI,
    },
    serde_json::{
        from_slice, Value as Json,
        Value::{
            Array as jArray, Bool as jBool, Null as jNull, Number as jNumber, Object as jObject,
            String as jString,
        },
    },
    std::{
        collections::VecDeque,
        convert::TryFrom,
        fmt,
        fs::File,
        io::{Read as ioRead, Stdin},
        path::PathBuf,
        str::{from_utf8, FromStr},
    },
};

/// Convenience macro for logging match arms
#[macro_export]
macro_rules! with_log {
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
#[derive(Debug)]
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
        if self.0.is_empty() {
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

/// Struct responsible for turning each unwound
/// JSON object into the components that Output / Builder
/// will use
pub struct BlockGenerator<'j, 'args: 'j> {
    queue: VecDeque<(Option<&'j Json>, Option<PointerKind>)>,
    buffer: Vec<OutputBuilder>,
    opts: &'args ProgramArgs,
    pristine: bool,
}

impl<'j, 'args> BlockGenerator<'j, 'args> {
    pub fn new(
        opts: &'args ProgramArgs,
        json: Option<&'j Json>,
        meta: (Option<PointerKind>, Option<usize>),
    ) -> Self {
        let (mut queue, buffer) = match (json, meta.1) {
            (_, Some(hint)) => (VecDeque::with_capacity(hint), Vec::with_capacity(hint)),
            (_, None) => (VecDeque::new(), Vec::new()),
        };
        queue.push_back((json, meta.0));

        BlockGenerator {
            queue,
            buffer,
            opts,
            pristine: true,
        }
    }

    pub fn parse_next(&mut self) -> Option<OutputBuilder> {
        while let Some(value) = self.queue.pop_front() {
            match value {
                (Some(jObject(map)), ref ptr) => {
                    for (k, v) in map.iter() {
                        let new_path = ptr.as_ref().map(|p| p.clone_extend(k.as_str()));
                        if v.is_object() {
                            self.output_checked(&new_path, None.into(), v.into());
                        }
                        if v.is_array() {
                            self.output_checked(&new_path, None.into(), v.into());
                        }
                        self.queue.push_back((Some(v), new_path));
                    }
                }
                (Some(jArray(a)), ref ptr) => {
                    for (i, v) in a.iter().map(Some).enumerate() {
                        let new_path = ptr.as_ref().map(|p| p.clone_extend(i));
                        self.queue.push_back((v, new_path));
                    }
                }
                (Some(jString(val)), ref ptr) => {
                    self.output_checked(
                        &ptr,
                        Some(val)
                            .filter(|_| self.opts.should_calculate(Field::Value))
                            .cloned()
                            .into(),
                        value.0.unwrap().into(),
                    );
                    break;
                }
                (Some(jNumber(val)), ref ptr) => {
                    self.output_checked(
                        &ptr,
                        Some(val)
                            .filter(|_| self.opts.should_calculate(Field::Value))
                            .map(|val| val.to_string())
                            .into(),
                        value.0.unwrap().into(),
                    );
                    break;
                }
                (Some(jBool(val)), ref ptr) => {
                    self.output_checked(
                        &ptr,
                        Some(val)
                            .filter(|_| self.opts.should_calculate(Field::Value))
                            .map(|val| val.to_string())
                            .into(),
                        value.0.unwrap().into(),
                    );
                    break;
                }
                (Some(tp @ jNull), ptr) => {
                    self.output_checked(
                        &ptr,
                        Some(tp)
                            .filter(|_| self.opts.should_calculate(Field::Value))
                            .map(|_| String::from("null"))
                            .into(),
                        tp.into(),
                    );
                    break;
                }
                (None, _) => {
                    // Ugly fix for if there is no Json to unwind, should rewrite this somehow
                    if self.pristine {
                        self.pristine = false;
                        return Some(OutputBuilder::new());
                    } else {
                        break;
                    }
                }
            }
        }
        self.buffer.pop()
    }

    /// Custom output storage checker due to the difficulties induced by PointerKind
    fn output_checked(&mut self, ptr: &Option<PointerKind>, jval: JsonValue, jtype: JType) {
        let s = &*self;
        let mut builder = OutputBuilder::new();

        if s.opts.should_store(Field::Pointer) {
            builder.store_unchecked(
                ptr.as_ref()
                    .map(|p| -> JsonPointer { p.as_complete().into() }),
            )
        }
        if s.opts.should_store(Field::Value) {
            builder.store_unchecked(Some(jval))
        }
        if s.opts.should_store(Field::Type) {
            builder.store_unchecked(Some(jtype))
        }
        if s.opts.should_store(Field::JmesPath) {
            builder.store_unchecked(ptr.as_ref().map(|p| JmesPath::from(p)))
        }
        self.buffer.push(builder);
    }
}

impl<'j, 'args> Iterator for BlockGenerator<'j, 'args> {
    type Item = OutputBuilder;

    fn next(&mut self) -> Option<Self::Item> {
        self.parse_next()
    }
}

type ToBlockGen = (
    Option<Json>,
    Option<Identifier>,
    (Option<PointerKind>, Option<usize>),
);

/// Convenience intermediate struct for
/// turning the raw parts that the scanner / unwinding
/// sends to JsonPointer
pub struct JsonPacket {
    ident: Option<Identifier>,
    base_path: Option<PointerKind>,
    json: Option<Json>,
}

impl JsonPacket {
    fn size_hint(&self) -> Option<usize> {
        match self.json {
            Some(jObject(ref val)) => match val.iter().size_hint() {
                (_, Some(ub)) => Some(ub),
                (lb, None) => Some(lb),
            },
            Some(jArray(ref val)) => Some(val.len()),
            _ => None,
        }
    }

    pub fn into_inner(self) -> ToBlockGen {
        let hint = self.size_hint();
        (self.json, self.ident, (self.base_path, hint))
    }
}

impl TryFrom<(Option<Identifier>, Option<PointerKind>, Option<Vec<u8>>)> for JsonPacket {
    type Error = Error;

    fn try_from(
        packet: (Option<Identifier>, Option<PointerKind>, Option<Vec<u8>>),
    ) -> std::result::Result<Self, Self::Error> {
        trace!(
            "trying to convert: {:?}",
            packet.2.as_ref().map(|vec| from_utf8(vec)).or_untracked()
        );
        let json: Option<Json> = packet
            .2
            .map(|data| {
                from_slice(data.as_slice()).context(
                    Some(data.len())
                        .filter(|l| *l >= CLI.input_buffer_size())
                        .map(|l| Context::DataLenEqualLineBufferLen(l)),
                )
            })
            .transpose()?;

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

pub trait OrDisplay {
    type Item: std::fmt::Debug;
    fn or_display<'a, 'b>(&'a self, fallback: &'b str) -> NoneFallback<'a, 'b, Self::Item>;
    fn or_untracked<'a>(&'a self) -> NoneDefault<'a, Self::Item>;
}

impl<T> OrDisplay for Option<T>
where
    T: std::fmt::Debug,
{
    type Item = T;
    fn or_display<'a, 'b>(&'a self, fallback: &'b str) -> NoneFallback<'a, 'b, T> {
        self.as_ref()
            .map_or(NoneFallback::from(None, fallback), |val| {
                NoneFallback::from(Some(val), "")
            })
    }

    fn or_untracked<'a>(&'a self) -> NoneDefault<'a, T> {
        NoneDefault::from(self.as_ref())
    }
}

pub struct NoneDefault<'a, T: fmt::Debug>(Option<&'a T>);

impl<'a, T> NoneDefault<'a, T>
where
    T: fmt::Debug,
{
    fn from(inner: Option<&'a T>) -> Self {
        NoneDefault(inner)
    }
}

impl<'a, T> fmt::Debug for NoneDefault<'a, T>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            Some(val) => write!(f, "{:?}", val),
            None => write!(f, "untracked"),
        }
    }
}

impl<'a, T> fmt::Display for NoneDefault<'a, T>
where
    T: fmt::Debug + fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            Some(val) => write!(f, "{}", val),
            None => write!(f, "untracked"),
        }
    }
}

pub struct NoneFallback<'a, 'b, T>
where
    T: fmt::Debug,
{
    inner: Option<&'a T>,
    or_else: &'b str,
}

impl<'a, 'b, T> NoneFallback<'a, 'b, T>
where
    T: fmt::Debug,
{
    fn from(inner: Option<&'a T>, or_else: &'b str) -> Self {
        NoneFallback { inner, or_else }
    }
}

impl<'a, 'b, T> fmt::Debug for NoneFallback<'a, 'b, T>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.inner {
            Some(val) => write!(f, "{:?}", val),
            None => write!(f, "{}", self.or_else),
        }
    }
}

impl<'a, 'b, T> fmt::Display for NoneFallback<'a, 'b, T>
where
    T: fmt::Debug + fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.inner {
            Some(val) => write!(f, "{}", val),
            None => write!(f, "{}", self.or_else),
        }
    }
}
