use {
    crate::{
        cli::ProgramArgs,
        models::{
            assets::RegexOptions,
            block::BlockKind,
            error::ErrorKind,
            field::{AsField, Field},
        },
    },
    fnv::FnvHashMap,
};

/// Interface for converting the collected output parts into
/// output
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

    fn jmes(&self) -> Result<Self::Block, Self::Error>;
}

/// Container for the various final parts
/// used to assemble the program's output
#[derive(Debug)]
pub struct Output {
    blocks: FnvHashMap<usize, BlockKind>,
}
// 0 == ident
// 1 == delimiter
// 2 == guard
// 3 == type
// 4 == jptr
// 5 == value
// 6 == jmes
// Remember to update OutputBuilder's done() if you change these
impl Output {
    fn get_ident(&self) -> Option<&BlockKind> {
        self.blocks.get(&0)
    }

    fn get_delimiter(&self) -> Option<&BlockKind> {
        self.blocks.get(&1)
    }

    fn get_guard(&self) -> Option<&BlockKind> {
        self.blocks.get(&2)
    }

    fn get_type(&self) -> Option<&BlockKind> {
        self.blocks.get(&3)
    }

    fn get_pointer(&self) -> Option<&BlockKind> {
        self.blocks.get(&4)
    }

    fn get_value(&self) -> Option<&BlockKind> {
        self.blocks.get(&5)
    }

    fn get_jmes(&self) -> Option<&BlockKind> {
        self.blocks.get(&6)
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
                .cloned()
                .ok_or_else(|| ErrorKind::MissingField(format!("{}", f))),
            f @ Field::Type => self
                .get_type()
                .cloned()
                .ok_or_else(|| ErrorKind::MissingField(format!("{}", f))),
            f @ Field::Pointer => self
                .get_pointer()
                .cloned()
                .ok_or_else(|| ErrorKind::MissingField(format!("{}", f))),
            f @ Field::Value => self
                .get_value()
                .cloned()
                .ok_or_else(|| ErrorKind::MissingField(format!("{}", f))),
            f @ Field::JmesPath => self
                .get_jmes()
                .cloned()
                .ok_or_else(|| ErrorKind::MissingField(format!("{}", f))),
            _ => unreachable!("Make sure clap only allows valid fields to hit this"),
        }
    }

    fn identifer(&self) -> Result<Self::Block, Self::Error> {
        self.get_ident()
            .cloned()
            .ok_or_else(|| ErrorKind::MissingField(format!("{}", Field::Identifier)))
    }

    fn delimiter(&self) -> Result<Self::Block, Self::Error> {
        self.get_delimiter()
            .cloned()
            .ok_or_else(|| ErrorKind::MissingField(format!("{}", Field::Delimiter)))
    }

    fn guard(&self) -> Result<Self::Block, Self::Error> {
        self.get_guard()
            .cloned()
            .ok_or_else(|| ErrorKind::MissingField(format!("{}", Field::Guard)))
    }

    fn type_of(&self) -> Result<Self::Block, Self::Error> {
        self.get_type()
            .cloned()
            .ok_or_else(|| ErrorKind::MissingField(format!("{}", Field::Type)))
    }

    fn pointer(&self) -> Result<Self::Block, Self::Error> {
        self.get_pointer()
            .cloned()
            .ok_or_else(|| ErrorKind::MissingField(format!("{}", Field::Pointer)))
    }

    fn value(&self) -> Result<Self::Block, Self::Error> {
        self.get_value()
            .cloned()
            .ok_or_else(|| ErrorKind::MissingField(format!("{}", Field::Value)))
    }

    fn jmes(&self) -> Result<Self::Block, Self::Error> {
        self.get_jmes()
            .cloned()
            .ok_or_else(|| ErrorKind::MissingField(format!("{}", Field::Value)))
    }
}

/// Used to build up an Output struct
#[derive(Debug)]
pub struct OutputBuilder {
    blocks: [Option<BlockKind>; 7],
}

impl OutputBuilder {
    pub fn new() -> Self {
        let blocks: [Option<BlockKind>; 7] = Default::default();
        Self { blocks }
    }

    // Checked storage of output fields
    pub fn store<T: AsField>(&mut self, opts: &ProgramArgs, item: Option<T>) {
        if let Some(i) = item {
            if opts.should_store(i.as_field()) {
                self.store_unchecked(Some(i))
            }
        }
    }

    // Unchecked storage of output fields, should only be used if item was checked in some other way
    pub fn store_unchecked<T: AsField>(&mut self, item: Option<T>) {
        if let Some(i) = item {
            match <T as Into<BlockKind>>::into(i) {
                BlockKind::Ident(i) => self.blocks[0] = Some(BlockKind::Ident(i)),
                BlockKind::Delimiter(i) => self.blocks[1] = Some(BlockKind::Delimiter(i)),
                BlockKind::Guard(i) => self.blocks[2] = Some(BlockKind::Guard(i)),
                BlockKind::Type(i) => self.blocks[3] = Some(BlockKind::Type(i)),
                BlockKind::Pointer(i) => self.blocks[4] = Some(BlockKind::Pointer(i)),
                BlockKind::Value(i) => self.blocks[5] = Some(BlockKind::Value(i)),
                BlockKind::Jmes(i) => self.blocks[6] = Some(BlockKind::Jmes(i)),
            }
        }
    }

    pub fn done(mut self) -> Output {
        let mut blocks =
            FnvHashMap::with_capacity_and_hasher(self.blocks.len(), Default::default());
        for opt in &mut self.blocks {
            if let Some(block) = opt.take() {
                // Hardcoded usizes for keys
                // If you change these YOU MUST UPDATE the
                // store_unchecked method AND the get_xx
                // functions in Output too
                match block {
                    i @ BlockKind::Ident(_) => {
                        blocks.insert(0, i);
                    }
                    d @ BlockKind::Delimiter(_) => {
                        blocks.insert(1, d);
                    }
                    g @ BlockKind::Guard(_) => {
                        blocks.insert(2, g);
                    }
                    t @ BlockKind::Type(_) => {
                        blocks.insert(3, t);
                    }
                    p @ BlockKind::Pointer(_) => {
                        blocks.insert(4, p);
                    }
                    v @ BlockKind::Value(_) => {
                        blocks.insert(5, v);
                    }
                    j @ BlockKind::Jmes(_) => {
                        blocks.insert(6, j);
                    }
                }
            };
        }

        Output { blocks }
    }

    /// Function is designed to be used with a filter_map
    /// will return None if a regex exists and failed to match,
    /// otherwise always returns Some
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
                    Some(BlockKind::Pointer(ref p)) if !regex.pattern().is_match(p.as_ref()) => {
                        None
                    }
                    _ => Some(self),
                },
                Field::Value => match self.blocks[5] {
                    Some(BlockKind::Value(ref o)) => match o.as_ref() {
                        Some(v) if !regex.pattern().is_match(v) => None,
                        // This arm excludes any jptrs that do not a have an (single) associated value
                        // i.e objects and arrays... for example
                        // "/" refers to the entire JSON doc and as such does not have single value (its 'value' is the entire doc)
                        // The decision to exclude these items is made on the assumptions that:
                        // 1. If the user matches on the value field they expect the program to ignore "empty" fields
                        // 2. It reduces output clutter when the user is looking for something
                        // It has one substantial downside:
                        // 1. If the user is looking for non-existent values not returning these "empty" values is un-intuitive
                        None => None,
                        _ => Some(self),
                    },
                    _ => Some(self),
                },
                Field::JmesPath => match &self.blocks[6] {
                    Some(BlockKind::Jmes(ref j)) if !regex.pattern().is_match(j.as_ref()) => None,
                    _ => Some(self),
                },
            },
            None => Some(self),
        }
    }
}
