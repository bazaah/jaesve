use crate::models::{
    assets::{Delimiter, Field, JType, RegexOptions},
    error::ErrorKind,
};

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
