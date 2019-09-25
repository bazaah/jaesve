use {
    crate::{
        cli::ProgramArgs,
        models::{
            assets::Field,
            error::{ErrorKind, Result},
        },
    },
    std::{fmt::Display, sync::Arc},
};

pub trait Pointer<T: Into<PointerKind> = PointerKind> {
    fn clone_extend<D: Display + Into<PointerParts>>(&self, other: D) -> T;

    fn as_complete(&self) -> String;

    fn as_parts(&self) -> Result<&Vec<PointerParts>>;
}

#[derive(Debug, Clone)]
pub enum PointerKind {
    Simple(String),
    Complex(Complex),
}

impl PointerKind {
    pub fn new(opts: &ProgramArgs) -> Option<Self> {
        match (opts.should_calculate(Field::Pointer), opts.should_calculate(Field::JmesPath)) {
            (_, true) => Some(PointerKind::Complex(Complex::new())),
            (true, false) => Some(PointerKind::Simple(String::new())),
            (false, false) => None,
        }
    }
}

impl Pointer for PointerKind {
    fn clone_extend<D: Display + Into<PointerParts>>(&self, other: D) -> PointerKind {
        match self {
            Self::Simple(s) => Self::Simple(format!("{}/{}", s, other)),
            Self::Complex(c) => Self::Complex(c.internal_clone_extend(other)),
        }
    }

    fn as_complete(&self) -> String {
        match self {
            Self::Simple(s) => s.clone(),
            Self::Complex(c) => format!("{}", c),
        }
    }

    fn as_parts(&self) -> Result<&Vec<PointerParts>> {
        match self {
            Self::Simple(_) => Err(ErrorKind::Message(format!("yada"))),
            Self::Complex(c) => Ok(&c.inner),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Complex {
    inner: Vec<PointerParts>,
}

impl Complex {
    fn new() -> Self {
        Complex { inner: Vec::new() }
    }

    fn internal_clone_extend<P: Into<PointerParts>>(&self, item: P) -> Complex {
        let mut new = self.clone();
        new.inner
            .extend_from_slice(&[PointerParts::Slash, item.into()]);
        new
    }
}

impl Display for Complex {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for item in self.inner.iter() {
            write!(f, "{}", item)?
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum PointerParts {
    Slash,
    Object(Arc<str>),
    Array(usize),
}

impl From<&str> for PointerParts {
    fn from(s: &str) -> Self {
        PointerParts::Object(Arc::from(s))
    }
}

impl From<usize> for PointerParts {
    fn from(u: usize) -> Self {
        PointerParts::Array(u)
    }
}

impl Display for PointerParts {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Slash => write!(f, "/"),
            Self::Array(u) => write!(f, "{}", u),
            Self::Object(s) => write!(f, "{}", s),
        }
    }
}
