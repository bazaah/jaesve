use super::args::*;

/// Interface for for interacting with the individual arg representation
pub(in crate::cli) trait ConfigMerge {
    fn merge<T: ConfigMerge>(&mut self, other: T);

    fn priority_merge<T>(higher: &mut Option<T>, mut lower: Option<T>) {
        if higher.is_none() && lower.is_some() {
            *higher = lower.take()
        }
    }

    fn debug_level(&mut self) -> OptDebug {
        None
    }

    fn quiet(&mut self) -> OptQuiet {
        None
    }

    fn append(&mut self) -> OptAppend {
        None
    }

    fn line(&mut self) -> OptLine {
        None
    }

    fn delimiter(&mut self) -> OptDelim {
        None
    }

    fn guard(&mut self) -> OptGuard {
        None
    }

    fn format(&mut self) -> OptFormat {
        None
    }

    fn input_buffer_size(&mut self) -> OptBufIn {
        None
    }

    fn output_buffer_size(&mut self) -> OptBufOut {
        None
    }

    fn linereader_eol(&mut self) -> OptEOL {
        None
    }

    fn factor(&mut self) -> OptFactor {
        None
    }
}
