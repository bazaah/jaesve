use crate::models::{
    block::{Delimiter, Guard},
    error::Result,
    field::Field,
};

pub(in crate::cli) trait ConfigMerge {
    fn merge<T: ConfigMerge>(&mut self, other: T);

    fn priority_merge<T>(higher: &mut Option<T>, mut lower: Option<T>) {
        if higher.is_none() && lower.is_some() {
            *higher = lower.take()
        }
    }

    fn debug_level(&mut self) -> Option<usize> {
        None
    }

    fn quiet(&mut self) -> Option<bool> {
        None
    }

    fn append(&mut self) -> Option<bool> {
        None
    }

    fn line(&mut self) -> Option<usize> {
        None
    }

    fn delimiter(&mut self) -> Option<Delimiter> {
        None
    }

    fn guard(&mut self) -> Option<Guard> {
        None
    }

    fn format(&mut self) -> Option<Result<Vec<Field>>> {
        None
    }

    fn input_buffer_size(&mut self) -> Option<usize> {
        None
    }

    fn output_buffer_size(&mut self) -> Option<usize> {
        None
    }

    fn linereader_eol(&mut self) -> Option<char> {
        None
    }

    fn factor(&mut self) -> Option<usize> {
        None
    }
}
