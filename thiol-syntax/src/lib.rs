pub mod lexer;
pub mod parser;

pub mod ast;

pub type FileId = usize;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FileLocation {
    pub file: FileId,
    pub start: usize,
    pub end: usize,
}

impl FileLocation {
    pub fn merge(&self, other: FileLocation) -> Self {
        debug_assert_eq!(self.file, other.file);
        Self {
            file: self.file,
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Loc<T> {
    pub loc: FileLocation,
    pub value: T,
}

impl<T> Loc<T> {
    pub fn new(loc: FileLocation, value: T) -> Self {
        Self { loc, value }
    }
}

pub trait HasLoc {
    fn loc(&self) -> FileLocation;
}

impl<T> HasLoc for Loc<T> {
    fn loc(&self) -> FileLocation {
        self.loc
    }
}
