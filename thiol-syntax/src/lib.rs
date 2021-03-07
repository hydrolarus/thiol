// SPDX-FileCopyrightText: 2021 The thiol developers
//
// SPDX-License-Identifier: EUPL-1.2

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
    pub fn range(&self) -> std::ops::Range<usize> {
        self.start..self.end
    }

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
