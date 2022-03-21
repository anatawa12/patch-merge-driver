//! Patch parser module
//!
//! The module to parse patch file.
//! This was written with reference to the GNU patch, but there are many differences.

mod context;
mod normal;
mod unified;

use crate::patch::context::ContextPatch;
use crate::patch::normal::NormalPatch;
use crate::patch::unified::UnifiedPatch;
use crate::patch::HankErrorKind::{EmptyLine, InvalidIndicator};
use crate::util::{ByteSliceOrStrExt, LineByteSlicesWithNewline};
use std::str::FromStr;
use std::{fmt, io, str};
use DiffParseError::*;
use Format::*;
use HankErrorKind::NoHankBody;

pub(crate) struct PatchParser<'a, I: Iterator<Item = &'a [u8]>> {
    iter: I,
    prev: Option<&'a [u8]>,
}

impl<'a> PatchParser<'a, LineByteSlicesWithNewline<'a>> {
    pub(crate) fn from_slice(slice: &'a [u8]) -> Self {
        Self::new(slice.line_byte_slices_with_newline())
    }

    pub(crate) fn from_str(slice: &'a str) -> Self {
        Self::new(slice.line_byte_slices_with_newline())
    }
}

impl<'a, I: Iterator<Item = &'a [u8]>> PatchParser<'a, I> {
    pub(crate) fn new(iter: I) -> Self {
        Self { iter, prev: None }
    }

    pub(super) fn peek(&mut self) -> Option<&'a [u8]> {
        match self.prev {
            Some(ref v) => Some(v),
            None => match self.iter.next() {
                None => None,
                Some(v) => {
                    self.prev = Some(v);

                    match self.prev {
                        Some(ref v) => Some(v),
                        None => unsafe { std::hint::unreachable_unchecked() },
                    }
                }
            },
        }
    }

    pub(super) fn next(&mut self) -> Option<&'a [u8]> {
        match self.prev.take() {
            Some(v) => Some(v),
            None => match self.iter.next() {
                None => None,
                Some(v) => Some(v),
            },
        }
    }

    /// parses patch file which does not know which format the patch file is
    pub(crate) fn parse(&mut self) -> Result<Patch<'a>, DiffParseError> {
        let mut comment: Vec<&'a [u8]> = vec![];
        let mut starts_last_line = false;
        while let Some(line) = self.peek() {
            if line.starts_with(b"@@ -") {
                return Ok(Patch::Unified(self.parse_unified0(Some(comment))?));
            }
            if starts_last_line && line.starts_with(b"*** ") {
                let stars = unsafe { comment.pop().unwrap_unchecked() };
                return Ok(Patch::Context(self.parse_context0(Some((comment, stars)))?));
            }
            if line.get(0).map(|&b| is_ascii_digit(b)).unwrap_or(false) {
                return Ok(Patch::Normal(self.parse_normal0(Some(comment))?));
            }

            comment.push(line);
            self.next();

            starts_last_line = line.starts_with(b"********");
        }

        Ok(Patch::Comment(comment))
    }
}

fn scan_int<T: FromStr>(v: &[u8]) -> Option<(T, &[u8])> {
    let end_int = v
        .into_iter()
        .position(|&x| !is_ascii_digit(x))
        .unwrap_or(v.len());
    let (number, tail) = v.split_at(end_int);
    let number = std::str::from_utf8(number).ok()?;
    Some((T::from_str(number).ok()?, tail))
}

fn is_ascii_digit(b: u8) -> bool {
    char::is_ascii_digit(&(b as char))
}

#[derive(Eq, PartialEq, Debug)]
pub(crate) enum Patch<'a> {
    Unified(UnifiedPatch<'a>),
    Context(ContextPatch<'a>),
    Normal(NormalPatch<'a>),
    //fallback
    Comment(Vec<&'a [u8]>),
}

impl<'a> Patch<'a> {
    pub(crate) fn format(&self) -> Option<Format> {
        match self {
            Patch::Unified(_) => Some(Format::Unified),
            Patch::Context(_) => Some(Format::Context),
            Patch::Normal(_) => Some(Format::Normal),
            Patch::Comment(_) => None,
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub(crate) enum Format {
    Unified,
    Context,
    Normal,
}

impl Format {
    pub fn name(self) -> &'static str {
        match self {
            Unified => "unified",
            Format::Context => "context",
            Format::Normal => "normal",
        }
    }
}

#[derive(Debug)]
pub(crate) enum HankErrorKind {
    InvalidIndicator(Vec<u8>),
    EmptyLine,
    NoHankBody,
}

#[derive(Debug)]
pub(crate) enum DiffParseError {
    InvalidHeader(Format), // TODO: more error kinds for header
    InvalidHank(Format, HankErrorKind),
    TooManyHankLine,
    UnexpectedEof,
}

impl std::error::Error for DiffParseError {}

impl fmt::Display for DiffParseError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InvalidHeader(f) => write!(fmt, "invalid {} header", f.name()),
            InvalidHank(f, InvalidIndicator(s)) => {
                if let Some(s) = str::from_utf8(s).ok() {
                    write!(fmt, "invalid {} hank indicator: '{}'", f.name(), s)
                } else {
                    write!(fmt, "invalid {} hank indicator: '{:?}'", f.name(), s)
                }
            }
            InvalidHank(f, EmptyLine) => {
                write!(fmt, "invalid {} hank: empty line", f.name())
            }
            InvalidHank(f, NoHankBody) => write!(fmt, "no {} hank body found", f.name()),
            TooManyHankLine => fmt.write_str("too many hank found"),
            UnexpectedEof => fmt.write_str("unexpected EOF"),
        }
    }
}

impl From<DiffParseError> for io::Error {
    fn from(v: DiffParseError) -> Self {
        match v {
            e @ InvalidHeader(_) => io::Error::new(io::ErrorKind::InvalidData, e),
            e @ InvalidHank(_, _) => io::Error::new(io::ErrorKind::InvalidData, e),
            e @ TooManyHankLine => io::Error::new(io::ErrorKind::InvalidData, e),
            UnexpectedEof => io::Error::new(io::ErrorKind::UnexpectedEof, "Unexpected EOF"),
        }
    }
}

fn parse_int_pair<T: From<usize>>(
    header: &[u8],
    after_default: impl FnOnce(usize) -> T,
) -> Option<(usize, T, &[u8])> {
    let (first, header) = scan_int::<usize>(header)?;
    let (second, header) = match header.strip_prefix(b",") {
        None => (after_default(first), header),
        Some(header) => {
            let (second, header) = scan_int::<usize>(header)?;
            (second.into(), header)
        }
    };
    Some((first, second, header))
}

pub(crate) trait PatchFile<'a> {
    type Hank: PatchHank<'a>;

    fn hanks(&self) -> &[Self::Hank];
    fn tailing_comments(&self) -> &[&'a [u8]];
}

// 実装方針
// git diffはindexでコンフリクトが生成されるようにする。
// *** a/main1.rs	Sat Mar  5 12:49:50 2022
//
pub(crate) trait PatchHank<'a>: Clone {
    type OldLinesIter: Iterator<Item = &'a [u8]>;
    type NewLinesIter: Iterator<Item = &'a [u8]>;

    fn comment(&self) -> &[&'a [u8]];
    fn old_name(&self) -> Option<&'a [u8]>;
    fn new_name(&self) -> Option<&'a [u8]>;

    fn old_first_line_num(&self) -> usize;
    fn old_last_line_num(&self) -> usize;
    fn old_lines(&self) -> Self::OldLinesIter;
    fn new_first_line_num(&self) -> usize;
    fn new_last_line_num(&self) -> usize;
    fn new_lines(&self) -> Self::NewLinesIter;
}

fn find_name<'a>(comment: &[&'a [u8]], heading: &[u8]) -> Option<&'a [u8]> {
    comment.iter().find_map(|line| {
        let line = line.strip_prefix(heading)?;
        let line = line
            .iter()
            .enumerate()
            .rfind(|(_, &b)| b == b'\t')
            .map(|(i, _)| &line[..i])
            .unwrap_or(line);
        Some(line)
    })
}
