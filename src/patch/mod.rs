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
use crate::util::{LinesWithNewline, StrExt};
use std::str::FromStr;
use std::{fmt, io};
use DiffParseError::*;
use Format::*;
use HankErrorKind::NoHankBody;

pub(crate) struct PatchParser<'a, I: Iterator<Item = &'a str>> {
    iter: I,
    prev: Option<&'a str>,
}

impl<'a> PatchParser<'a, LinesWithNewline<'a>> {
    pub(crate) fn from_str(str: &'a str) -> Self {
        Self {
            iter: str.lines_with_newline(),
            prev: None,
        }
    }
}

impl<'a, I: Iterator<Item = &'a str>> PatchParser<'a, I> {
    pub(crate) fn new(iter: I) -> Self {
        Self { iter, prev: None }
    }

    pub(super) fn peek(&mut self) -> Option<&'a str> {
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

    pub(super) fn next(&mut self) -> Option<&'a str> {
        match self.prev.take() {
            Some(v) => Some(v),
            None => match self.iter.next() {
                None => None,
                Some(v) => Some(v),
            },
        }
    }

    /// parses patch file which does not know which format the patch file is
    pub(crate) fn parse(&mut self) -> Result<Patch, DiffParseError> {
        let mut comment: Vec<&'a str> = vec![];
        let mut starts_last_line = false;
        while let Some(line) = self.peek() {
            if line.starts_with("@@ -") {
                return Ok(Patch::Unified(self.parse_unified0(Some(comment))?));
            }
            if starts_last_line && line.starts_with("*** ") {
                let stars = unsafe { comment.pop().unwrap_unchecked() };
                return Ok(Patch::Context(self.parse_context0(Some((comment, stars)))?));
            }
            if line.starts_with(|c| char::is_ascii_digit(&c)) {
                return Ok(Patch::Normal(self.parse_normal0(Some(comment))?));
            }

            comment.push(line);
            self.next();

            starts_last_line = line.starts_with("********");
        }

        Ok(Patch::CommentOnly(comment))
    }
}

fn scan_int<T: FromStr>(v: &str) -> Result<(T, &str), T::Err> {
    let end_int = v.find(|x| !char::is_ascii_digit(&x)).unwrap_or(v.len());
    let (number, tail) = v.split_at(end_int);
    Ok((T::from_str(number)?, tail))
}

pub(crate) enum Patch<'a> {
    Unified(UnifiedPatch<'a>),
    Context(ContextPatch<'a>),
    Normal(NormalPatch<'a>),
    //fallback
    CommentOnly(Vec<&'a str>),
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
    InvalidIndicator(String),
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
                write!(fmt, "invalid {} hank indicator: '{}'", f.name(), s)
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
    header: &str,
    after_default: impl FnOnce(usize) -> T,
) -> Result<(usize, T, &str), std::num::ParseIntError> {
    let (first, header) = scan_int::<usize>(header)?;
    let (second, header) = match header.strip_prefix(",") {
        None => (after_default(first), header),
        Some(header) => {
            let (second, header) = scan_int::<usize>(header)?;
            (second.into(), header)
        }
    };
    Ok((first, second, header))
}
