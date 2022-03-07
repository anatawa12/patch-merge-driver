use crate::patch::DiffParseError::{InvalidHank, InvalidHeader, TooManyHankLine, UnexpectedEof};
use crate::patch::Format::Unified;
use crate::patch::HankErrorKind::InvalidIndicator;
use crate::patch::{parse_int_pair, DiffParseError, PatchParser};
use crate::return_if_none;
use UnifiedHankLine::{Add, Common, Delete};

#[derive(Debug, Eq, PartialEq)]
pub(crate) struct UnifiedPatch<'a> {
    pub(crate) hanks: Vec<UnifiedHank<'a>>,
    pub(crate) tailing_comment: Vec<&'a str>,
}

#[derive(Debug, Eq, PartialEq)]
pub(crate) struct UnifiedHeader<'a> {
    pub(crate) line: &'a str,
    pub(crate) from_offset: usize,
    pub(crate) from_count: usize,
    pub(crate) to_offset: usize,
    pub(crate) to_count: usize,
    pub(crate) tailing: &'a str,
}

#[derive(Debug, Eq, PartialEq)]
pub(crate) struct UnifiedHank<'a> {
    pub(crate) comment: Vec<&'a str>,
    pub(crate) header: UnifiedHeader<'a>,
    pub(crate) lines: Vec<UnifiedHankLine<'a>>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) enum UnifiedHankLine<'a> {
    Delete(&'a str, &'a str),
    Add(&'a str, &'a str),
    Common(&'a str, &'a str),
}

impl<'a, I: Iterator<Item = &'a str>> PatchParser<'a, I> {
    /// parses unified patch. non-unified patch will be parsed as comment or returns error
    pub(crate) fn parse_unified(&mut self) -> Result<UnifiedPatch<'a>, DiffParseError> {
        self.parse_unified0(None)
    }

    pub(in super::super) fn parse_unified0(
        &mut self,
        first_comment: Option<Vec<&'a str>>,
    ) -> Result<UnifiedPatch<'a>, DiffParseError> {
        let mut hanks = vec![];

        if let Some(first_comment) = first_comment {
            hanks.push(self.parse_unified_hank(first_comment)?);
        }

        let mut comment: Vec<&'a str> = vec![];

        while let Some(line) = self.peek() {
            if line.starts_with("@@ -") {
                hanks.push(self.parse_unified_hank(comment)?);
                comment = vec![];
            } else {
                comment.push(line);
                self.next();
            }
        }

        Ok(UnifiedPatch {
            hanks,
            tailing_comment: comment,
        })
    }

    fn parse_unified_hank(
        &mut self,
        comment: Vec<&'a str>,
    ) -> Result<UnifiedHank<'a>, DiffParseError> {
        let header = parse_unified_header(self.next().expect("expected unified header"))?;
        let mut lines = vec![];
        let mut from_read: usize = 0;
        let mut to_read: usize = 0;

        loop {
            let some = return_if_none!(self.next(), Err(UnexpectedEof));
            if some.starts_with('-') {
                lines.push(Delete("-", some.split_at(1).1));
                from_read += 1;
            } else if some.starts_with('+') {
                lines.push(Add("+", some.split_at(1).1));
                to_read += 1;
            } else if some.starts_with(' ') {
                lines.push(Common(" ", some.split_at(1).1));
                from_read += 1;
                to_read += 1;
            } else if some.is_empty() {
                lines.push(Common("", ""));
                from_read += 1;
                to_read += 1;
            } else {
                return Err(InvalidHank(
                    Unified,
                    InvalidIndicator(some.chars().nth(0).unwrap().into()),
                ));
            }

            if from_read == header.from_count && to_read == header.to_count {
                break;
            }

            if from_read > header.from_count || to_read > header.to_count {
                return Err(TooManyHankLine);
            }
        }

        return Ok(UnifiedHank {
            comment,
            header,
            lines,
        });
    }
}

fn parse_unified_header(header: &str) -> Result<UnifiedHeader, DiffParseError> {
    let line = header;
    let header = header.strip_prefix("@@ -").ok_or(InvalidHeader(Unified))?;
    let (from_offset, from_count, header) =
        parse_int_pair(header, |_| 1).map_err(|_| InvalidHeader(Unified))?;
    let header = header.strip_prefix(' ').unwrap_or(header);
    let header = return_if_none!(header.strip_prefix('+'), Err(InvalidHeader(Unified)));
    let (to_offset, to_count, header) =
        parse_int_pair(header, |_| 1).map_err(|_| InvalidHeader(Unified))?;
    let header = header.strip_prefix(' ').unwrap_or(header);
    let header = return_if_none!(header.strip_prefix('@'), Err(InvalidHeader(Unified)));
    let header = header.strip_prefix("@").unwrap_or(header);
    let tailing = header;

    Ok(UnifiedHeader {
        line,
        from_offset,
        from_count,
        to_offset,
        to_count,
        tailing,
    })
}
