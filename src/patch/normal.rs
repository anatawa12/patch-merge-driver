use crate::patch::DiffParseError::{InvalidHank, InvalidHeader, UnexpectedEof};
use crate::patch::Format::Normal;
use crate::patch::HankErrorKind::InvalidIndicator;
use crate::patch::{parse_int_pair, DiffParseError, PatchParser};
use crate::return_if_none;

#[derive(Debug, Eq, PartialEq)]
pub(crate) struct NormalPatch<'a> {
    pub(crate) hanks: Vec<NormalHank<'a>>,
    pub(crate) tailing_comment: Vec<&'a str>,
}

#[derive(Debug, Eq, PartialEq)]
pub(crate) struct NormalHank<'a> {
    pub(crate) comment: Vec<&'a str>,
    pub(crate) header: NormalHeader<'a>,
    pub(crate) from_lines: Vec<NormalHankLine<'a>>,
    pub(crate) separator: &'a str,
    pub(crate) to_lines: Vec<NormalHankLine<'a>>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct NormalHeader<'a> {
    line: &'a str,
    from_begin: usize,
    from_end: usize,
    cmd: Command,
    to_begin: usize,
    to_end: usize,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Command {
    Addition,
    Replace,
    Deletion,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) struct NormalHankLine<'a>(pub(crate) &'a str, pub(crate) &'a str);

impl<'a, I: Iterator<Item = &'a str>> PatchParser<'a, I> {
    /// parses normal patch. non-normal patch will be parsed as comment or returns error
    pub(crate) fn parse_normal(&mut self) -> Result<NormalPatch<'a>, DiffParseError> {
        self.parse_normal0(None)
    }

    pub(in super::super) fn parse_normal0(
        &mut self,
        first_comment: Option<Vec<&'a str>>,
    ) -> Result<NormalPatch<'a>, DiffParseError> {
        let mut hanks = vec![];

        if let Some(first_comment) = first_comment {
            hanks.push(self.parse_normal_hank(first_comment)?);
        }

        let mut comment: Vec<&'a str> = vec![];

        while let Some(line) = self.peek() {
            if line.starts_with("@@ -") {
                hanks.push(self.parse_normal_hank(comment)?);
                comment = vec![];
            } else {
                comment.push(line);
                self.next();
            }
        }

        Ok(NormalPatch {
            hanks,
            tailing_comment: comment,
        })
    }

    fn parse_normal_hank(
        &mut self,
        comment: Vec<&'a str>,
    ) -> Result<NormalHank<'a>, DiffParseError> {
        let header = parse_normal_header(self.next().expect("expected normal header"))?;

        let mut from_lines = vec![];
        for _i in 0..=(header.from_begin - header.from_end) {
            let some = return_if_none!(self.next(), Err(UnexpectedEof));

            let indicator_end = some
                .char_indices()
                .nth(1)
                .map(|(i, _)| i)
                .unwrap_or(some.len());
            let (indicator, body) = some.split_at(indicator_end);

            if !matches!(indicator, "< " | "<\t") {
                return Err(InvalidHank(Normal, InvalidIndicator(indicator.into())));
            }

            from_lines.push(NormalHankLine(indicator, body));
        }

        let separator = return_if_none!(self.next(), Err(UnexpectedEof));
        if !separator.starts_with("---") {
            return Err(InvalidHeader(Normal));
        }

        let mut to_lines = vec![];
        for _i in 0..=(header.to_begin - header.to_end) {
            let some = return_if_none!(self.next(), Err(UnexpectedEof));

            let indicator_end = some
                .char_indices()
                .nth(1)
                .map(|(i, _)| i)
                .unwrap_or(some.len());
            let (indicator, body) = some.split_at(indicator_end);

            if !matches!(indicator, "> " | ">\t") {
                return Err(InvalidHank(Normal, InvalidIndicator(indicator.into())));
            }

            to_lines.push(NormalHankLine(indicator, body));
        }

        return Ok(NormalHank {
            comment,
            header,
            from_lines,
            separator,
            to_lines,
        });
    }
}

fn parse_normal_header(header: &str) -> Result<NormalHeader, DiffParseError> {
    let line = header;
    let (from_begin, from_end, header) =
        parse_int_pair(header, |x| x).map_err(|_| InvalidHeader(Normal))?;
    let cmd = match return_if_none!(header.as_bytes().get(0), Err(InvalidHeader(Normal))) {
        b'a' => Command::Addition,
        b'c' => Command::Replace,
        b'd' => Command::Deletion,
        _ => return Err(InvalidHeader(Normal)),
    };
    let (to_begin, to_end, _header) =
        parse_int_pair(header, |x| x).map_err(|_| InvalidHeader(Normal))?;

    match cmd {
        Command::Addition => {
            if !(from_begin == from_end && to_begin <= to_end) {
                return Err(InvalidHeader(Normal));
            }
        }
        Command::Replace => {
            if !(from_begin <= from_end && to_begin <= to_end) {
                return Err(InvalidHeader(Normal));
            }
        }
        Command::Deletion => {
            if !(from_begin <= from_end && to_begin == to_end) {
                return Err(InvalidHeader(Normal));
            }
        }
    }

    Ok(NormalHeader {
        line,
        from_begin,
        from_end,
        cmd,
        to_begin,
        to_end,
    })
}
