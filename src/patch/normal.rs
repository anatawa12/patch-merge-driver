use crate::patch::DiffParseError::{InvalidHank, InvalidHeader, UnexpectedEof};
use crate::patch::Format::Normal;
use crate::patch::HankErrorKind::InvalidIndicator;
use crate::patch::{is_ascii_digit, parse_int_pair, DiffParseError, PatchParser};
use crate::return_if_none;

#[derive(Debug, Eq, PartialEq)]
pub(crate) struct NormalPatch<'a> {
    pub(crate) hanks: Vec<NormalHank<'a>>,
    pub(crate) tailing_comment: Vec<&'a [u8]>,
}

#[derive(Debug, Eq, PartialEq)]
pub(crate) enum NormalHank<'a> {
    Add(NormalAddHank<'a>),
    Delete(NormalDeleteHank<'a>),
    Replace(NormalReplaceHank<'a>),
}

#[derive(Debug, Eq, PartialEq)]
pub(crate) struct NormalAddHank<'a> {
    pub(crate) comment: Vec<&'a [u8]>,
    pub(crate) header_line: &'a [u8],
    pub(crate) insert_to: usize,
    pub(crate) inserted_begin: usize,
    pub(crate) separator: Option<&'a [u8]>,
    pub(crate) lines: Vec<NormalHankLine<'a>>,
}

#[derive(Debug, Eq, PartialEq)]
pub(crate) struct NormalDeleteHank<'a> {
    pub(crate) comment: Vec<&'a [u8]>,
    pub(crate) header_line: &'a [u8],
    pub(crate) delete_begin: usize,
    pub(crate) deleted_at: usize,
    pub(crate) lines: Vec<NormalHankLine<'a>>,
    pub(crate) separator: Option<&'a [u8]>,
}

#[derive(Debug, Eq, PartialEq)]
pub(crate) struct NormalReplaceHank<'a> {
    pub(crate) comment: Vec<&'a [u8]>,
    pub(crate) header_line: &'a [u8],
    pub(crate) old_begin: usize,
    pub(crate) new_begin: usize,
    pub(crate) old_lines: Vec<NormalHankLine<'a>>,
    pub(crate) separator: &'a [u8],
    pub(crate) new_lines: Vec<NormalHankLine<'a>>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) struct NormalHankLine<'a>(pub(crate) &'a [u8], pub(crate) &'a [u8]);

impl<'a, I: Iterator<Item = &'a [u8]>> PatchParser<'a, I> {
    /// parses normal patch. non-normal patch will be parsed as comment or returns error
    pub(crate) fn parse_normal(&mut self) -> Result<NormalPatch<'a>, DiffParseError> {
        self.parse_normal0(None)
    }

    pub(in super::super) fn parse_normal0(
        &mut self,
        first_comment: Option<Vec<&'a [u8]>>,
    ) -> Result<NormalPatch<'a>, DiffParseError> {
        let mut hanks = vec![];

        if let Some(first_comment) = first_comment {
            hanks.push(self.parse_normal_hank(first_comment)?);
        }

        let mut comment: Vec<&'a [u8]> = vec![];

        while let Some(line) = self.peek() {
            if line.get(0).map(|&x| is_ascii_digit(x)).unwrap_or(false) {
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
        comment: Vec<&'a [u8]>,
    ) -> Result<NormalHank<'a>, DiffParseError> {
        let header = parse_normal_header(self.next().expect("expected normal header"))?;

        let hank = match header.cmd {
            Command::Addition => {
                let separator = self.parse_optional_separator();
                let lines = self.parse_hank_body(header.to, b'>')?;
                NormalHank::Add(NormalAddHank {
                    comment,
                    header_line: header.line,
                    insert_to: header.old.0,
                    inserted_begin: header.to.0,
                    separator,
                    lines,
                })
            }
            Command::Replace => {
                let old_lines = self.parse_hank_body(header.old, b'<')?;
                let separator = match self.peek() {
                    None => return Err(UnexpectedEof),
                    Some(line) if line.starts_with(b"---") => {
                        self.next();
                        line
                    }
                    Some(line) => {
                        return Err(InvalidHank(Normal, InvalidIndicator(line.to_owned())))
                    }
                };
                let new_lines = self.parse_hank_body(header.to, b'>')?;
                NormalHank::Replace(NormalReplaceHank {
                    comment,
                    header_line: header.line,
                    old_begin: header.old.0,
                    new_begin: header.to.0,
                    old_lines,
                    separator,
                    new_lines,
                })
            }
            Command::Deletion => {
                let separator = self.parse_optional_separator();
                let lines = self.parse_hank_body(header.old, b'<')?;
                NormalHank::Delete(NormalDeleteHank {
                    comment,
                    header_line: header.line,
                    delete_begin: header.old.0,
                    deleted_at: header.to.0,
                    lines,
                    separator,
                })
            }
        };

        return Ok(hank);
    }

    fn parse_optional_separator(&mut self) -> Option<&'a [u8]> {
        match self.peek() {
            None => None,
            Some(line) => {
                if line.starts_with(b"---") {
                    // if the line starts with b"---", the line is a separator.
                    self.next();
                    Some(line)
                } else {
                    // otherwise, separator is not required; assume as no lines
                    None
                }
            }
        }
    }

    fn parse_hank_body(
        &mut self,
        (begin, end): (usize, usize),
        char: u8,
    ) -> Result<Vec<NormalHankLine<'a>>, DiffParseError> {
        let mut lines = vec![];

        for _i in 0..=(end - begin) {
            let some = return_if_none!(self.next(), Err(UnexpectedEof));

            let (indicator, body) = match some {
                [b, b' ' | b'\t', ..] if *b == char => some.split_at(2),
                _ => return Err(InvalidHank(Normal, InvalidIndicator(some.into()))),
            };

            lines.push(NormalHankLine(indicator, body));
        }

        Ok(lines)
    }
}

fn parse_normal_header(header: &[u8]) -> Result<NormalHeader, DiffParseError> {
    let line = header;
    let (old_begin, old_end, header) =
        parse_int_pair(header, |x| x).ok_or(InvalidHeader(Normal))?;
    let cmd = match return_if_none!(header.get(0), Err(InvalidHeader(Normal))) {
        b'a' => Command::Addition,
        b'c' => Command::Replace,
        b'd' => Command::Deletion,
        _ => return Err(InvalidHeader(Normal)),
    };
    let header = &header[1..];
    let (new_begin, new_end, _header) =
        parse_int_pair(header, |x| x).ok_or(InvalidHeader(Normal))?;

    match cmd {
        Command::Addition => {
            if !(old_begin == old_end && new_begin <= new_end) {
                return Err(InvalidHeader(Normal));
            }
        }
        Command::Replace => {
            if !(old_begin <= old_end && new_begin <= new_end) {
                return Err(InvalidHeader(Normal));
            }
        }
        Command::Deletion => {
            if !(old_begin <= old_end && new_begin == new_end) {
                return Err(InvalidHeader(Normal));
            }
        }
    }

    Ok(NormalHeader {
        line,
        old: (old_begin, old_end),
        cmd,
        to: (new_begin, new_end),
    })
}

#[derive(Debug, Eq, PartialEq)]
struct NormalHeader<'a> {
    line: &'a [u8],
    old: (usize, usize),
    cmd: Command,
    to: (usize, usize),
}

#[derive(Debug, Eq, PartialEq)]
enum Command {
    Addition,
    Replace,
    Deletion,
}

#[test]
fn parse() {
    use NormalHankLine as NHL;
    assert_eq!(
        PatchParser::new(
            vec![
                b"1,2d0\n" as &[u8],
                b"< The Way that can be told of is not the eternal Way;\n",
                b"< The name that can be named is not the eternal name.\n",
                b"4c2,3\n",
                b"< The Named is the mother of all things.\n",
                b"---\n",
                b"> The named is the mother of all things.\n",
                b"> \n",
                b"11a11,13\n",
                b"> They both may be called deep and profound.\n",
                b"> Deeper and more profound,\n",
                b"> The door of all subtleties!\n",
                b"\n",
            ]
            .into_iter()
        )
        .parse_normal()
        .unwrap(),
        NormalPatch {
            hanks: vec![
                NormalHank::Delete(NormalDeleteHank {
                    comment: vec![],
                    header_line: b"1,2d0\n",
                    delete_begin: 1,
                    deleted_at: 0,
                    lines: vec![
                        NHL(
                            b"< ",
                            b"The Way that can be told of is not the eternal Way;\n"
                        ),
                        NHL(
                            b"< ",
                            b"The name that can be named is not the eternal name.\n"
                        ),
                    ],
                    separator: None,
                }),
                NormalHank::Replace(NormalReplaceHank {
                    comment: vec![],
                    header_line: b"4c2,3\n",
                    old_begin: 4,
                    new_begin: 2,
                    old_lines: vec![NHL(b"< ", b"The Named is the mother of all things.\n")],
                    separator: b"---\n",
                    new_lines: vec![
                        NHL(b"> ", b"The named is the mother of all things.\n"),
                        NHL(b"> ", b"\n"),
                    ]
                }),
                NormalHank::Add(NormalAddHank {
                    comment: vec![],
                    header_line: b"11a11,13\n",
                    insert_to: 11,
                    inserted_begin: 11,
                    separator: None,
                    lines: vec![
                        NHL(b"> ", b"They both may be called deep and profound.\n"),
                        NHL(b"> ", b"Deeper and more profound,\n"),
                        NHL(b"> ", b"The door of all subtleties!\n"),
                    ]
                }),
            ],
            tailing_comment: vec![b"\n"],
        }
    )
}

#[test]
fn parse_detect() {
    use NormalHankLine as NHL;
    assert_eq!(
        PatchParser::new(
            vec![
                b"1,2d0\n" as &[u8],
                b"< The Way that can be told of is not the eternal Way;\n",
                b"< The name that can be named is not the eternal name.\n",
                b"4c2,3\n",
                b"< The Named is the mother of all things.\n",
                b"---\n",
                b"> The named is the mother of all things.\n",
                b"> \n",
                b"11a11,13\n",
                b"> They both may be called deep and profound.\n",
                b"> Deeper and more profound,\n",
                b"> The door of all subtleties!\n",
                b"\n",
            ]
            .into_iter()
        )
        .parse()
        .unwrap(),
        super::Patch::Normal(NormalPatch {
            hanks: vec![
                NormalHank::Delete(NormalDeleteHank {
                    comment: vec![],
                    header_line: b"1,2d0\n",
                    delete_begin: 1,
                    deleted_at: 0,
                    lines: vec![
                        NHL(
                            b"< ",
                            b"The Way that can be told of is not the eternal Way;\n"
                        ),
                        NHL(
                            b"< ",
                            b"The name that can be named is not the eternal name.\n"
                        ),
                    ],
                    separator: None,
                }),
                NormalHank::Replace(NormalReplaceHank {
                    comment: vec![],
                    header_line: b"4c2,3\n",
                    old_begin: 4,
                    new_begin: 2,
                    old_lines: vec![NHL(b"< ", b"The Named is the mother of all things.\n"),],
                    separator: b"---\n",
                    new_lines: vec![
                        NHL(b"> ", b"The named is the mother of all things.\n"),
                        NHL(b"> ", b"\n"),
                    ]
                }),
                NormalHank::Add(NormalAddHank {
                    comment: vec![],
                    header_line: b"11a11,13\n",
                    insert_to: 11,
                    inserted_begin: 11,
                    separator: None,
                    lines: vec![
                        NHL(b"> ", b"They both may be called deep and profound.\n"),
                        NHL(b"> ", b"Deeper and more profound,\n"),
                        NHL(b"> ", b"The door of all subtleties!\n"),
                    ]
                }),
            ],
            tailing_comment: vec![b"\n"],
        })
    )
}
