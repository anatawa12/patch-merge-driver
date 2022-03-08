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
pub(crate) enum NormalHank<'a> {
    Add(NormalAddHank<'a>),
    Delete(NormalDeleteHank<'a>),
    Replace(NormalReplaceHank<'a>),
}

#[derive(Debug, Eq, PartialEq)]
pub(crate) struct NormalAddHank<'a> {
    pub(crate) comment: Vec<&'a str>,
    pub(crate) header_line: &'a str,
    pub(crate) insert_to: usize,
    pub(crate) inserted_begin: usize,
    pub(crate) separator: Option<&'a str>,
    pub(crate) lines: Vec<NormalHankLine<'a>>,
}

#[derive(Debug, Eq, PartialEq)]
pub(crate) struct NormalDeleteHank<'a> {
    pub(crate) comment: Vec<&'a str>,
    pub(crate) header_line: &'a str,
    pub(crate) delete_begin: usize,
    pub(crate) deleted_at: usize,
    pub(crate) lines: Vec<NormalHankLine<'a>>,
    pub(crate) separator: Option<&'a str>,
}

#[derive(Debug, Eq, PartialEq)]
pub(crate) struct NormalReplaceHank<'a> {
    pub(crate) comment: Vec<&'a str>,
    pub(crate) header_line: &'a str,
    pub(crate) from_begin: usize,
    pub(crate) to_begin: usize,
    pub(crate) from_lines: Vec<NormalHankLine<'a>>,
    pub(crate) separator: &'a str,
    pub(crate) to_lines: Vec<NormalHankLine<'a>>,
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
            if line
                .chars()
                .nth(0)
                .map(|x| x.is_ascii_digit())
                .unwrap_or(false)
            {
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

        let hank = match header.cmd {
            Command::Addition => {
                let separator = self.parse_optional_separator();
                let lines = self.parse_hank_body(header.to, b'>')?;
                NormalHank::Add(NormalAddHank {
                    comment,
                    header_line: header.line,
                    insert_to: header.from.0,
                    inserted_begin: header.to.0,
                    separator,
                    lines,
                })
            }
            Command::Replace => {
                let from_lines = self.parse_hank_body(header.from, b'<')?;
                let separator = match self.peek() {
                    None => return Err(UnexpectedEof),
                    Some(line) if line.starts_with("---") => {
                        self.next();
                        line
                    }
                    Some(line) => {
                        return Err(InvalidHank(Normal, InvalidIndicator(line.to_string())))
                    }
                };
                let to_lines = self.parse_hank_body(header.to, b'>')?;
                NormalHank::Replace(NormalReplaceHank {
                    comment,
                    header_line: header.line,
                    from_begin: header.from.0,
                    to_begin: header.to.0,
                    from_lines,
                    separator,
                    to_lines,
                })
            }
            Command::Deletion => {
                let separator = self.parse_optional_separator();
                let lines = self.parse_hank_body(header.from, b'<')?;
                NormalHank::Delete(NormalDeleteHank {
                    comment,
                    header_line: header.line,
                    delete_begin: header.from.0,
                    deleted_at: header.to.0,
                    lines,
                    separator,
                })
            }
        };

        return Ok(hank);
    }

    fn parse_optional_separator(&mut self) -> Option<&'a str> {
        match self.peek() {
            None => None,
            Some(line) => {
                if line.starts_with("---") {
                    // if the line starts with "---", the line is a separator.
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

            let (indicator, body) = match some.as_bytes() {
                [b, b' ' | b'\t', ..] if *b == char => some.split_at(2),
                _ => return Err(InvalidHank(Normal, InvalidIndicator(some.into()))),
            };

            lines.push(NormalHankLine(indicator, body));
        }

        Ok(lines)
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
    let header = &header[1..];
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
        from: (from_begin, from_end),
        cmd,
        to: (to_begin, to_end),
    })
}

#[derive(Debug, Eq, PartialEq)]
struct NormalHeader<'a> {
    line: &'a str,
    from: (usize, usize),
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
                "1,2d0\n",
                "< The Way that can be told of is not the eternal Way;\n",
                "< The name that can be named is not the eternal name.\n",
                "4c2,3\n",
                "< The Named is the mother of all things.\n",
                "---\n",
                "> The named is the mother of all things.\n",
                "> \n",
                "11a11,13\n",
                "> They both may be called deep and profound.\n",
                "> Deeper and more profound,\n",
                "> The door of all subtleties!\n",
                "\n",
            ]
            .into_iter()
        )
        .parse_normal()
        .unwrap(),
        NormalPatch {
            hanks: vec![
                NormalHank::Delete(NormalDeleteHank {
                    comment: vec![],
                    header_line: "1,2d0\n",
                    delete_begin: 1,
                    deleted_at: 0,
                    lines: vec![
                        NHL(
                            "< ",
                            "The Way that can be told of is not the eternal Way;\n"
                        ),
                        NHL(
                            "< ",
                            "The name that can be named is not the eternal name.\n"
                        ),
                    ],
                    separator: None,
                }),
                NormalHank::Replace(NormalReplaceHank {
                    comment: vec![],
                    header_line: "4c2,3\n",
                    from_begin: 4,
                    to_begin: 2,
                    from_lines: vec![NHL("< ", "The Named is the mother of all things.\n")],
                    separator: "---\n",
                    to_lines: vec![
                        NHL("> ", "The named is the mother of all things.\n"),
                        NHL("> ", "\n"),
                    ]
                }),
                NormalHank::Add(NormalAddHank {
                    comment: vec![],
                    header_line: "11a11,13\n",
                    insert_to: 11,
                    inserted_begin: 11,
                    separator: None,
                    lines: vec![
                        NHL("> ", "They both may be called deep and profound.\n"),
                        NHL("> ", "Deeper and more profound,\n"),
                        NHL("> ", "The door of all subtleties!\n"),
                    ]
                }),
            ],
            tailing_comment: vec!["\n"],
        }
    )
}

#[test]
fn parse_detect() {
    use NormalHankLine as NHL;
    assert_eq!(
        PatchParser::new(
            vec![
                "1,2d0\n",
                "< The Way that can be told of is not the eternal Way;\n",
                "< The name that can be named is not the eternal name.\n",
                "4c2,3\n",
                "< The Named is the mother of all things.\n",
                "---\n",
                "> The named is the mother of all things.\n",
                "> \n",
                "11a11,13\n",
                "> They both may be called deep and profound.\n",
                "> Deeper and more profound,\n",
                "> The door of all subtleties!\n",
                "\n",
            ]
            .into_iter()
        )
        .parse()
        .unwrap(),
        super::Patch::Normal(NormalPatch {
            hanks: vec![
                NormalHank::Delete(NormalDeleteHank {
                    comment: vec![],
                    header_line: "1,2d0\n",
                    delete_begin: 1,
                    deleted_at: 0,
                    lines: vec![
                        NHL(
                            "< ",
                            "The Way that can be told of is not the eternal Way;\n"
                        ),
                        NHL(
                            "< ",
                            "The name that can be named is not the eternal name.\n"
                        ),
                    ],
                    separator: None,
                }),
                NormalHank::Replace(NormalReplaceHank {
                    comment: vec![],
                    header_line: "4c2,3\n",
                    from_begin: 4,
                    to_begin: 2,
                    from_lines: vec![NHL("< ", "The Named is the mother of all things.\n"),],
                    separator: "---\n",
                    to_lines: vec![
                        NHL("> ", "The named is the mother of all things.\n"),
                        NHL("> ", "\n"),
                    ]
                }),
                NormalHank::Add(NormalAddHank {
                    comment: vec![],
                    header_line: "11a11,13\n",
                    insert_to: 11,
                    inserted_begin: 11,
                    separator: None,
                    lines: vec![
                        NHL("> ", "They both may be called deep and profound.\n"),
                        NHL("> ", "Deeper and more profound,\n"),
                        NHL("> ", "The door of all subtleties!\n"),
                    ]
                }),
            ],
            tailing_comment: vec!["\n"],
        })
    )
}
