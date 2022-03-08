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
    pub(crate) separator: Option<&'a str>,
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

        let mut from_lines = vec![];
        if header.cmd != Command::Addition {
            for _i in 0..=(header.from_end - header.from_begin) {
                let some = return_if_none!(self.next(), Err(UnexpectedEof));

                let indicator_end = some
                    .char_indices()
                    .nth(2)
                    .map(|(i, _)| i)
                    .unwrap_or(some.len());
                let (indicator, body) = some.split_at(indicator_end);

                if !matches!(indicator, "< " | "<\t") {
                    return Err(InvalidHank(Normal, InvalidIndicator(indicator.into())));
                }

                from_lines.push(NormalHankLine(indicator, body));
            }
        }

        let separator = match self.peek() {
            None => {
                if header.cmd == Command::Replace {
                    // for replace, separator is required
                    return Err(UnexpectedEof);
                } else {
                    // otherwise, separator is not required; assume as no lines
                    None
                }
            }
            Some(line) => {
                if line.starts_with("---") {
                    // if the line starts with "---", the line is a separator.
                    self.next();
                    Some(line)
                } else if header.cmd == Command::Replace {
                    // for replace, separator is required
                    return Err(UnexpectedEof);
                } else {
                    // otherwise, separator is not required; assume as no lines
                    None
                }
            }
        };

        let mut to_lines = vec![];
        if header.cmd != Command::Deletion {
            for _i in 0..=(header.to_end - header.to_begin) {
                let some = return_if_none!(self.next(), Err(UnexpectedEof));

                let indicator_end = some
                    .char_indices()
                    .nth(2)
                    .map(|(i, _)| i)
                    .unwrap_or(some.len());
                let (indicator, body) = some.split_at(indicator_end);

                if !matches!(indicator, "> " | ">\t") {
                    return Err(InvalidHank(Normal, InvalidIndicator(indicator.into())));
                }

                to_lines.push(NormalHankLine(indicator, body));
            }
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
        from_begin,
        from_end,
        cmd,
        to_begin,
        to_end,
    })
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
                NormalHank {
                    comment: vec![],
                    header: NormalHeader {
                        line: "1,2d0\n",
                        from_begin: 1,
                        from_end: 2,
                        cmd: Command::Deletion,
                        to_begin: 0,
                        to_end: 0
                    },
                    from_lines: vec![
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
                    to_lines: vec![]
                },
                NormalHank {
                    comment: vec![],
                    header: NormalHeader {
                        line: "4c2,3\n",
                        from_begin: 4,
                        from_end: 4,
                        cmd: Command::Replace,
                        to_begin: 2,
                        to_end: 3,
                    },
                    from_lines: vec![NHL("< ", "The Named is the mother of all things.\n"),],
                    separator: Some("---\n"),
                    to_lines: vec![
                        NHL("> ", "The named is the mother of all things.\n"),
                        NHL("> ", "\n"),
                    ]
                },
                NormalHank {
                    comment: vec![],
                    header: NormalHeader {
                        line: "11a11,13\n",
                        from_begin: 11,
                        from_end: 11,
                        cmd: Command::Addition,
                        to_begin: 11,
                        to_end: 13,
                    },
                    from_lines: vec![],
                    separator: None,
                    to_lines: vec![
                        NHL("> ", "They both may be called deep and profound.\n"),
                        NHL("> ", "Deeper and more profound,\n"),
                        NHL("> ", "The door of all subtleties!\n"),
                    ]
                }
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
                NormalHank {
                    comment: vec![],
                    header: NormalHeader {
                        line: "1,2d0\n",
                        from_begin: 1,
                        from_end: 2,
                        cmd: Command::Deletion,
                        to_begin: 0,
                        to_end: 0
                    },
                    from_lines: vec![
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
                    to_lines: vec![]
                },
                NormalHank {
                    comment: vec![],
                    header: NormalHeader {
                        line: "4c2,3\n",
                        from_begin: 4,
                        from_end: 4,
                        cmd: Command::Replace,
                        to_begin: 2,
                        to_end: 3,
                    },
                    from_lines: vec![NHL("< ", "The Named is the mother of all things.\n"),],
                    separator: Some("---\n"),
                    to_lines: vec![
                        NHL("> ", "The named is the mother of all things.\n"),
                        NHL("> ", "\n"),
                    ]
                },
                NormalHank {
                    comment: vec![],
                    header: NormalHeader {
                        line: "11a11,13\n",
                        from_begin: 11,
                        from_end: 11,
                        cmd: Command::Addition,
                        to_begin: 11,
                        to_end: 13,
                    },
                    from_lines: vec![],
                    separator: None,
                    to_lines: vec![
                        NHL("> ", "They both may be called deep and profound.\n"),
                        NHL("> ", "Deeper and more profound,\n"),
                        NHL("> ", "The door of all subtleties!\n"),
                    ]
                }
            ],
            tailing_comment: vec!["\n"],
        })
    )
}
