use crate::patch::DiffParseError::{InvalidHank, InvalidHeader, UnexpectedEof};
use crate::patch::Format::Context;
use crate::patch::HankErrorKind::{InvalidIndicator, NoHankBody};
use crate::patch::{parse_int_pair, DiffParseError, PatchParser};
use crate::util::SliceExt;
use std::hint::unreachable_unchecked;
use ContextHankLine::{AddDel, Common, Modified};

#[derive(Debug, Eq, PartialEq)]
pub(crate) struct ContextPatch<'a> {
    pub(crate) hanks: Vec<ContextHank<'a>>,
    pub(crate) tailing_comment: Vec<&'a str>,
}

#[derive(Debug, Eq, PartialEq)]
pub(crate) struct ContextHank<'a> {
    pub(crate) comment: Vec<&'a str>,
    pub(crate) stars_line: &'a str,
    pub(crate) from_header_line: &'a str,
    pub(crate) from_header: (usize, usize),
    /// empty vec if omitted
    pub(crate) from_lines: Vec<ContextHankLine<'a>>,
    pub(crate) to_header_line: &'a str,
    pub(crate) to_header: (usize, usize),
    /// empty vec if omitted
    pub(crate) to_lines: Vec<ContextHankLine<'a>>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) enum ContextHankLine<'a> {
    Modified(&'a str, &'a str),
    AddDel(&'a str, &'a str),
    Common(&'a str, &'a str),
}

impl<'a, I: Iterator<Item = &'a str>> PatchParser<'a, I> {
    pub(crate) fn parse_context(&mut self) -> Result<ContextPatch<'a>, DiffParseError> {
        self.parse_context0(None)
    }

    // TODO: check format of NEW context and OLD context.
    // I don't know which is already implemented
    /// parses normal patch. non-normal patch will be parsed as comment or returns error
    pub(in super::super) fn parse_context0(
        &mut self,
        first_info: Option<(Vec<&'a str>, &'a str)>,
    ) -> Result<ContextPatch<'a>, DiffParseError> {
        let mut hanks = vec![];

        if let Some((comment, stars)) = first_info {
            hanks.push(self.parse_context_hank(comment, stars)?);
        }

        let mut comment: Vec<&'a str> = vec![];

        let mut starts_last_line = false;

        while let Some(line) = self.peek() {
            if starts_last_line && line.starts_with("*** ") {
                let stars = unsafe { comment.pop().unwrap_unchecked() };
                hanks.push(self.parse_context_hank(comment, stars)?);
                comment = vec![];
            } else {
                comment.push(line);
                self.next();

                starts_last_line = line.starts_with("********");
            }
        }

        Ok(ContextPatch {
            hanks,
            tailing_comment: comment,
        })
    }

    fn parse_context_hank(
        &mut self,
        comment: Vec<&'a str>,
        stars_line: &'a str,
    ) -> Result<ContextHank<'a>, DiffParseError> {
        let (from_line, from_begin, from_end) =
            parse_context_header(self.next().expect("expected context header"))?;
        let from_lines = self.parse_context_hank_lines(b'-', from_begin, from_end)?;
        let (to_line, to_begin, to_end) =
            parse_context_header(self.next().expect("expected context header"))?;
        let to_lines = self.parse_context_hank_lines(b'+', to_begin, to_end)?;

        if from_lines.is_empty() && to_lines.is_empty() {
            return Err(InvalidHank(Context, NoHankBody));
        }

        return Ok(ContextHank {
            comment,
            stars_line,
            from_header_line: from_line,
            from_header: (from_begin, from_end),
            from_lines,
            to_header_line: to_line,
            to_header: (to_begin, to_end),
            to_lines,
        });
    }

    fn parse_context_hank_lines(
        &mut self,
        add_del: u8,
        begin: usize,
        end: usize,
    ) -> Result<Vec<ContextHankLine<'a>>, DiffParseError> {
        let line_count = end - begin + 1;
        let mut lines = vec![];

        for i in 0..line_count {
            let begin = i == 0;
            let line = match self.next() {
                None if begin => return Ok(vec![]),
                None => return Err(UnexpectedEof),
                Some(l) => l,
            };
            let line_full = line;
            let bytes = line.as_bytes();
            let first = bytes.get(0);
            let line = if first == Some(&add_del) || matches!(first, Some(b'!' | b' ')) {
                let sep = match bytes.get(1) {
                    Some(b' ' | b'\t') => 2,
                    None | Some(_) if begin => return break_on_begin(self, line_full),
                    None | Some(_) => 1,
                };
                unsafe { parse_line(line.get_unchecked(..sep), line.get_unchecked(sep..)) }
            } else if matches!(first, Some(b'\t')) {
                // '\t' -> assume indicator as ' ' -> common line
                unsafe { parse_line(line.get_unchecked(..1), line.get_unchecked(1..)) }
            } else if matches!(first, None) {
                // '' -> assume indicator as ' ' -> zero length
                parse_line("", "")
            } else {
                return if begin {
                    break_on_begin(self, line_full)
                } else {
                    Err(InvalidHank(
                        Context,
                        InvalidIndicator(line.chars().nth(0).unwrap().into()),
                    ))
                };
            };
            lines.push(line);
        }

        return Ok(lines);

        fn break_on_begin<'a, I: Iterator<Item = &'a str>>(
            this: &mut PatchParser<'a, I>,
            line: &'a str,
        ) -> Result<Vec<ContextHankLine<'a>>, DiffParseError> {
            assert!(this.prev.is_none());
            this.prev = Some(line);
            return Ok(vec![]);
        }
    }
}

fn parse_line<'a>(indicator: &'a str, body: &'a str) -> ContextHankLine<'a> {
    match indicator.as_bytes().get(0) {
        None | Some(b'\t' | b' ') => Common(indicator, body),
        Some(b'!') => Modified(indicator, body),
        Some(b'+' | b'-') => AddDel(indicator, body),
        _ => unsafe { unreachable_unchecked() },
    }
}

fn parse_context_header(header: &str) -> Result<(&str, usize, usize), DiffParseError> {
    let line = header;
    let header = header
        .strip_prefix_while(|x| !x.is_ascii_digit())
        .unwrap_or("");
    let (begin, end, _header) =
        parse_int_pair(header, |x| x).map_err(|_| InvalidHeader(Context))?;
    if begin > end {
        return Err(InvalidHeader(Context));
    }

    Ok((line, begin, end))
}

fn parse_context_hank_line(
    header: &str,
) -> (
    /* possible for beginning: */ bool,
    /* indicator: */ Option<char>,
    /* indicator: */ &str,
    /* body: */ &str,
) {
    let mut iter = header.char_indices();
    let indicator = iter.next().map(|(_, c)| c);

    let for_beginning: bool;
    let indicator_end: usize;

    let second = iter.next();
    if let Some((_, ' ' | '\t')) = second {
        // if the char after indicator is space
        for_beginning = true;
        indicator_end = iter.next().map(|(i, _)| i).unwrap_or(header.len());
    } else {
        // if not, this can be hank iff it's not first line
        for_beginning = false;
        indicator_end = second.map(|(i, _)| i).unwrap_or(header.len());
    };

    (
        for_beginning,
        indicator,
        &header[0..indicator_end],
        &header[indicator_end..],
    )
}

#[test]
fn parse() {
    assert_eq!(
        PatchParser::from_str(concat!(
            "*** lao	2002-02-21 23:30:39.942229878 -0800\n",
            "--- tzu	2002-02-21 23:30:50.442260588 -0800\n",
            "***************\n",
            "*** 1,5 ****\n",
            "- The Way that can be told of is not the eternal Way;\n",
            "- The name that can be named is not the eternal name.\n",
            "  The Nameless is the origin of Heaven and Earth;\n",
            "! The Named is the mother of all things.\n",
            "  Therefore let there always be non-being,\n",
            "--- 1,4 ----\n",
            "  The Nameless is the origin of Heaven and Earth;\n",
            "! The named is the mother of all things.\n",
            "!\n",
            "  Therefore let there always be non-being,\n",
            "***************\n",
            "*** 11 ****\n",
            "--- 10,13 ----\n",
            "    they have different names.\n",
            "+ They both may be called deep and profound.\n",
            "+ Deeper and more profound,\n",
            "+ The door of all subtleties!\n",
            "\n",
        ))
        .parse_context()
        .unwrap(),
        ContextPatch {
            hanks: vec![
                ContextHank {
                    comment: vec![
                        "*** lao	2002-02-21 23:30:39.942229878 -0800\n",
                        "--- tzu	2002-02-21 23:30:50.442260588 -0800\n",
                    ],
                    stars_line: "***************\n",
                    from_header_line: "*** 1,5 ****\n",
                    from_header: (1, 5),
                    from_lines: vec![
                        AddDel(
                            "- ",
                            "The Way that can be told of is not the eternal Way;\n"
                        ),
                        AddDel(
                            "- ",
                            "The name that can be named is not the eternal name.\n"
                        ),
                        Common("  ", "The Nameless is the origin of Heaven and Earth;\n"),
                        Modified("! ", "The Named is the mother of all things.\n"),
                        Common("  ", "Therefore let there always be non-being,\n"),
                    ],
                    to_header_line: "--- 1,4 ----\n",
                    to_header: (1, 4),
                    to_lines: vec![
                        Common("  ", "The Nameless is the origin of Heaven and Earth;\n"),
                        Modified("! ", "The named is the mother of all things.\n"),
                        Modified("!", "\n"),
                        Common("  ", "Therefore let there always be non-being,\n"),
                    ]
                },
                ContextHank {
                    comment: vec![],
                    stars_line: "***************\n",
                    from_header_line: "*** 11 ****\n",
                    from_header: (11, 11),
                    from_lines: vec![],
                    to_header_line: "--- 10,13 ----\n",
                    to_header: (10, 13),
                    to_lines: vec![
                        Common("  ", "  they have different names.\n"),
                        AddDel("+ ", "They both may be called deep and profound.\n"),
                        AddDel("+ ", "Deeper and more profound,\n"),
                        AddDel("+ ", "The door of all subtleties!\n"),
                    ]
                }
            ],
            tailing_comment: vec!["\n"],
        }
    )
}

#[test]
fn parse_detext() {
    assert_eq!(
        PatchParser::from_str(concat!(
            "*** lao	2002-02-21 23:30:39.942229878 -0800\n",
            "--- tzu	2002-02-21 23:30:50.442260588 -0800\n",
            "***************\n",
            "*** 1,5 ****\n",
            "- The Way that can be told of is not the eternal Way;\n",
            "- The name that can be named is not the eternal name.\n",
            "  The Nameless is the origin of Heaven and Earth;\n",
            "! The Named is the mother of all things.\n",
            "  Therefore let there always be non-being,\n",
            "--- 1,4 ----\n",
            "  The Nameless is the origin of Heaven and Earth;\n",
            "! The named is the mother of all things.\n",
            "!\n",
            "  Therefore let there always be non-being,\n",
            "***************\n",
            "*** 11 ****\n",
            "--- 10,13 ----\n",
            "    they have different names.\n",
            "+ They both may be called deep and profound.\n",
            "+ Deeper and more profound,\n",
            "+ The door of all subtleties!\n",
            "\n",
        ))
        .parse()
        .unwrap(),
        super::Patch::Context(ContextPatch {
            hanks: vec![
                ContextHank {
                    comment: vec![
                        "*** lao	2002-02-21 23:30:39.942229878 -0800\n",
                        "--- tzu	2002-02-21 23:30:50.442260588 -0800\n",
                    ],
                    stars_line: "***************\n",
                    from_header_line: "*** 1,5 ****\n",
                    from_header: (1, 5),
                    from_lines: vec![
                        AddDel(
                            "- ",
                            "The Way that can be told of is not the eternal Way;\n"
                        ),
                        AddDel(
                            "- ",
                            "The name that can be named is not the eternal name.\n"
                        ),
                        Common("  ", "The Nameless is the origin of Heaven and Earth;\n"),
                        Modified("! ", "The Named is the mother of all things.\n"),
                        Common("  ", "Therefore let there always be non-being,\n"),
                    ],
                    to_header_line: "--- 1,4 ----\n",
                    to_header: (1, 4),
                    to_lines: vec![
                        Common("  ", "The Nameless is the origin of Heaven and Earth;\n"),
                        Modified("! ", "The named is the mother of all things.\n"),
                        Modified("!", "\n"),
                        Common("  ", "Therefore let there always be non-being,\n"),
                    ]
                },
                ContextHank {
                    comment: vec![],
                    stars_line: "***************\n",
                    from_header_line: "*** 11 ****\n",
                    from_header: (11, 11),
                    from_lines: vec![],
                    to_header_line: "--- 10,13 ----\n",
                    to_header: (10, 13),
                    to_lines: vec![
                        Common("  ", "  they have different names.\n"),
                        AddDel("+ ", "They both may be called deep and profound.\n"),
                        AddDel("+ ", "Deeper and more profound,\n"),
                        AddDel("+ ", "The door of all subtleties!\n"),
                    ]
                }
            ],
            tailing_comment: vec!["\n"],
        })
    )
}
