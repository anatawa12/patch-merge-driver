use crate::patch::Patch::{Comment, Context, Unified};
use crate::patch::{Patch, PatchFile, PatchHank, PatchParser};
use clap::Parser;
use memmap::Mmap;
use std::borrow::Cow;
use std::collections::HashMap;
use std::env::var_os;
use std::fs::{File, OpenOptions};
use std::io;
use std::io::{Seek, SeekFrom};
use std::path::{Path, PathBuf};
use std::process::Command;
use Patch::Normal;

#[derive(Parser)]
/// Merges two patch files into one
pub(crate) struct Options {
    /// the path to original, merge ancestor's version
    original: PathBuf,
    /// the path to current version
    current: PathBuf,
    /// the path to others version
    others: PathBuf,
    #[clap(default_value = "7")]
    conflict_marker_size: usize,
}

pub(crate) fn main(options: &Options) {
    let (original_file, original_m) =
        open_mmap(&options.original, false).expect("cannot open original file");
    let (current_file, current_m) =
        open_mmap(&options.current, true).expect("cannot open current file");
    let (others_file, others_m) =
        open_mmap(&options.others, false).expect("cannot open others file");

    let (original, current, others) = match eq_bytes(&original_m, &current_m, &others_m) {
        TEQ::AB => return copy_file(current_file, current_m, &others_m),
        TEQ::BC => return copy_file(current_file, current_m, &original_m),
        TEQ::CA => return copy_file(current_file, current_m, &original_m),
        TEQ::Al => return,
        TEQ::No => match (
            PatchParser::from_slice(&original_m).parse(),
            PatchParser::from_slice(&current_m).parse(),
            PatchParser::from_slice(&others_m).parse(),
        ) {
            (Ok(original), Ok(current), Ok(others)) => (original, current, others),
            (original, current, others) => {
                if let Some(err) = original.err() {
                    eprintln!("error parsing original file: {}", err);
                }
                if let Some(err) = current.err() {
                    eprintln!("error parsing current file:  {}", err);
                }
                if let Some(err) = others.err() {
                    eprintln!("error parsing others file:   {}", err);
                }
                eprintln!("error parsing input files. aborting and fall backing to git merge-file");

                call_git_merge_file(options);
            }
        },
    };
    // close fd
    drop((original_file, current_file, others_file));

    match (original, current, others) {
        (Normal(ref original), Normal(ref current), Normal(ref others)) => {
            do_merge(original, current, others)
            // TODO: write
        }
        (Unified(ref original), Unified(ref current), Unified(ref others)) => {
            do_merge(original, current, others)
            // TODO: write
        }
        (Context(ref original), Context(ref current), Context(ref others)) => {
            do_merge(original, current, others)
            // TODO: write
        }
        (Comment(_), Comment(_), Comment(_)) => {
            eprintln!("comment only found. fallback to git merge-file.");
            drop((original_m, current_m, others_m));
            call_git_merge_file(options)
        }
        (r, c, o) => {
            eprintln!("patch file format mismatch found. falling back to git merge-file");
            eprintln!(
                "original format: {}",
                r.format().map(|f| f.name()).unwrap_or("comment only")
            );
            eprintln!(
                "current  format: {}",
                c.format().map(|f| f.name()).unwrap_or("comment only")
            );
            eprintln!(
                "others   format: {}",
                o.format().map(|f| f.name()).unwrap_or("comment only")
            );
            call_git_merge_file(options)
        }
    }
}

// a: ancestor
// o: ours
// t: theirs

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
enum FileId<'a, 'b> {
    File(&'a [u8], &'a [u8]),
    Comment(&'b [&'a [u8]]),
}
type NamedHanks<'a, 'b, H> = (FileId<'a, 'b>, &'b [H]);

struct ThreeFile<'a, 'b, H: PatchHank<'a>> {
    id: FileId<'a, 'b>,
    a_hanks: Option<&'b [H]>,
    o_hanks: Option<&'b [H]>,
    t_hanks: Option<&'b [H]>,
}

impl<'a, 'b, H: PatchHank<'a>> ThreeFile<'a, 'b, H> {
    pub(crate) fn new_ours(named: NamedHanks<'a, 'b, H>) -> Self {
        ThreeFile {
            id: named.0,
            a_hanks: None,
            o_hanks: Some(named.1),
            t_hanks: None,
        }
    }

    pub(crate) fn new_theirs(named: NamedHanks<'a, 'b, H>) -> Self {
        ThreeFile {
            id: named.0,
            a_hanks: None,
            o_hanks: None,
            t_hanks: Some(named.1),
        }
    }

    pub(crate) fn new_theirs_ours(
        theirs: NamedHanks<'a, 'b, H>,
        ours: NamedHanks<'a, 'b, H>,
    ) -> Self {
        debug_assert_eq!(theirs.0, ours.0);
        ThreeFile {
            id: ours.0,
            a_hanks: None,
            o_hanks: Some(ours.1),
            t_hanks: Some(theirs.1),
        }
    }
}

fn do_merge<'a, P: PatchFile<'a>>(a_patch: &P, o_patch: &P, t_patch: &P) {
    let a_files = collect_hanks_by_file_name(a_patch);
    let o_files = collect_hanks_by_file_name(o_patch);
    let t_files = collect_hanks_by_file_name(t_patch);
    let a_files = a_files.as_slice();
    let mut o_files = o_files.as_slice();
    let mut t_files = t_files.as_slice();
    let mut elements = Vec::<ThreeFile<P::Hank>>::with_capacity(o_files.len());

    for (id, a_hanks) in a_files.iter() {
        let (o_skipped, o_hanks) = find_by_id(&mut o_files, *id);
        let (t_skipped, t_hanks) = find_by_id(&mut t_files, *id);

        // process skipped
        two_way_merge(o_skipped, t_skipped, &mut elements);

        elements.push(ThreeFile {
            id: *id,
            a_hanks: Some(a_hanks),
            o_hanks: o_hanks.map(|(_, a)| *a),
            t_hanks: t_hanks.map(|(_, a)| *a),
        })
    }

    // merge rest
    two_way_merge(o_files, t_files, &mut elements);

    elements
        .iter()
        .map(|file| match (file.a_hanks, file.o_hanks, file.t_hanks) {
            (Some(a_hanks), Some(b_hanks), None) | (Some(a_hanks), None, Some(b_hanks)) => {
                do_merge_file_old_new(a_hanks, b_hanks)
            }
            (Some(a_hanks), Some(o_hanks), Some(t_hanks)) => {
                do_merge_file_three_way(a_hanks, o_hanks, t_hanks)
            }
            (None, Some(o_hanks), Some(t_hanks)) => do_merge_file_two_way(o_hanks, t_hanks),
            (Some(hanks), None, None) | (None, Some(hanks), None) | (None, None, Some(hanks)) => {
                Ok(hanks.into())
            }
            (None, None, None) => unreachable!(),
        });
    todo!("do merge")
}

fn do_merge_file_old_new<'a, 'b, H: PatchHank<'a>>(
    old_hanks: &'b [H],
    new_hanks: &'b [H],
) -> Result<Cow<'b, [H]>, ()> {
    if is_there_contradiction(old_hanks, &[new_hanks]) {
        return Err(());
    }

    // if there's no contradiction, return new_hanks
    Ok(new_hanks.into())
}

fn do_merge_file_three_way<'a, 'b, H: PatchHank<'a>>(
    a_hanks: &'b [H],
    o_hanks: &'b [H],
    t_hanks: &'b [H],
) -> Result<Cow<'b, [H]>, ()> {
    if is_there_contradiction(a_hanks, &[o_hanks, t_hanks]) {
        return Err(());
    }

    todo!("three-way-merge")
}

fn do_merge_file_two_way<'a, 'b, H: PatchHank<'a>>(
    o_hanks: &'b [H],
    t_hanks: &'b [H],
) -> Result<Cow<'b, [H]>, ()> {
    if is_there_contradiction(o_hanks, &[t_hanks]) {
        return Err(());
    }

    let mut result_hanks: Vec<H> = Vec::new();

    let mut o_iter = o_hanks.iter().peekable();
    let mut t_iter = t_hanks.iter().peekable();

    while let (Some(&o_peek), Some(&t_peek)) = (o_iter.peek(), t_iter.peek()) {
        if o_peek.old_last_line_num() < t_peek.old_first_line_num() {
            result_hanks.push(o_peek.clone());
            o_iter.next();
        } else if t_peek.old_last_line_num() < o_peek.old_first_line_num() {
            result_hanks.push(t_peek.clone());
            t_iter.next();
        } else {
            todo!("merge hank")
        }
    }

    // append rest
    result_hanks.extend(o_iter.cloned());
    result_hanks.extend(t_iter.cloned());

    Ok(Cow::Owned(result_hanks))
}

fn is_there_contradiction<'a, 'b, H: PatchHank<'a>>(
    original: &'b [H],
    modified: &[&'b [H]],
) -> bool {
    // check there's no contradiction between original of both hank
    let mut original_lines = HashMap::<usize, &'a [u8]>::new();
    for h in original.iter() {
        let mut i = h.old_first_line_num();
        for old_line in h.old_lines() {
            original_lines.insert(i, old_line);
            i += 1;
        }
    }

    let original_lines = original_lines;

    for hanks in modified {
        for h in hanks.iter() {
            let mut i = h.old_first_line_num();
            for old_line in h.old_lines() {
                if let Some(&line) = original_lines.get(&i) {
                    if old_line != line {
                        return true;
                    }
                }
                i += 1;
            }
        }
    }

    false
}

fn two_way_merge<'a, 'b, H: PatchHank<'a>>(
    mut o_files: &[NamedHanks<'a, 'b, H>],
    mut t_files: &[NamedHanks<'a, 'b, H>],
    elements: &mut Vec<ThreeFile<'a, 'b, H>>,
) {
    while !o_files.is_empty() && !t_files.is_empty() {
        let o_first = o_files[0];
        o_files = &o_files[0..];
        let t_first = t_files[0];
        t_files = &t_files[0..];

        if o_first.0 == t_first.0 {
            elements.push(ThreeFile::new_theirs_ours(t_first, o_first));
        } else {
            match (
                find_by_id(&mut o_files, t_first.0),
                find_by_id(&mut t_files, o_first.0),
            ) {
                ((_, None), (_, None)) => {
                    elements.push(ThreeFile::new_theirs(t_first));
                    elements.push(ThreeFile::new_ours(o_first));
                }
                ((_, None), (t_skipped, Some(&t_found))) => {
                    elements.push(ThreeFile::new_theirs(t_first));
                    elements.extend(t_skipped.iter().copied().map(ThreeFile::new_theirs));
                    elements.push(ThreeFile::new_theirs_ours(t_found, o_first));
                }
                ((o_skipped, Some(&o_found)), (_, None)) => {
                    elements.push(ThreeFile::new_ours(o_first));
                    elements.extend(o_skipped.iter().copied().map(ThreeFile::new_ours));
                    elements.push(ThreeFile::new_theirs_ours(t_first, o_found));
                }
                ((o_skipped, Some(&o_found)), (t_skipped, Some(&t_found))) => {
                    if o_skipped.len() < t_skipped.len() {
                        elements.push(ThreeFile::new_ours(o_first));
                        elements.extend(o_skipped.iter().copied().map(ThreeFile::new_ours));
                        elements.push(ThreeFile::new_theirs_ours(t_first, o_found));
                        elements.extend(t_skipped.iter().copied().map(ThreeFile::new_theirs));
                        elements.push(ThreeFile::new_theirs(t_found));
                    } else {
                        elements.push(ThreeFile::new_theirs(t_first));
                        elements.extend(t_skipped.iter().copied().map(ThreeFile::new_theirs));
                        elements.push(ThreeFile::new_theirs_ours(t_found, o_first));
                        elements.extend(o_skipped.iter().copied().map(ThreeFile::new_ours));
                        elements.push(ThreeFile::new_ours(o_found));
                    }
                }
            }
        }
    }

    if !o_files.is_empty() {
        elements.extend(o_files.iter().copied().map(ThreeFile::new_ours));
    }

    if !t_files.is_empty() {
        elements.extend(t_files.iter().copied().map(ThreeFile::new_theirs));
    }
}

fn find_by_id<'a, 'b, 'c, H: PatchHank<'a>>(
    files: &mut &'c [NamedHanks<'a, 'b, H>],
    id: FileId,
) -> (
    &'c [NamedHanks<'a, 'b, H>],
    Option<&'c NamedHanks<'a, 'b, H>>,
) {
    match files.iter().position(|(fid, _)| fid == &id) {
        None => (&[], None),
        Some(idx) => {
            let to_process = &files[..idx];
            let element = &files[idx];
            *files = &files[idx + 1..];
            (to_process, Some(element))
        }
    }
}

fn collect_hanks_by_file_name<'a, P: PatchFile<'a>>(p: &P) -> Vec<NamedHanks<'a, '_, P::Hank>> {
    let mut hanks = p.hanks();
    let mut files = Vec::new();

    while let Some(i) = hanks.iter().position(|h| !h.comment().is_empty()) {
        append_to_files(&mut files, &hanks[..i]);
        hanks = &hanks[i..];
    }

    append_to_files(&mut files, hanks);

    return files;

    #[inline(always)]
    fn append_to_files<'a, 'b, H: PatchHank<'a>>(
        files: &mut Vec<NamedHanks<'a, 'b, H>>,
        hanks: &'b [H],
    ) {
        if !hanks.is_empty() {
            let names = match (hanks[0].old_name(), hanks[0].new_name()) {
                (Some(old), Some(new)) => FileId::File(old, new),
                _ => FileId::Comment(hanks[0].comment()),
            };
            files.push((names, hanks));
        }
    }
}

fn call_git_merge_file(options: &Options) -> ! {
    let mut command = match var_os("GIT_EXEC_PATH") {
        Some(exec_path) => {
            let mut exec_path = PathBuf::from(exec_path);
            exec_path.push("git-merge-file");
            Command::new(exec_path)
        }
        None => {
            let mut cmd = Command::new("git");
            cmd.arg("merge-file");
            cmd
        }
    };
    command.arg("-L").arg("ours");
    command.arg("-L").arg("base");
    command.arg("-L").arg("theirs");
    command.arg("--diff3");
    command.arg(format!("--marker-size={}", options.conflict_marker_size));
    command.arg(&options.current);
    command.arg(&options.original);
    command.arg(&options.others);

    #[cfg(unix)]
    fn start(cmd: &mut Command) -> ! {
        use std::os::unix::process::CommandExt;
        panic!("can't launch git merge-file: {}", cmd.exec());
    }

    #[cfg(windows)]
    fn start(cmd: &mut Command) -> ! {
        let s = cmd
            .spawn()
            .and_then(|mut x| x.wait())
            .expect("Can't launch git merge-file");
        exit(s.code().unwrap());
    }

    start(&mut command);
}

fn copy_file(mut to: File, map: Mmap, body: &[u8]) {
    drop(map);
    to.set_len(body.len() as u64).expect("can't write");
    to.seek(SeekFrom::Start(0)).expect("can't write");
    io::copy(&mut &*body, &mut to).expect("can't write");
}

fn open_mmap(path: &Path, write: bool) -> io::Result<(File, Mmap)> {
    let file = OpenOptions::new().read(true).write(write).open(&path)?;
    let mapped = unsafe { Mmap::map(&file)? };
    Ok((file, mapped))
}

fn eq_bytes(a: &[u8], b: &[u8], c: &[u8]) -> TEQ {
    TEQ::cmp(a, b, c)
}

#[repr(u8)]
#[allow(dead_code)]
enum TEQ {
    AB = 0b100,
    BC = 0b010,
    CA = 0b001,
    Al = 0b111,
    No = 0b000,
}

impl TEQ {
    #[inline]
    pub(super) fn cmp<T: Eq>(a: T, b: T, c: T) -> TEQ {
        let b: u8 = (if a == b { 0b100 } else { 0b000 })
            | (if b == c { 0b010 } else { 0b000 })
            | (if c == a { 0b001 } else { 0b000 });
        unsafe { std::mem::transmute(b) }
    }
}
