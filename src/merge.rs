use crate::patch::Patch::{Comment, Context, Unified};
use crate::patch::{Patch, PatchParser};
use clap::Parser;
use memmap::Mmap;
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
        (Normal(original), Normal(current), Normal(others)) => {
            todo!("Normal implementation")
        }
        (Unified(original), Unified(current), Unified(others)) => {
            todo!("Unified implementation")
        }
        (Context(original), Context(current), Context(others)) => {
            todo!("Context implementation")
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
    to.seek(SeekFrom::Start(0));
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
