use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::process::exit;

pub(crate) trait PathExt {
    fn pushed(self, path: impl AsRef<Path>) -> Self;
    fn popped(self) -> Self;
}

impl PathExt for PathBuf {
    fn pushed(mut self, path: impl AsRef<Path>) -> Self {
        self.push(path);
        self
    }

    fn popped(mut self) -> Self {
        self.pop();
        self
    }
}

pub(crate) trait SliceExt {
    type Item;
    fn strip_prefix_while(&self, f: impl FnMut(&Self::Item) -> bool) -> Option<&Self>;
}

impl<T> SliceExt for [T] {
    type Item = T;

    fn strip_prefix_while(&self, mut f: impl FnMut(&Self::Item) -> bool) -> Option<&Self> {
        for (i, v) in self.iter().enumerate() {
            if !f(v) {
                return Some(&self[i..]);
            }
        }
        None
    }
}

impl SliceExt for str {
    type Item = char;

    fn strip_prefix_while(&self, mut f: impl FnMut(&Self::Item) -> bool) -> Option<&Self> {
        for (i, v) in self.char_indices() {
            if !f(&v) {
                return Some(&self[i..]);
            }
        }
        None
    }
}

pub(crate) trait ByteSliceOrStrExt {
    fn line_byte_slices_with_newline(&self) -> LineByteSlicesWithNewline;
}

impl ByteSliceOrStrExt for str {
    fn line_byte_slices_with_newline(&self) -> LineByteSlicesWithNewline {
        LineByteSlicesWithNewline { buf: self.as_bytes() }
    }
}

impl ByteSliceOrStrExt for [u8] {
    fn line_byte_slices_with_newline(&self) -> LineByteSlicesWithNewline {
        LineByteSlicesWithNewline { buf: self }
    }
}

pub(crate) struct LineByteSlicesWithNewline<'a> {
    buf: &'a [u8],
}

impl<'a> Iterator for LineByteSlicesWithNewline<'a> {
    type Item = &'a [u8];

    fn next(&mut self) -> Option<Self::Item> {
        match self.buf.into_iter().position(|&b| b == b'\n') {
            // if it reached end of text, returns none
            None if self.buf.is_empty() => None,
            None => {
                let buf = self.buf;
                self.buf = b"";
                Some(buf)
            }
            Some(i) => {
                // SAFETY: the '\n' is in one byte so +1 is safe
                let (line, rest) = self.buf.split_at(i + 1);
                self.buf = rest;
                Some(line)
            }
        }
    }
}

pub(crate) fn read_fully(f: &mut File, buf: &mut [u8]) -> std::io::Result<usize> {
    let mut start = 0;
    while start < buf.len() {
        let read = f.read(&mut buf[start..])?;
        if read == 0 {
            break;
        }
        start += read;
    }

    Ok(start)
}

pub(crate) fn die(message: &str) -> ! {
    eprintln!("merge-driver: {}", message);
    exit(1);
}

#[macro_export]
macro_rules! return_if_none {
    ($option: expr, $returns: expr) => {
        if let ::std::option::Option::Some(v) = $option {
            v
        } else {
            #[allow(unreachable_code)]
            return $returns;
        }
    };
}
