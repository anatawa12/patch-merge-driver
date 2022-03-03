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
