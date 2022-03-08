use crate::path::xdg_config_home_for;
use crate::return_if_none;
use crate::util::{die, read_fully, PathExt, SliceExt};
use clap::{ArgGroup, Parser};
use std::borrow::Cow;
use std::env::{current_dir, current_exe, var_os};
use std::fs::{create_dir_all, File, OpenOptions};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;

#[derive(Parser)]
#[clap(group(
    ArgGroup::new("config-target")
        .required(true)
        .args(&[/*"system", */"global", "local", "config-file"]),
))]
#[clap(group(
    ArgGroup::new("attr-target")
        .args(&["repository", "attr-file"]),
))]
/// Installs patch-merge-driver to your git
pub(crate) struct Options {
    // config target flags
    // TODO: support system wide
    /// install this merge driver globally, for your user
    #[clap(long)]
    global: bool,
    /// install this merge driver locally, for your repository.
    #[clap(long)]
    local: bool,
    /// Write git config to the specified file.
    #[clap(long, requires_all=&["attr-file"])]
    config_file: Option<PathBuf>,

    /// save git attribute to `.gitattributes` in current directory.
    #[clap(long)]
    repository: bool,
    /// save git attribute to specified file
    #[clap(long, requires_all=&["config-file"])]
    attr_file: Option<PathBuf>,
}

// memo: git rev-parse --show-toplevel

impl Options {
    fn target(&self) -> TargetConfig {
        if self.global {
            TargetConfig::Global
        } else if self.local {
            TargetConfig::Local
        } else if let Some(config_file) = &self.config_file {
            TargetConfig::File {
                config: config_file,
                attr: self
                    .attr_file
                    .as_ref()
                    .expect("if --config-file exists, --attr-file must be exist"),
            }
        } else {
            unreachable!()
        }
    }
}

enum TargetConfig<'a> {
    // System,
    Global,
    Local,
    File { config: &'a Path, attr: &'a Path },
}

impl<'a> TargetConfig<'a> {
    pub(crate) fn put_git_config_target(&self, cmd: &mut Command) {
        match self {
            TargetConfig::Global => cmd.arg("--global"),
            TargetConfig::Local => cmd.arg("--local"),
            TargetConfig::File { config, .. } => cmd.arg("--file").arg(config),
        };
    }

    pub(crate) fn git_attribute_path(&self) -> Cow<Path> {
        match self {
            TargetConfig::Global => return_if_none!(
                xdg_config_home_for("git", "attributes"),
                die("can't guess XDG_CONFIG_HOME")
            )
            .into(),
            TargetConfig::Local => return_if_none!(guess_git_dir(), die("not in a git directory"))
                .join("info/attributes")
                .into(),
            TargetConfig::File { attr, .. } => Cow::Borrowed(attr),
        }
    }
}

pub(crate) fn main(options: &Options) {
    let target = options.target();
    let executable = return_if_none!(current_exe().ok(), die("path to this exe not found"));
    let escaped_executable = executable.to_string_lossy().replace("\'", "\'\"\'\"\'");
    let driver_name = "patch";

    call_git_config(
        &target,
        &format!("merge.{}.name", driver_name),
        "patch merge driver by anatawa12",
    )
    .expect("failed to call git config");

    call_git_config(
        &target,
        &format!("merge.{}.driver", driver_name),
        &format!("'{}' merge -- %O %A %B %L", escaped_executable),
    )
    .expect("failed to call git config");

    let attribute_file = if options.repository {
        Cow::Borrowed(Path::new(".gitattributes"))
    } else {
        target.git_attribute_path()
    };

    write_git_attributes(attribute_file, driver_name).expect("writing to git attribute failed");

    eprintln!("installation succeed!");
}

fn call_git_config(target: &TargetConfig, name: &str, value: &str) -> std::io::Result<()> {
    let mut git_config = Command::new("git");
    git_config.arg("config");
    target.put_git_config_target(&mut git_config);
    git_config.arg(name);
    git_config.arg(value);
    let status = git_config.spawn()?.wait()?;
    if !status.success() {
        return Err(std::io::Error::new(
            std::io::ErrorKind::Other,
            "git config exited with non zero value",
        ));
    }
    Ok(())
}

fn write_git_attributes(
    attribute_file: impl AsRef<Path>,
    driver_name: &str,
) -> std::io::Result<()> {
    let attribute_file = attribute_file.as_ref();
    if let Some(parent) = attribute_file.parent() {
        create_dir_all(parent)?;
    }
    let mut file = OpenOptions::new()
        .append(true)
        .create(true)
        .open(attribute_file)?;
    write!(file, "\n*.patch merge={}\n", driver_name)?;
    file.flush()?;
    Ok(())
}

// utils

// simplified rewrite of setup_git_directory_gently_1 in git
fn guess_git_dir() -> Option<PathBuf> {
    // try GIT_DIR first
    if let Some(some) = var_os("GIT_DIR") {
        return Some(PathBuf::from(some));
    }

    // then, try .git (gitdir: <path>), .git/, .(bare) for each parent directory
    if let Some(mut cwd) = current_dir().ok() {
        while {
            let dot_git = cwd.pushed(".git");

            // first, try .git file
            if let Some(path) = try_dot_git_file(&dot_git) {
                // if found, return that
                return Some(path);
            }

            // then, try .git directory
            if is_git_directory(&dot_git) {
                return Some(dot_git);
            }
            cwd = dot_git.popped();

            // finally, try cwd as bare
            if is_git_directory(&cwd) {
                return Some(cwd);
            }

            cwd.pop() // go parent
        } {}
    }

    None
}

// try to read .git file, a symbolic ref to other .git dir
fn try_dot_git_file(file: &Path) -> Option<PathBuf> {
    const MAX_SIZE: u64 = 1 << 20; // 1 Mi Bytes
    const PREFIX: &'static [u8] = b"gitdir: ";

    let stat = file.metadata().ok()?;
    if !stat.is_file() {
        return None;
    }
    if stat.len() >= MAX_SIZE {
        return None;
    }
    let f_len = stat.len() as usize;
    if f_len <= PREFIX.len() {
        return None; // no path body
    }

    // read 'gitdir: (<dirname>[^\0]*)(\0.*)?[\r\n]*'
    let body = std::fs::read_to_string(file).ok()?;
    let body = body.strip_prefix("gitdir: ")?;
    let body = body.trim_end_matches(|x| x == '\r' || x == '\n');
    let body = body.split_once('\0').map(|x| x.0).unwrap_or(body);
    let mut path = PathBuf::from(body);

    if !path.is_absolute() {
        if let Some(parent) = file.parent() {
            path = parent.join(path)
        }
    }

    let path = path.canonicalize().ok()?;

    if !is_git_directory(&path) {
        return None;
    }

    return Some(path);
}

fn is_git_directory(path: &Path) -> bool {
    is_head_ref_file(&path.join("HEAD"))
        && (var_os("GIT_OBJECT_DIRECTORY").is_some() || path.join("objects").exists())
        && path.join("refs").exists()
}

fn is_head_ref_file(path: &Path) -> bool {
    let stat = return_if_none!(path.metadata().ok(), false);
    if stat.is_symlink() {
        return return_if_none!(path.read_link().ok(), false).starts_with("refs");
    }
    let mut buf = vec![b'\0'; 256];
    let mut file = return_if_none!(File::open(path).ok(), false);
    let len = return_if_none!(read_fully(&mut file, &mut buf).ok(), false);
    let read = &buf[..len];
    drop(file);

    // symbolic ref
    if let Some(mut ref_target) = read.strip_prefix(b"ref:") {
        ref_target = ref_target
            .strip_prefix_while(|b| char::is_ascii_whitespace(&(*b as char)))
            .unwrap_or(ref_target);
        if ref_target.starts_with(b"refs/") {
            return true;
        }
    }

    // detached HEAD
    return is_oid_hex(read);
}

fn is_oid_hex(buf: &[u8]) -> bool {
    buf.len() >= 40
        && buf[..40]
            .iter()
            .all(|x| matches!(*x, b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F'))
}
