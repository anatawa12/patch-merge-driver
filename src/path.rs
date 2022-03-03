use home::home_dir;
use std::env::var_os;
use std::path::{Path, PathBuf};

pub fn xdg_config_home_for(dir: impl AsRef<Path>, file: impl AsRef<Path>) -> Option<PathBuf> {
    fn impl_(dir: &Path, file: &Path) -> Option<PathBuf> {
        if let Some(config_home) = var_os("XDG_CONFIG_HOME") {
            if !config_home.is_empty() {
                let mut buf = PathBuf::from(config_home);
                buf.reserve(dir.as_os_str().len() + file.as_os_str().len() + 1);
                buf.push(dir);
                buf.push(file);
                return Some(buf);
            }
        }
        if let Some(mut buf) = home_dir() {
            buf.push(".config");
            buf.reserve(dir.as_os_str().len() + file.as_os_str().len() + ".config".len() + 3);
            buf.push(dir);
            buf.push(file);
            return Some(buf);
        }

        None
    }
    impl_(dir.as_ref(), file.as_ref())
}
