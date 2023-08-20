use std::fs;
use std::fs::File;
use std::path::{Path, PathBuf};

// concat path
macro_rules! path {
    ($root:expr, $($segment:expr),+) => {{
        let mut root: PathBuf = $root;
        $(root.push($segment);)*
        root
    }}
}

#[derive(Debug)]
pub struct Tester {
    source_path: PathBuf,
    snapshot_path: PathBuf,
}

impl Tester {}

pub struct TesterBuilder<P: AsRef<Path>, S: AsRef<str>> {
    source_name: P,
    module_path: S,
}

impl<P: AsRef<Path>, S: AsRef<str>> TesterBuilder<P, S> {
    pub fn new(source_name: P, module_path: S) -> Self {
        Self {
            source_name,
            module_path,
        }
    }

    fn base(&self) -> PathBuf {
        let mut root: PathBuf = std::env!("CARGO_MANIFEST_DIR").into();
        root.push("snapshots");

        self.module_path
            .as_ref()
            .trim_end_matches("::tests")
            .trim_end_matches("::test")
            .split("::")
            .skip(1) // remove the module name
            .for_each(|segment| root.push(segment));

        root
    }

    fn auto_create_dir<T: AsRef<Path>>(path: T) {
        let path = path.as_ref();
        if !path.exists() {
            fs::create_dir_all(path).unwrap();
        }
    }

    fn auto_create_file<T: AsRef<Path>>(path: T) {
        let path = path.as_ref();
        if !path.exists() {
            File::create(path).unwrap();
        }
    }

    pub fn source_path(&self) -> PathBuf {
        let path = path!(self.base(), "input");
        Self::auto_create_dir(&path);
        let path = path!(path, &self.source_name);
        Self::auto_create_file(&path);
        path
    }

    pub fn snapshot_path(&self) -> PathBuf {
        let path = path!(self.base(), "output");
        Self::auto_create_dir(&path);
        path
    }

    pub fn build(self) -> Tester {
        let tester = Tester {
            source_path: self.source_path(),
            snapshot_path: self.snapshot_path(),
        };
        tester
    }
}

pub fn source_exec<F: FnMut(&str)>(source_name: &str, module_path: &str, mut f: F) {}

#[cfg(test)]
mod tests {
    use crate::unittest::tester::TesterBuilder;
    use std::fs;
    use std::path::PathBuf;

    #[test]
    fn builder() {
        let tester = TesterBuilder::new("foobar.lox", module_path!()).build();
        assert!(tester
            .source_path
            .ends_with("snapshots/unittest/tester/input/foobar.lox"));
        assert!(tester
            .snapshot_path
            .ends_with("snapshots/unittest/tester/output"));
        assert!(path!(
            env!("CARGO_MANIFEST_DIR").into(),
            "snapshots/unittest/tester/input/foobar.lox"
        )
        .exists());
        assert!(path!(
            env!("CARGO_MANIFEST_DIR").into(),
            "snapshots/unittest/tester/output"
        )
        .exists());
        fs::remove_dir_all(path!(env!("CARGO_MANIFEST_DIR").into(), "snapshots")).unwrap();
    }
}
