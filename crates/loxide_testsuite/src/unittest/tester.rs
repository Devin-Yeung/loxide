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

impl Tester {
    fn source_exec<F: FnMut(&str)>(&self, filters: Vec<(&str, &str)>, mut f: F) {
        let source = fs::read_to_string(&self.source_path).unwrap();
        insta::with_settings!({
            snapshot_path => &self.snapshot_path,
            prepend_module_to_snapshot => false,
            filters => filters,
        },{
            f(&source);
        });
    }
}

pub struct TesterBuilder<P: AsRef<Path>, S: AsRef<str>> {
    source_name: P,
    module_path: S,
    cargo_manifest_dir: P,
}

impl<P: AsRef<Path>, S: AsRef<str>> TesterBuilder<P, S> {
    fn new(source_name: P, module_path: S, cargo_manifest_dir: P) -> Self {
        Self {
            source_name,
            module_path,
            cargo_manifest_dir,
        }
    }

    fn base(&self) -> PathBuf {
        let mut root: PathBuf = self.cargo_manifest_dir.as_ref().into();
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

    fn source_path(&self) -> PathBuf {
        let path = path!(self.base(), "input");
        Self::auto_create_dir(&path);
        let path = path!(path, &self.source_name);
        Self::auto_create_file(&path);
        path
    }

    fn snapshot_path(&self) -> PathBuf {
        let path = path!(self.base(), "output");
        Self::auto_create_dir(&path);
        path
    }

    fn build(self) -> Tester {
        Tester {
            source_path: self.source_path(),
            snapshot_path: self.snapshot_path(),
        }
    }
}

pub fn source_exec<F: FnMut(&str)>(
    source_name: &str,
    module_path: &str,
    cargo_manifest_dir: &str,
    filter: Vec<(&str, &str)>,
    f: F,
) {
    TesterBuilder::new(source_name, module_path, cargo_manifest_dir)
        .build()
        .source_exec(filter, f);
}

#[cfg(test)]
mod tests {
    use crate::unittest::tester::TesterBuilder;
    use std::fs;
    use std::path::PathBuf;

    #[test]
    fn builder() {
        let tester =
            TesterBuilder::new("foobar.lox", module_path!(), env!("CARGO_MANIFEST_DIR")).build();
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
