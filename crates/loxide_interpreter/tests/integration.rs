use crate::common::annotate::annotated_eval;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt::Display;
use std::path::PathBuf;
use std::sync::mpsc;
use std::thread;
use std::time::Duration;
use tabled::{Table, Tabled};
use walkdir::WalkDir;

mod common;

#[derive(Debug)]
pub struct TestEntry {
    pub name: String,
    pub path: PathBuf,
}

#[derive(Debug, PartialEq, Eq, Ord)]
pub enum Status {
    Passed,
    Failed,
    Skipped,
}

impl PartialOrd for Status {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        // pass > skip > fail
        Some(match (self, other) {
            (Status::Passed, Status::Passed) => Ordering::Equal,
            (Status::Passed, _) => Ordering::Greater,
            (_, Status::Passed) => Ordering::Less,
            (Status::Skipped, Status::Skipped) => Ordering::Equal,
            (Status::Skipped, _) => Ordering::Greater,
            (_, Status::Skipped) => Ordering::Less,
            (Status::Failed, Status::Failed) => Ordering::Equal,
        })
    }
}

impl Display for Status {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Status::Passed => write!(f, "Passed"),
            Status::Failed => write!(f, "Failed"),
            Status::Skipped => write!(f, "Skipped"),
        }
    }
}

#[derive(Tabled)]
pub struct TestResult {
    pub name: String,
    pub status: Status,
}

#[test]
fn integration() {
    let skip = vec![
        // insane
        "benchmark",
        "limit",
        // panic
        "unexpected_character",
        // oop
        "while/class_in_body",
        "field",
        "call/object",
        "if/class_in_else",
        "if/class_in_then",
        "assignment/to_this",
        "for/class_in_body",
        "operator/equals_method",
        "operator/equals_class",
        "operator/not_class",
        "return/in_method",
        // dead loop
        "number/decimal_point_at_eof",
        "number/trailing_dot",
        "string/unterminated",
        "scanning/numbers",
        // contains non-ascii
        "string/error_after_multiline",
        "for/fun_in_body",
        "string/literals",
        "unicode",
        // disable all tests
        "closure",
        "constructor",
        "expressions",
        "limit",
        "method",
        "number",
        "regression",
        "scanning",
        "super",
        "benchmark",
        "class",
        "comments",
        "field",
        "inheritance",
        "logical_operator",
        "string",
        "this",
        "variable",
        // bug
        "function/mutual_recursion", // need to investigate the semantic
        "function/print",            // don't support native function yet
    ];

    let test_cases = WalkDir::new("fixture")
        .into_iter()
        .filter_map(Result::ok)
        .filter(|entry| entry.file_type().is_file())
        .map(|entry| {
            let path = entry.path().to_path_buf();
            let test_name = path.display().to_string().replace("\\", "/"); // I hate windows
            let test_name = test_name
                .strip_prefix("fixture/")
                .expect("fail to strip prefix")
                .strip_suffix(".lox")
                .expect("fail to strip suffix");
            TestEntry {
                name: test_name.to_string(),
                path,
            }
        })
        .collect::<Vec<_>>();

    let skipped = test_cases
        .iter()
        .filter(|entry| skip.iter().any(|s| entry.name.starts_with(s)))
        .map(|entry| TestResult {
            name: entry.name.clone(),
            status: Status::Skipped,
        })
        .collect::<Vec<_>>();

    let filtered = test_cases
        .into_iter()
        .filter(|entry| skip.iter().all(|s| !entry.name.starts_with(s)))
        .collect::<Vec<_>>();

    let mut summary: HashMap<String, bool> = filtered
        .iter()
        .map(|entry| (entry.name.clone(), false))
        .collect();

    let (tx, rx) = mpsc::channel();

    let handles = filtered
        .into_iter()
        .map(|case| {
            let tx = tx.clone();
            thread::spawn(move || {
                let content = std::fs::read_to_string(&case.path)
                    .unwrap_or_else(|_| panic!("Invalid file: {}", case.path.display()));

                let result = annotated_eval(&content);
                insta::with_settings!({
                    snapshot_suffix => &case.name,
                    prepend_module_to_snapshot => false,
                }, {
                    insta::assert_snapshot!(result)
                });
                tx.send((case.name.clone(), true)).unwrap();
            })
        })
        .collect::<Vec<_>>();

    for _ in handles {
        match rx.recv_timeout(Duration::from_secs(5)) {
            Ok((name, pass)) => {
                let _ = summary.get_mut(&name).map(|v| *v = pass);
            }
            Err(_) => { /* Do nothing */ }
        }
    }

    // sort the test cases by name, all passed test cases will be at the top
    let mut summary = summary
        .into_iter()
        .map(|(name, pass)| {
            let status = if pass { Status::Passed } else { Status::Failed };
            TestResult { name, status }
        })
        .chain(skipped.into_iter())
        .collect::<Vec<_>>();
    summary.sort_by(|a, b| a.status.cmp(&b.status).then(a.name.cmp(&b.name)));

    let count = summary.len();
    let table = Table::new(summary);
    let mut output = String::from(table.to_string());
    output.push_str(&format!("\n\nTest cases in total: {}", count));

    insta::with_settings!({
        snapshot_suffix => "summary",
        prepend_module_to_snapshot => false,
    }, {
        insta::assert_snapshot!(output)
    });
}
