use crate::common::annotate::annotated_eval;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::{mpsc, Arc};
use std::thread;
use std::time::Duration;
use walkdir::WalkDir;

mod common;

#[derive(Debug)]
pub struct TestEntry {
    pub name: String,
    pub path: PathBuf,
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
        .filter(|entry| skip.iter().all(|s| !entry.name.starts_with(s)))
        .collect::<Vec<_>>();

    let mut summary: HashMap<String, bool> = test_cases
        .iter()
        .map(|entry| (entry.name.clone(), false))
        .collect();

    let (tx, rx) = mpsc::channel();

    let handles = test_cases
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
    let mut sorted = summary.into_iter().collect::<Vec<_>>();
    sorted.sort_by(|(name1, pass1), (name2, pass2)| pass2.cmp(pass1).then(name1.cmp(name2)));

    insta::with_settings!({
        snapshot_suffix => "summary",
        prepend_module_to_snapshot => false,
    }, {
        insta::assert_debug_snapshot!(sorted)
    });
}
