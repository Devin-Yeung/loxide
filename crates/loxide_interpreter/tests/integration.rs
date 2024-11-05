use crate::common::annotate::annotated_eval;

mod common;

#[test]
fn integration() {
    use walkdir::WalkDir;

    let files = WalkDir::new("fixture")
        .into_iter()
        .filter_map(Result::ok)
        .filter(|entry| entry.file_type().is_file())
        .collect::<Vec<_>>();

    for file in files {
        let content = std::fs::read_to_string(file.path())
            .unwrap_or_else(|_| panic!("Invalid file: {}", file.path().display()));

        let test_name = file.path().display().to_string().replace("\\", "/"); // I hate windows

        let skip = vec![
            // insane
            "benchmark",
            "limit",
            // panic
            "unexpected_character",
            // oop
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
            "while",
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

        let test_name = test_name
            .strip_prefix("fixture/")
            .expect("fail to strip prefix")
            .strip_suffix(".lox")
            .expect("fail to strip suffix");

        if skip.into_iter().any(|s| test_name.starts_with(s)) {
            continue;
        }
        println!("Running test: {}", test_name);

        let result = annotated_eval(&content);
        insta::with_settings!({
            snapshot_suffix => test_name,
            prepend_module_to_snapshot => false,
        }, {
            insta::assert_snapshot!(result)
        });
    }
}
