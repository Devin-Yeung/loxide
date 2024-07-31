use crate::common::annotate::annotated_eval;

mod common;

// #[test]
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

        // We don't support class yet
        let skip = vec!["class"];
        if skip.into_iter().any(|s| content.contains(s)) {
            continue;
        }

        let test_name = file.path().display().to_string().replace("\\", "/"); // I hate windows

        let skip = vec![
            // insane
            "benchmark",
            "limit",
            // panic
            "unexpected_character",
            // oop
            "field",
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
        ];
        if skip.into_iter().any(|s| test_name.contains(s)) {
            continue;
        }

        let test_name = test_name
            .strip_prefix("fixture/")
            .expect("fail to strip prefix")
            .strip_suffix(".lox")
            .expect("fail to strip suffix");
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
