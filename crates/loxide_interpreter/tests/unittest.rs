mod common;

#[cfg(test)]
mod eval {
    mod test {
        use crate::common::annotate::annotated_eval;
        use crate::common::utils::LINEBREAK;
        use loxide_diagnostic::reporter::{Reporter, Style};
        use loxide_interpreter::environment::Environment;
        use loxide_interpreter::eval::Evaluable;
        use loxide_testsuite::unittest;
        use miette::Report;
        use std::string::String;
        use std::sync::Arc;

        fn display_eval_error(src: &str) -> String {
            let mut parser = loxide_parser::parser::Parser::new(src);
            let mut env = Environment::global();
            let mut stdout = Vec::<u8>::new();
            let source = Arc::new(src.to_string());

            let (stmts, _) = parser.parse();
            stmts
                .into_iter()
                .map(|stmt| stmt.eval(&mut env, &mut stdout))
                .filter(Result::is_err)
                .map(Result::unwrap_err)
                .map(|err| {
                    let mut reporter = Reporter::new(Style::NoColor);
                    let report: Report = err.into();
                    reporter.push(report.with_source_code(source.clone()));
                    reporter.report_to_string()
                })
                .collect::<Vec<_>>()
                .join(LINEBREAK)
        }

        unittest!(literal, |src| {
            insta::assert_snapshot!(annotated_eval(src));
        });

        unittest!(valid_unary, |src| {
            insta::assert_snapshot!(annotated_eval(src));
        });

        unittest!(invalid_unary, |src| {
            let results = src
                .split(LINEBREAK)
                .map(display_eval_error)
                .collect::<Vec<_>>()
                .join(LINEBREAK);
            insta::assert_snapshot!(results);
        });

        unittest!(invalid_binary, |src| {
            let results = src
                .split(LINEBREAK)
                .map(display_eval_error)
                .collect::<Vec<_>>()
                .join(LINEBREAK);
            insta::assert_snapshot!(results);
        });

        unittest!(valid_binary, |src| {
            insta::assert_snapshot!(annotated_eval(src));
        });

        unittest!(variable, |src| {
            insta::assert_snapshot!(annotated_eval(src));
        });

        unittest!(block_stmt, |src| {
            insta::assert_snapshot!(annotated_eval(src));
        });

        unittest!(if_stmt, |src| {
            insta::assert_snapshot!(annotated_eval(src));
        });

        unittest!(while_stmt, |src| {
            insta::assert_snapshot!(annotated_eval(src));
        });

        unittest!(for_stmt, |src| {
            insta::assert_snapshot!(annotated_eval(src));
        });

        unittest!(for_stmt_expect_bool, |src| {
            let result = display_eval_error(src);
            insta::assert_snapshot!(result);
        });

        unittest!(while_stmt_expect_bool, |src| {
            let result = display_eval_error(src);
            insta::assert_snapshot!(result);
        });

        unittest!(fn_call, |src| {
            insta::assert_snapshot!(annotated_eval(src));
        });

        unittest!(bad_fn_call, |src| {
            let result = display_eval_error(src);
            insta::assert_snapshot!(result);
        });

        unittest!(closure, |src| {
            insta::assert_snapshot!(annotated_eval(src));
        });
    }
}
