use loxide_diagnostic::reporter::{Reporter, Style};
use loxide_interpreter::environment::Environment;
use loxide_interpreter::eval::Evaluable;
use loxide_parser::token::Span;
use miette::Report;
use std::ops::Range;
use std::sync::Arc;

struct LineSpan {
    /// the position of linebreaks
    linebreaks: Vec<usize>,
}

impl LineSpan {
    fn new<S: AsRef<str>>(src: S) -> Self {
        let linebreaks = src
            .as_ref()
            .char_indices()
            .filter_map(|(i, c)| if c == '\n' { Some(i) } else { None })
            .collect::<Vec<_>>();
        LineSpan { linebreaks }
    }

    /// Get the line span of the given span
    /// index start from 0, right exclusive
    fn get_line_span(&self, span: &Span) -> Range<usize> {
        let start = self
            .linebreaks
            .iter()
            .position(|index| span.start <= *index)
            .unwrap_or(self.linebreaks.len());
        let end = self
            .linebreaks
            .iter()
            .position(|index| span.end <= *index)
            .unwrap_or(self.linebreaks.len())
            + 1;
        start..end
    }
}

#[derive(PartialEq, Eq)]
struct Insertion {
    /// insertion after line #
    after: usize,
    /// non-descending id
    id: usize,
    content: String,
}

impl PartialOrd for Insertion {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Insertion {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.after.cmp(&other.after).then(self.id.cmp(&other.id))
    }
}

struct AnnotationBuilder {
    source: Arc<String>,
    reporter: Reporter,
    line_span: LineSpan,
    insertions: Vec<Insertion>,
    counter: usize,
}

impl AnnotationBuilder {
    fn new<S: AsRef<str>>(s: S) -> Self {
        let line_span = LineSpan::new(&s);
        let reporter = Reporter::new(Style::NoColor);
        let source = Arc::new(s.as_ref().to_string());
        AnnotationBuilder {
            source,
            reporter,
            line_span,
            insertions: Vec::new(),
            counter: 0,
        }
    }

    fn report_err<E: Into<Report>>(&mut self, err: E) {
        let report: Report = err.into();
        let report = report.with_source_code(self.source.clone());
        self.reporter.push(report)
    }

    /// annotate the evaluation result of a statement
    fn annotate(&mut self, content: String, span: Span) {
        let line_span = self.line_span.get_line_span(&span);
        self.insertions.push(Insertion {
            after: line_span.end - 1,
            id: self.counter,
            content,
        });
        self.counter += 1;
    }

    fn build(mut self) -> String {
        let mut source = self
            .source
            .as_str()
            .lines()
            .map(String::from)
            .collect::<Vec<_>>();
        self.insertions.sort();
        // insert in reverse order
        for insertion in self.insertions.into_iter().rev() {
            source.insert(insertion.after + 1, insertion.content);
        }
        if self.reporter.has_diagnostics() {
            source.push(format!(
                "\n>>>>> Error Section <<<<<\n{}",
                self.reporter.report_to_string()
            ));
        }
        #[cfg(target_os = "windows")]
        {
            source.join("\r\n")
        }
        #[cfg(not(target_os = "windows"))]
        {
            source.join("\n")
        }
    }
}

#[test]
fn test_line_span() {
    let src = "hello\nworld\n";
    let line_span = LineSpan::new(src);
    assert_eq!(line_span.get_line_span(&Span::new(0, 5)), 0..1);
    assert_eq!(line_span.get_line_span(&Span::new(6, 11)), 1..2);
    assert_eq!(line_span.get_line_span(&Span::new(1, 10)), 0..2);
    assert_eq!(line_span.get_line_span(&Span::new(4, 7)), 0..2);
}

fn annotated_eval<S: AsRef<str>>(src: S) -> String {
    let mut builder = AnnotationBuilder::new(&src);
    let mut parser = loxide_parser::parser::Parser::new(src.as_ref());
    let mut env = Environment::global();
    let (stmts, errs) = parser.parse();
    for err in errs {
        builder.report_err(err);
    }
    for stmt in stmts {
        let mut stdout = Vec::<u8>::new();
        let span = stmt.span();
        let _ = stmt
            .eval(&mut env, &mut stdout)
            .map_err(|e| builder.report_err(e));
        let content = String::from_utf8(stdout)
            .expect("Invalid utf8")
            .lines()
            .map(|line| format!("// => {}", line))
            .collect::<Vec<_>>()
            .join("\n");
        builder.annotate(content, span);
    }

    builder.build()
}

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
