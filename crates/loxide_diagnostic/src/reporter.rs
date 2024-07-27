use miette::{GraphicalReportHandler, GraphicalTheme, Report};

#[derive(Debug)]
pub enum Style {
    Fancy,
    NoColor,
    #[cfg(test)]
    Test,
}

pub struct Reporter {
    handler: GraphicalReportHandler,
    diagnostics: Vec<Report>,
}

impl Reporter {
    pub fn new(style: Style) -> Self {
        let theme = match style {
            Style::Fancy => todo!(),
            Style::NoColor => GraphicalTheme::unicode_nocolor(),
            #[cfg(test)]
            Style::Test => GraphicalTheme::unicode_nocolor(),
        };

        Self {
            handler: GraphicalReportHandler::new_themed(theme),
            diagnostics: Vec::new(),
        }
    }

    pub fn push<T: Into<Report>>(&mut self, diagnostic: T) {
        self.diagnostics.push(diagnostic.into());
    }

    pub fn extend<T, I>(&mut self, diagnostic: I)
    where
        T: Into<Report>,
        I: IntoIterator<Item = T>,
    {
        self.diagnostics
            .extend(diagnostic.into_iter().map(Into::into));
    }

    pub fn report<W: std::fmt::Write>(&self, writer: &mut W) -> std::fmt::Result {
        for diagnostic in &self.diagnostics {
            self.handler.render_report(writer, diagnostic.as_ref())?
        }
        Ok(())
    }

    #[cfg(test)]
    pub fn report_to_string(&self) -> String {
        let mut buffer = String::new();
        self.report(&mut buffer).unwrap();
        buffer
    }
}

#[cfg(test)]
mod tests {
    use crate::reporter::Style;
    use miette::{Diagnostic, Report};
    use std::sync::Arc;
    use thiserror::Error;

    #[derive(Diagnostic, Debug, Error)]
    #[diagnostic()]
    pub enum ExampleError {
        #[error("Trailing space is not allowed")]
        NoTrailingSpace {
            #[label("remove this trailing space")]
            span: (usize, usize), // `(usize, usize)` is `Into<SourceSpan>`
        },
        #[error("Expected upper case letter")]
        ExpectedUpperCase {
            #[label("found lowercase letter")]
            span: (usize, usize), // `(usize, usize)` is `Into<SourceSpan>`
        },
    }

    #[test]
    fn unnamed_source() {
        let source = Arc::new(String::from("hello World! "));

        let diagnostics = vec![
            ExampleError::ExpectedUpperCase {
                span: (0 /* start offset */, 1 /* length */),
            },
            ExampleError::NoTrailingSpace { span: (12, 1) },
        ];

        let diagnostic = diagnostics
            .into_iter()
            .map(|d| Report::from(d).with_source_code(source.clone()))
            .collect::<Vec<_>>();

        let mut reporter = super::Reporter::new(Style::Test);
        reporter.extend(diagnostic);

        insta::assert_snapshot!(reporter.report_to_string());
    }
}
