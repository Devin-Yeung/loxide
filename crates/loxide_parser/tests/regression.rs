#[cfg(test)]
mod regression {
    macro_rules! regression {
        ($name:ident, $closure:expr) => {
            #[::loxide_testsuite::pin("regression")]
            mod $name {
                const FILTERS: [(&str, &str); 1] = [(r"(?s)Span \{\s*start: \d*,\s*end: \d*,\s*}", "[Span]")];
                ::loxide_testsuite::unittest!($name, filters => FILTERS.into(), $closure);
            }
        };
    }
    automod::dir!("tests/regression");
}
