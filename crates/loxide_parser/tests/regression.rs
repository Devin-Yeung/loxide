#[cfg(test)]
mod regression {
    macro_rules! regression {
        ($name:ident, $closure:expr) => {
            #[::loxide_testsuite::pin("regression")]
            mod $name {
                ::loxide_testsuite::unittest!($name, $closure);
            }
        };
    }
    automod::dir!("tests/regression");
}
