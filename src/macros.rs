#[doc(hidden)]
#[macro_export]
macro_rules! src {
    ($base:expr, $($segment:expr),+) => {{
        let mut base: ::std::path::PathBuf = $base.into();
        $(base.push($segment);)*
        let current = ::std::path::PathBuf::from(::std::file!());
        let current = current.parent().unwrap();
        let target = ::std::fs::canonicalize(current.join(base)).unwrap();
        ::std::fs::read_to_string(target).unwrap()
    }}
}

#[doc(hidden)]
#[macro_export]
macro_rules! tokens {
    ($src:expr) => {{
        let scanner = $crate::scanner::Scanner::from($src);
        scanner.collect::<::std::vec::Vec<_>>()
    }};
}
