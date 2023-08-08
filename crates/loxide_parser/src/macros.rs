#[doc(hidden)]
#[macro_export]
/// the source file is relative to cargo manifest dir
macro_rules! src {
    ($($segment:expr),+) => {{
        let mut root: ::std::path::PathBuf = ::std::env!("CARGO_MANIFEST_DIR").into();
        $(root.push($segment);)*
        dbg!(&root);
        ::std::fs::read_to_string(root).unwrap()
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
