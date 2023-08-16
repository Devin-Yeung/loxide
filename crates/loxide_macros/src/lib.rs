#[macro_export]
/// get the source file (relative to cargo manifest dir)
macro_rules! src {
    ($($segment:expr),+) => {{
        let mut root: ::std::path::PathBuf = ::std::env!("CARGO_MANIFEST_DIR").into();
        $(root.push($segment);)*
        ::std::fs::read_to_string(root).unwrap()
    }}
}
