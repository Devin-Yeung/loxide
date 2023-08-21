#[macro_export]
macro_rules! register {
    () => {
        $crate::__private_api::register($crate::_function_name!());
    };
}

#[macro_export]
macro_rules! probe {
    ($target:expr) => {{
        $crate::__private_api::probe(::std::format!("{:#?}", $target));
        $target
    }};
}

#[macro_export]
macro_rules! footprints {
    () => {
        $crate::__private_api::footprints($crate::_function_name!())
    };
}

#[macro_export]
macro_rules! unittest {
    ($name:ident, $closure:expr) => {
        #[test]
        fn $name() {
            $crate::_macro_support::source_exec(
                concat!(stringify!($name), ".lox"),
                ::std::module_path!(),
                ::std::env!("CARGO_MANIFEST_DIR"),
                $closure,
            );
        }
    };
}
