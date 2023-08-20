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
    ($file:expr, $closure:expr) => {{
        $crate::_macro_support::source_exec(
            $file,
            ::std::module_path!(),
            ::std::env!("CARGO_MANIFEST_DIR"),
            $closure,
        );
    }};
}
