#[macro_export]
macro_rules! register {
    () => {
        $crate::__private_api::register($crate::_function_name!());
    };
}

#[macro_export]
macro_rules! probe {
    ($target:expr) => {
        $crate::__private_api::probe(::std::format!("{:#?}", $target));
    };
}

#[macro_export]
macro_rules! footprints {
    () => {
        $crate::__private_api::footprints($crate::_function_name!())
    };
}
