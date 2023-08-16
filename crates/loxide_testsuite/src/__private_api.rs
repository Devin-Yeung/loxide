use crate::probe::{FootPrint, Probe};

#[doc(hidden)]
#[macro_export]
macro_rules! _function_name {
    () => {{
        fn f() {}
        fn type_name_of_val<T>(_: T) -> &'static str {
            std::any::type_name::<T>()
        }
        let mut name = type_name_of_val(f).strip_suffix("::f").unwrap_or("");
        while let Some(rest) = name.strip_suffix("::{{closure}}") {
            name = rest;
        }
        name
    }};
}

pub fn register(test_name: &str) {
    Probe::register(test_name);
}

pub fn probe(content: String) {
    Probe::probe(content);
}

pub fn footprints(test_name: &str) -> Vec<FootPrint> {
    Probe::footprints(test_name)
}
