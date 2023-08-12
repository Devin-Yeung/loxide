#[cfg(test)]
mod regression {
    macro_rules! regression {
        ($file:expr, $closure:expr) => {
            ::insta::with_settings!(
                {
                    snapshot_path => "../../snapshots/regression/snapshots-outputs",
                    prepend_module_to_snapshot => false
                },
                {
                    ::insta::glob!("../../snapshots/regression/snapshots-inputs", $file, $closure)
                }
            );
        };
    }
    automod::dir!("tests/regression");
}
