use once_cell::sync::Lazy;
use std::backtrace::Backtrace;
use std::collections::HashMap;
use std::sync::Mutex;

static PROBE: Lazy<Mutex<Probe>> = Lazy::new(|| Mutex::new(Probe::new()));
#[derive(Debug)]
pub struct Probe {
    mapping: HashMap<String, Vec<String>>,
}

impl Probe {
    fn new() -> Self {
        Self {
            mapping: HashMap::new(),
        }
    }

    pub(crate) fn register(test_name: &str) {
        PROBE
            .lock()
            .unwrap()
            .mapping
            .insert(test_name.to_string(), Vec::new());
    }

    pub(crate) fn probe(content: String) {
        let raw = Backtrace::capture().to_string();
        let backtrace = Self::backtrace(&raw);
        let mut guard = PROBE.lock().unwrap();
        for (k, v) in guard.mapping.iter_mut() {
            if backtrace.iter().any(|func| func == k) {
                v.push(content);
                break;
            }
        }
    }

    pub(crate) fn footprints(test_name: &str) -> Vec<String> {
        let guard = PROBE.lock().unwrap();
        guard.mapping.get(test_name).unwrap_or(&Vec::new()).clone()
    }

    fn backtrace(backtrace: &str) -> Vec<&str> {
        let mut stacks = Vec::new();
        for line in backtrace.lines() {
            let trimmed = line.trim_start();
            if &trimmed[..2] != "at" {
                let func = trimmed[trimmed.find(':').unwrap() + 1..].trim();
                stacks.push(func);
            }
        }
        stacks
    }
}

mod tests {
    use crate::{footprints, probe, register};

    fn nested(name: &str) {
        outer(name);
    }

    fn outer(name: &str) {
        inner(name);
    }

    fn inner(name: &str) {
        probe!(name);
    }

    #[test]
    fn same_prefix() {
        fn same_prefix_foo() {
            register!();
            nested("foo");
            assert_eq!("\"foo\"", footprints!()[0]);
        }

        fn same_prefix_bar() {
            register!();
            nested("bar");
            assert_eq!("\"bar\"", footprints!()[0]);
        }
        same_prefix_foo();
        same_prefix_bar();
    }
}
