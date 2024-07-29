use once_cell::sync::Lazy;
use std::backtrace::Backtrace;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::sync::Mutex;

static PROBE: Lazy<Mutex<Probe>> = Lazy::new(|| Mutex::new(Probe::new()));
static PROBE_COUNT: Lazy<Mutex<u64>> = Lazy::new(|| Mutex::new(0));
#[derive(Debug)]
pub struct Probe {
    mapping: HashMap<String, Vec<FootPrint>>,
}

#[derive(Clone, PartialEq)]
pub struct FootPrint {
    /// the span of the call expression
    span: Option<(usize, usize)>,
    content: String,
    /// id in non-descending order
    id: u64,
}

impl PartialEq<FootPrint> for &str {
    fn eq(&self, other: &FootPrint) -> bool {
        self.eq(&other.content)
    }
}

impl PartialEq<&str> for FootPrint {
    fn eq(&self, other: &&str) -> bool {
        self.content.eq(other)
    }
}

impl Debug for FootPrint {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.content)
    }
}

impl Probe {
    fn new() -> Self {
        Self {
            mapping: HashMap::new(),
        }
    }

    pub fn register(test_name: &str) {
        PROBE
            .lock()
            .unwrap()
            .mapping
            .insert(test_name.to_string(), Vec::new());
    }

    pub fn probe(content: String, span: Option<(usize, usize)>) {
        let raw = Backtrace::capture().to_string();
        let backtrace = Self::backtrace(&raw);
        let mut guard = PROBE.lock().unwrap();
        for (k, v) in guard.mapping.iter_mut() {
            if backtrace.iter().any(|func| func == k) {
                let mut guard = PROBE_COUNT.lock().expect("lock poisoned");
                let id = *guard;
                *guard += 1;
                v.push(FootPrint { content, span, id });
                break;
            }
        }
    }

    pub fn footprints(test_name: &str) -> Vec<FootPrint> {
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

#[cfg(test)]
mod tests {
    use crate::{footprints, probe, register};

    fn nested(num: i32) {
        outer(num);
    }

    fn outer(num: i32) {
        inner(num);
    }

    fn inner(num: i32) {
        probe!(num);
    }

    #[test]
    fn same_prefix() {
        fn same_prefix_foo() {
            register!();
            nested(42);
            assert_eq!("42", footprints!()[0]);
        }

        fn same_prefix_bar() {
            register!();
            nested(-42);
            assert_eq!(footprints!()[0], "-42");
        }
        same_prefix_foo();
        same_prefix_bar();
    }
}
