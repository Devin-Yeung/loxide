#[cfg(test)]
pub mod test_utils {
    pub(crate) const SPAN_CONCEAL: &str = r"(?s)Span \{\s*start: \d*,\s*end: \d*,\s*}";
    pub(crate) const SPAN_REPLACEMENT: &str = "[Span]";
    pub(crate) const SPAN_FILTER: (&str, &str) = (SPAN_CONCEAL, SPAN_REPLACEMENT);

    #[cfg(not(target_os = "windows"))]
    pub(crate) const LINEBREAK: &str = "\n";
    #[cfg(target_os = "windows")]
    pub(crate) const LINEBREAK: &str = "\r\n";
}
