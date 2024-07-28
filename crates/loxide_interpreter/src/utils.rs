#[cfg(test)]
pub mod test_utils {
    #[cfg(not(target_os = "windows"))]
    pub(crate) const LINEBREAK: &str = "\n";
    #[cfg(target_os = "windows")]
    pub(crate) const LINEBREAK: &str = "\r\n";
}
