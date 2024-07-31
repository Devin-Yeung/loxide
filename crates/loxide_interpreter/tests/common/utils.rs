#[cfg(not(target_os = "windows"))]
pub const LINEBREAK: &str = "\n";
#[cfg(target_os = "windows")]
pub const LINEBREAK: &str = "\r\n";
