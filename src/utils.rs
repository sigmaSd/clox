use std::slice;

pub trait Helper {
    fn to_str<'a>(self, len: usize) -> &'a str;
}

impl Helper for *const u8 {
    fn to_str<'a>(self, len: usize) -> &'a str {
        unsafe { std::str::from_utf8_unchecked(slice::from_raw_parts(self, len)) }
    }
}
