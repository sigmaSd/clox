use std::slice;

pub trait Helper {
    fn to_str<'a>(self, len: usize) -> &'a str;
}
pub trait PointerDeref<'a, T> {
    fn deref(self) -> &'a T;
    fn deref_mut(self) -> &'a mut T;
}
impl<'a, T> PointerDeref<'a, T> for *const T {
    fn deref(self) -> &'a T {
        unsafe { self.as_ref().unwrap() }
    }

    fn deref_mut(self) -> &'a mut T {
        panic!("only *mut T can use deref_mut");
    }
}
impl<'a, T> PointerDeref<'a, T> for *mut T {
    fn deref(self) -> &'a T {
        (self as *const T).deref()
    }

    fn deref_mut(self) -> &'a mut T {
        unsafe { self.as_mut().unwrap() }
    }
}

impl Helper for *const u8 {
    fn to_str<'a>(self, len: usize) -> &'a str {
        unsafe { std::str::from_utf8_unchecked(slice::from_raw_parts(self, len)) }
    }
}
