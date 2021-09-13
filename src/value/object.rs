use std::{alloc::Layout, mem, ptr};

use crate::vm::VM;

pub fn print_object(obj: &Obj) {
    match obj {
        Obj::ObjString(string) => {
            print!("{}", string.as_str());
        }
        Obj::A => todo!(),
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Obj {
    ObjString(ObjString),
    A,
}
impl Obj {
    pub fn is_string(&self) -> bool {
        matches!(self, Obj::ObjString(_))
    }
}

impl PartialEq for Obj {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::ObjString(l0), Self::ObjString(r0)) => l0.as_str() == r0.as_str(),
            _ => todo!(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct ObjString {
    pub len: usize,
    pub chars: *const u8,
    pub next: *const Obj,
}
impl ObjString {
    fn as_str(&self) -> &str {
        unsafe {
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(self.chars as _, self.len))
        }
    }
}

pub fn copy_string(chars: *const u8, len: usize) -> *const ObjString {
    unsafe {
        let heap_chars = std::alloc::realloc(
            ptr::null_mut(),
            Layout::new::<u8>(),
            mem::size_of::<u8>() * len,
        ) as _;
        std::ptr::copy_nonoverlapping(chars, heap_chars, len);
        allocate_string(heap_chars, len)
    }
}

fn allocate_string(heap_chars: *mut u8, len: usize) -> *const ObjString {
    unsafe {
        let string = std::alloc::realloc(
            ptr::null_mut(),
            Layout::new::<ObjString>(),
            mem::size_of::<ObjString>(),
        ) as *mut ObjString;
        (*string).chars = heap_chars;
        (*string).len = len;
        (*string).next = VM.objects;
        VM.objects = Box::into_raw(Box::new(Obj::ObjString(*string)));
        string
    }
}

#[macro_export]
macro_rules! AS_OBJ_TYPE {
    ($obj: expr, $otype: path) => {
        if let $otype(obj) = $obj {
            obj
        } else {
            unreachable!()
        }
    };
}

#[macro_export]
macro_rules! AS_STRING {
    ($value: expr) => {
        crate::AS_OBJ_TYPE!(crate::AS_OBJ!($value), crate::value::Obj::ObjString)
    };
}
