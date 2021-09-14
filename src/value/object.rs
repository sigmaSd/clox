use std::{alloc::Layout, mem, ptr};

use crate::{memory::allocate, vm::VM};

use super::Value;

pub fn print_object(value: Value) {
    match crate::OBJ_TYPE!(value) {
        ObjType::ObjString => {
            print!("{}", crate::AS_RSTRING!(value))
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct Obj {
    pub otype: ObjType,
    pub next: *mut Obj,
}
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[repr(C)]
pub enum ObjType {
    ObjString,
}
impl Obj {
    pub fn is_obj_type(&self, otype: ObjType) -> bool {
        self.otype == otype
    }
}

impl PartialEq for Obj {
    fn eq(&self, other: &Self) -> bool {
        if self.otype != other.otype {
            return false;
        }
        if self.is_obj_type(ObjType::ObjString) {
            let this = self as *const _ as *const ObjString;
            let other = other as *const _ as *const ObjString;
            return unsafe { (*this).as_str() == (*other).as_str() };
        }
        todo!()
    }
}

pub fn take_string(chars: *mut u8, len: usize) -> *const ObjString {
    allocate_string(chars, len)
}

#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(C)]
pub struct ObjString {
    pub len: usize,
    pub chars: *const u8,
    obj: Obj,
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
        let heap_chars = allocate::<u8>(len);
        std::ptr::copy_nonoverlapping(chars, heap_chars, len);
        allocate_string(heap_chars, len)
    }
}

fn allocate_string(chars: *mut u8, len: usize) -> *const ObjString {
    unsafe {
        let string: *mut ObjString = allocate_object::<ObjString>(ObjType::ObjString) as _;
        (*string).chars = chars;
        (*string).len = len;
        string
    }
}

pub unsafe fn allocate_object<T>(otype: ObjType) -> *mut Obj {
    let object: *mut Obj =
        std::alloc::realloc(ptr::null_mut(), Layout::new::<T>(), mem::size_of::<T>()) as _;

    (*object).otype = otype;
    (*object).next = VM.objects;
    VM.objects = object;

    object
}

#[macro_export]
macro_rules! OBJ_TYPE {
    ($val: expr) => {
        unsafe { (*crate::AS_OBJ!($val)).otype }
    };
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
        crate::AS_OBJ!($value) as *const u8 as *const crate::value::object::ObjString
    };
}

#[macro_export]
macro_rules! AS_RSTRING {
    ($value: expr) => {{
        use crate::utils::Helper;
        let string = unsafe { *crate::AS_STRING!($value) };
        string.chars.to_str(string.len)
    }};
}
