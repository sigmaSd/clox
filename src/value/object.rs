use std::{alloc::Layout, mem, ptr};

use crate::{
    memory::{allocate, free_array},
    vm::VM,
    NIL_VAL,
};

use super::Value;

pub fn print_object(value: Value) {
    match crate::OBJ_TYPE!(value) {
        ObjType::ObjString => {
            print!("{}", crate::AS_RSTRING!(value))
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

pub fn take_string(chars: *mut u8, len: usize) -> *const ObjString {
    unsafe {
        let hash = hash_string(chars, len);
        let interned = VM.strings.find_string(chars, len, hash);
        if let Some(interned) = interned {
            free_array(chars, len + 1);
            return interned;
        }
        allocate_string(chars, len, hash)
    }
}

unsafe fn hash_string(key: *const u8, len: usize) -> u32 {
    let mut hash: u32 = 2166136261;
    for i in 0..len {
        hash ^= *key.add(i) as u32;
        hash *= 16777619;
    }
    hash
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[repr(C)]
pub struct ObjString {
    pub len: usize,
    pub chars: *const u8,
    pub hash: u32,
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
        let hash = hash_string(chars, len);
        let interned = VM.strings.find_string(chars, len, hash);
        if let Some(interned) = interned {
            return interned;
        }

        let heap_chars = allocate::<u8>(len);
        std::ptr::copy_nonoverlapping(chars, heap_chars, len);
        allocate_string(heap_chars, len, hash)
    }
}

fn allocate_string(chars: *mut u8, len: usize, hash: u32) -> *const ObjString {
    unsafe {
        let string: *mut ObjString = allocate_object::<ObjString>(ObjType::ObjString) as _;
        (*string).chars = chars;
        (*string).len = len;
        (*string).hash = hash;

        VM.strings.table_set(string, NIL_VAL!());

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
