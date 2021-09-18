use std::{alloc::Layout, fmt::Display, mem, ptr};

use crate::{
    chunk::Chunk,
    memory::{allocate, free_array},
    table::Table,
    utils::Helper,
    vm::VM,
    NIL_VAL, OBJ_VAL,
};

use super::Value;

pub fn print_object(value: Value) {
    match crate::OBJ_TYPE!(value) {
        ObjType::String => {
            print!("{}", crate::AS_RSTRING!(value))
        }
        ObjType::Function => {
            print!("{}", unsafe { *crate::AS_FUNCTION!(value) });
        }
        ObjType::Native => {
            print!("<native fn>");
        }
        ObjType::Closure => {
            print!("{}", unsafe { *(*crate::AS_CLOSURE!(value)).function });
        }
        ObjType::UpValue => {
            print!("upvalue");
        }
        ObjType::Class => {
            print!("{}", unsafe { (*(*crate::AS_CLASS!(value)).name).as_str() })
        }
        ObjType::Instance => {
            print!("{} instance", unsafe {
                (*(*(*crate::AS_INSTANCE!(value)).klass).name).as_str()
            })
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ObjInstance {
    obj: Obj,
    pub klass: *const ObjClass,
    pub fields: Table,
}

impl ObjInstance {
    pub fn new(klass: *const ObjClass) -> *mut Self {
        unsafe {
            let instance: *mut ObjInstance = allocate_object(ObjType::Instance);
            (*instance).klass = klass;
            (*instance).fields.init();
            instance
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ObjClass {
    obj: Obj,
    pub name: *const ObjString,
}

impl ObjClass {
    pub fn new(name: *const ObjString) -> *mut Self {
        unsafe {
            let klass: *mut ObjClass = allocate_object(ObjType::Class);
            (*klass).name = name;
            klass
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ObjUpValue {
    obj: Obj,
    pub location: *mut Value,
    pub closed: Value,
    pub next: *mut ObjUpValue,
}

impl ObjUpValue {
    pub fn new(slot: *mut Value) -> *mut Self {
        unsafe {
            let upvalue: *mut ObjUpValue = allocate_object(ObjType::UpValue);
            (*upvalue).location = slot;
            (*upvalue).closed = NIL_VAL!();
            (*upvalue).next = ptr::null_mut();
            upvalue
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ObjClosure {
    obj: Obj,
    pub function: *mut ObjFunction,
    pub upvalues: *mut *mut ObjUpValue,
    pub upvalue_count: usize,
}

impl ObjClosure {
    pub fn new(function: *mut ObjFunction) -> *mut Self {
        unsafe {
            let upvalues: *mut *mut ObjUpValue = allocate((*function).upvalue_count);
            for i in 0..(*function).upvalue_count {
                *upvalues.add(i) = ptr::null_mut();
            }

            let closure: *mut ObjClosure = allocate_object(ObjType::Closure);
            (*closure).function = function;
            (*closure).upvalues = upvalues;
            (*closure).upvalue_count = (*function).upvalue_count;
            closure
        }
    }
}

pub type NativeFn = fn(usize, *const Value) -> Value;

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ObjNative {
    obj: Obj,
    pub function: NativeFn,
}

impl ObjNative {
    pub fn new(function: NativeFn) -> *mut ObjNative {
        unsafe {
            let native: *mut ObjNative = allocate_object(ObjType::Native);
            (*native).function = function;
            native
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ObjFunction {
    obj: Obj,
    pub arity: usize,
    pub upvalue_count: usize,
    pub chunk: Chunk,
    pub name: *mut ObjString,
}
impl Display for ObjFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !self.name.is_null() {
            unsafe { write!(f, "<fn {}>", (*self.name).as_str()) }
        } else {
            write!(f, "<script>")
        }
    }
}

impl ObjFunction {
    pub fn new() -> *mut ObjFunction {
        unsafe {
            let function: *mut ObjFunction = allocate_object(ObjType::Function);
            (*function).arity = 0;
            (*function).upvalue_count = 0;
            (*function).name = ptr::null_mut();
            (*function).chunk = Chunk::new();

            function
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Obj {
    pub otype: ObjType,
    pub next: *mut Obj,
    pub is_marked: bool,
}
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ObjType {
    Function,
    Native,
    String,
    Closure,
    UpValue,
    Class,
    Instance,
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
        hash = hash.wrapping_mul(16777619);
    }
    hash
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[repr(C)]
pub struct ObjString {
    pub obj: Obj,
    pub len: usize,
    pub chars: *const u8,
    pub hash: u32,
}
impl ObjString {
    pub fn as_str(&self) -> &str {
        self.chars.to_str(self.len)
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
        let string: *mut ObjString = allocate_object::<ObjString>(ObjType::String);
        (*string).chars = chars;
        (*string).len = len;
        (*string).hash = hash;

        VM.push(OBJ_VAL!(string));
        VM.strings.table_set(string, NIL_VAL!());
        VM.pop();

        string
    }
}

pub unsafe fn allocate_object<T>(otype: ObjType) -> *mut T {
    let object: *mut Obj =
        std::alloc::realloc(ptr::null_mut(), Layout::new::<T>(), mem::size_of::<T>()) as _;

    (*object).otype = otype;
    (*object).is_marked = false;
    (*object).next = VM.objects;
    VM.objects = object;

    if cfg!(feature = "DEBUG_LOG_GC") {
        println!(
            "{:p} allocate {}bytes for {:?}",
            object,
            mem::size_of::<T>(),
            otype
        );
    }

    (*object).otype = otype;
    object as _
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
        crate::AS_OBJ!($value) as *const crate::value::object::ObjString
    };
}

#[macro_export]
macro_rules! AS_FUNCTION {
    ($value: expr) => {
        crate::AS_OBJ!($value) as *mut crate::value::object::ObjFunction
    };
}

#[macro_export]
macro_rules! AS_NATIVE {
    ($value: expr) => {{
        let native = crate::AS_OBJ!($value) as *mut crate::value::object::ObjNative;
        (*native).function
    }};
}

#[macro_export]
macro_rules! AS_CLOSURE {
    ($value: expr) => {{
        crate::AS_OBJ!($value) as *mut crate::value::object::ObjClosure
    }};
}

#[macro_export]
macro_rules! AS_CLASS {
    ($value: expr) => {{
        crate::AS_OBJ!($value) as *mut crate::value::object::ObjClass
    }};
}

#[macro_export]
macro_rules! AS_INSTANCE {
    ($value: expr) => {{
        crate::AS_OBJ!($value) as *mut crate::value::object::ObjInstance
    }};
}

#[macro_export]
macro_rules! AS_RSTRING {
    ($value: expr) => {{
        use crate::utils::Helper;
        let string = unsafe { *crate::AS_STRING!($value) };
        string.chars.to_str(string.len)
    }};
}
