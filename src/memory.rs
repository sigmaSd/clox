use std::{
    alloc::{dealloc, realloc, Layout},
    mem, process,
    ptr::{self, null_mut},
};

use crate::value::{
    object::{ObjString, ObjType},
    Obj,
};

pub unsafe fn free_object(obj: *const Obj) {
    match (*obj).otype {
        ObjType::ObjString => {
            let string: *const ObjString = obj as _;
            let string = *string;
            free_array(string.chars as *mut u8, string.len);
            free::<ObjString>(obj);
        }
    }
}

unsafe fn free<T>(obj: *const Obj) {
    realloc(obj as _, Layout::new::<T>(), 0);
}

pub fn grow_capacity(capacity: usize) -> usize {
    if capacity < 8 {
        8
    } else {
        capacity * 2
    }
}

pub fn grow_array<T>(ptr: *mut T, old_count: usize, new_count: usize) -> *mut T {
    let result = reallocate(
        ptr as *mut u8,
        mem::size_of::<T>() * old_count,
        mem::size_of::<T>() * new_count,
    );
    if result.is_null() {
        process::exit(1)
    }
    result as *mut T
}
pub fn free_array<T>(ptr: *mut T, old_count: usize) {
    reallocate(ptr, mem::size_of::<T>() * old_count, 0);
}

fn reallocate<T>(ptr: *mut T, _old_size: usize, new_size: usize) -> *mut u8 {
    unsafe {
        if new_size == 0 {
            dealloc(ptr as _, Layout::new::<T>());
            return null_mut();
        }
        realloc(ptr as _, Layout::new::<T>(), new_size)
    }
}
pub unsafe fn allocate<T>(len: usize) -> *mut T {
    std::alloc::realloc(
        ptr::null_mut(),
        Layout::new::<T>(),
        mem::size_of::<T>() * len,
    ) as *mut T
}
