use std::{
    alloc::{dealloc, realloc, Layout},
    mem, process,
    ptr::null_mut,
};

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
    reallocate(ptr as *mut u8, mem::size_of::<T>() * old_count, 0);
}

const ARRAY_LAYOUT: Layout = Layout::new::<u8>();

fn reallocate(ptr: *mut u8, _old_size: usize, new_size: usize) -> *mut u8 {
    unsafe {
        if new_size == 0 {
            dealloc(ptr, ARRAY_LAYOUT);
            return null_mut();
        }
        realloc(ptr, ARRAY_LAYOUT, new_size)
    }
}
