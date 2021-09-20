use std::{
    alloc::{dealloc, realloc, Layout},
    mem, process,
    ptr::{self, null_mut},
};

use crate::{
    compile::mark_compiler_roots,
    table::mark_table,
    utils::PointerDeref,
    value::{
        object::{
            ObjBoundMethod, ObjClass, ObjClosure, ObjFunction, ObjInstance, ObjNative, ObjString,
            ObjType, ObjUpValue,
        },
        print_value, Obj, Value,
    },
    vm::VM,
    AS_OBJ, OBJ_VAL,
};

pub unsafe fn free_objectes() {
    let mut object = VM.objects;
    while !object.is_null() {
        let next = (*object).next;
        free_object(object);
        object = next;
    }

    free::<*mut Obj>(VM.gray_stack as _);
}
pub unsafe fn free_object(obj: *const Obj) {
    if cfg!(feature = "DEBUG_LOG_GC") {
        println!("{:p} free type {:?}", obj, (*obj).otype);
    }
    match (*obj).otype {
        ObjType::String => {
            let string: *const ObjString = obj as _;
            let string = *string;
            free_array(string.chars as *mut u8, string.len);
            free::<ObjString>(obj as _);
        }
        ObjType::Function => {
            let function: *mut ObjFunction = obj as _;
            (*function).chunk.free();
            free::<ObjFunction>(obj as _);
        }
        ObjType::Native => {
            free::<ObjNative>(obj as _);
        }
        ObjType::Closure => {
            let closure: *mut ObjClosure = obj as _;
            free_array((*closure).upvalues, (*closure).upvalue_count);
            free::<ObjClosure>(obj as _);
        }
        ObjType::UpValue => {
            free::<ObjUpValue>(obj as _);
        }
        ObjType::Class => {
            let class: *mut ObjClass = obj as _;
            class.deref_mut().methods.free_table();
            free::<ObjClass>(obj as _);
        }
        ObjType::Instance => {
            let instance: *mut ObjInstance = obj as _;
            (*instance).fields.free_table();
            free::<ObjInstance>(obj as _);
        }
        ObjType::BoundMethod => {
            let bound: *mut ObjBoundMethod = obj as _;
            mark_value(&mut bound.deref_mut().receiver);
            mark_object(bound.deref().method as _);
        }
    }
}

unsafe fn free<T>(obj: *mut u8) {
    realloc(obj, Layout::new::<T>(), 0);
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

fn reallocate<T>(ptr: *mut T, old_size: usize, new_size: usize) -> *mut u8 {
    unsafe {
        let diff = new_size as isize - old_size as isize;
        if diff > 0 {
            VM.bytes_allocated += diff as usize;
        } else {
            VM.bytes_allocated = VM.bytes_allocated.saturating_sub(diff.abs() as usize);
        }

        if new_size > old_size {
            if cfg!(feature = "DEBUG_STRESS_GC") {
                collect_garbage();
            }

            if VM.bytes_allocated > VM.next_gc {
                collect_garbage();
            }
        }

        if new_size == 0 {
            dealloc(ptr as _, Layout::new::<T>());
            return null_mut();
        }
        realloc(ptr as _, Layout::new::<T>(), new_size)
    }
}

pub unsafe fn collect_garbage() {
    #[cfg(feature = "DEBUG_LOG_GC")]
    println!("-- gc begin");
    #[cfg(feature = "DEBUG_LOG_GC")]
    let before = VM.bytes_allocated;

    mark_roots();
    trace_references();
    VM.strings.remove_white();
    sweep();

    const GC_HEAP_GROW_FACTOR: usize = 2;
    VM.next_gc = VM.bytes_allocated * GC_HEAP_GROW_FACTOR;

    #[cfg(feature = "DEBUG_LOG_GC")]
    {
        println!("-- gc end");
        println!(
            "-- collected {} bytes (from {} to {}) next at {}",
            before as isize - VM.bytes_allocated as isize,
            before,
            VM.bytes_allocated,
            VM.next_gc
        );
    }
}

unsafe fn sweep() {
    let mut previous = ptr::null_mut();
    let mut object = VM.objects;
    while !object.is_null() {
        if (*object).is_marked {
            (*object).is_marked = false;
            previous = object;
            object = (*object).next;
        } else {
            let unreached = object;
            object = (*object).next;
            if !previous.is_null() {
                (*previous).next = object;
            } else {
                VM.objects = object;
            }

            free_object(unreached);
        }
    }
}

unsafe fn trace_references() {
    while VM.gray_count > 0 {
        VM.gray_count -= 1;
        let object = *VM.gray_stack.add(VM.gray_count);
        blacken_object(object);
    }
}

unsafe fn blacken_object(object: *mut Obj) {
    if cfg!(feature = "DEBUG_LOG_GC") {
        print!("{:p} blacken", object);
        print_value(OBJ_VAL!(object));
        println!();
    }

    match (*object).otype {
        ObjType::Native => (),
        ObjType::String => (),
        ObjType::UpValue => mark_value(&mut (*(object as *mut ObjUpValue)).closed),
        ObjType::Function => {
            let function: *mut ObjFunction = object as _;
            mark_object((*function).name as _);
            mark_array(&(*function).chunk.constants);
        }
        ObjType::Closure => {
            let closure: *mut ObjClosure = object as _;
            mark_object((*closure).function as _);
            for i in 0..(*closure).upvalue_count {
                mark_object((*(*closure).upvalues.add(i)) as _);
            }
        }
        ObjType::Class => {
            let klass: *mut ObjClass = object as _;
            mark_object((*klass).name as _);
        }
        ObjType::Instance => {
            let instance: *mut ObjInstance = object as _;
            mark_object((*instance).klass as _);
            mark_table(&(*instance).fields);
        }
        ObjType::BoundMethod => {
            let bound: *mut ObjBoundMethod = object as _;
            mark_value(&mut bound.deref_mut().receiver);
            mark_object(bound.deref().method as _);
        }
    }
}

unsafe fn mark_array(array: &crate::value::ValueArray) {
    for i in 0..array.count {
        mark_value(array.values.add(i));
    }
}

unsafe fn mark_roots() {
    {
        let mut slot: *mut Value = &mut VM.stack as *mut _;
        loop {
            if slot >= VM.stack_top {
                break;
            }
            mark_value(slot);
            slot = slot.add(1);
        }
    }

    for i in 0..VM.frame_count {
        mark_object(VM.frames[i].closure as _);
    }

    {
        let mut upvalue = VM.open_upvalues;
        loop {
            if upvalue.is_null() {
                break;
            }
            mark_object(upvalue as _);
            upvalue = (*upvalue).next;
        }
    }

    mark_table(&VM.globals);
    mark_compiler_roots();
    mark_object(VM.init_string as _);
}

pub unsafe fn mark_value(value: *mut Value) {
    if (*value).is_obj() {
        mark_object(AS_OBJ!(*value) as *mut _);
    }
}

pub unsafe fn mark_object(object: *mut Obj) {
    if object.is_null() {
        return;
    }
    if (*object).is_marked {
        return;
    }
    if cfg!(feature = "DEBUG_LOG_GC") {
        print!("{:p} mark", object);
        print_value(OBJ_VAL!(object));
        println!();
    }
    (*object).is_marked = true;

    if VM.gray_capacity < VM.gray_count.saturating_sub(1) {
        VM.gray_capacity = grow_capacity(VM.gray_capacity);
        VM.gray_stack = realloc(
            VM.gray_stack as _,
            Layout::new::<*mut Obj>(),
            mem::size_of::<*mut Obj>() * VM.gray_capacity,
        ) as _;
        if VM.gray_stack.is_null() {
            process::exit(1)
        }
    }
}

pub unsafe fn allocate<T>(len: usize) -> *mut T {
    std::alloc::realloc(
        ptr::null_mut(),
        Layout::new::<T>(),
        mem::size_of::<T>() * len,
    ) as *mut T
}
