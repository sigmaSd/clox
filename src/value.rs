use std::ptr;

use crate::{
    memory::{free_array, grow_array, grow_capacity},
    value::object::{print_object, ObjType},
};

pub mod object;
pub use object::{copy_string, Obj};

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Bool(bool),
    Number(f64),
    // points to a heap allocated Object
    Obj(*const Obj),
    Nil,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::Obj(o1), Self::Obj(o2)) => o1 == o2,
            _ => std::mem::discriminant(self) == std::mem::discriminant(other),
        }
    }
}
impl Value {
    pub fn is_bool(&self) -> bool {
        matches!(self, Value::Bool(_))
    }
    pub fn is_nil(&self) -> bool {
        matches!(self, Value::Nil)
    }
    pub fn is_number(&self) -> bool {
        matches!(self, Value::Number(_))
    }
    pub fn is_obj(&self) -> bool {
        matches!(self, Value::Obj(_))
    }
    pub fn is_string(&self) -> bool {
        unsafe { self.is_obj() && (**crate::AS_OBJ!(self)).is_obj_type(ObjType::String) }
    }
    pub fn is_function(&self) -> bool {
        unsafe { self.is_obj() && (**crate::AS_OBJ!(self)).is_obj_type(ObjType::Function) }
    }
    pub fn is_native(&self) -> bool {
        unsafe { self.is_obj() && (**crate::AS_OBJ!(self)).is_obj_type(ObjType::Native) }
    }
    pub fn is_closure(&self) -> bool {
        unsafe { self.is_obj() && (**crate::AS_OBJ!(self)).is_obj_type(ObjType::Closure) }
    }
    pub fn is_class(&self) -> bool {
        unsafe { self.is_obj() && (**crate::AS_OBJ!(self)).is_obj_type(ObjType::Class) }
    }
    pub fn is_instance(&self) -> bool {
        unsafe { self.is_obj() && (**crate::AS_OBJ!(self)).is_obj_type(ObjType::Instance) }
    }
}

#[macro_export]
macro_rules! AS_VAL_TYPE {
    ($val: expr, $vtype: path) => {{
        if let $vtype(v) = $val {
            v
        } else {
            unreachable!()
        }
    }};
}
#[macro_export]
macro_rules! AS_BOOL {
    ($val: expr) => {
        crate::AS_VAL_TYPE!($val, crate::value::Value::Bool)
    };
}
#[macro_export]
macro_rules! AS_NUMBER {
    ($val: expr) => {
        crate::AS_VAL_TYPE!($val, crate::value::Value::Number)
    };
}
#[macro_export]
macro_rules! AS_OBJ {
    ($val: expr) => {
        crate::AS_VAL_TYPE!($val, crate::value::Value::Obj)
    };
}
#[macro_export]
macro_rules! BOOL_VAL {
    ($val: expr) => {{
        crate::value::Value::Bool($val)
    }};
}
#[macro_export]
macro_rules! NUMBER_VAL {
    ($val: expr) => {{
        crate::value::Value::Number($val)
    }};
}
#[macro_export]
macro_rules! NIL_VAL {
    () => {{
        crate::value::Value::Nil
    }};
}
#[macro_export]
macro_rules! OBJ_VAL {
    ($obj: expr) => {{
        crate::value::Value::Obj($obj as *const Obj)
    }};
}

pub fn print_value(val: Value) {
    match val {
        Value::Bool(b) => print!("{}", b),
        Value::Number(n) => print!("{}", n),
        Value::Nil => print!("nil"),
        Value::Obj(_) => print_object(val),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ValueArray {
    pub count: usize,
    capacity: usize,
    pub values: *mut Value,
}

impl ValueArray {
    pub fn new() -> Self {
        Self {
            count: 0,
            capacity: 0,
            values: ptr::null_mut(),
        }
    }
    pub fn init(&mut self) {
        self.count = 0;
        self.capacity = 0;
        self.values = ptr::null_mut();
    }
    pub fn write(&mut self, value: Value) {
        unsafe {
            if self.capacity < self.count + 1 {
                let old_capacity = self.capacity;
                self.capacity = grow_capacity(old_capacity);
                self.values = grow_array::<Value>(self.values, old_capacity, self.capacity);
            }
            *self.values.add(self.count) = value;
            self.count += 1;
        }
    }
    pub fn free(&mut self) {
        free_array::<Value>(self.values, self.capacity);
        self.init();
    }
}
