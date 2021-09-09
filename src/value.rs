use std::ptr;

use crate::memory::{free_array, grow_array, grow_capacity};

pub struct Value(pub f64);
impl Value {
    pub fn print(&self) {
        print!("{}", self.0)
    }
}

#[derive(Debug)]
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

impl Drop for ValueArray {
    fn drop(&mut self) {
        self.free();
    }
}
