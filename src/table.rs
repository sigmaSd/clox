use std::ptr;

use crate::{
    memory::{allocate, free_array, grow_capacity, mark_object, mark_value},
    utils::Helper,
    value::{object::ObjString, Value},
    BOOL_VAL, NIL_VAL,
};

const TABLE_MAX_LOAD: f32 = 0.75;

#[derive(Debug)]
pub struct Table {
    count: usize,
    capacity: usize,
    entries: *mut Entry,
    tombstone: *mut Entry,
}

impl Table {
    pub const fn new() -> Self {
        Self {
            count: 0,
            capacity: 0,
            entries: ptr::null_mut(),
            tombstone: ptr::null_mut(),
        }
    }
    pub fn init(&mut self) {
        self.count = 0;
        self.capacity = 0;
        self.entries = ptr::null_mut();
        self.tombstone = ptr::null_mut();
    }
    pub fn free_table(&mut self) {
        free_array::<Entry>(self.entries, self.capacity);
        self.init();
    }
    pub fn table_get(&mut self, key: *const ObjString) -> Option<Value> {
        unsafe {
            if self.count == 0 {
                return None;
            }

            let entry = self.find_entry(self.entries, self.capacity, key);
            if (*entry).key.is_null() {
                return None;
            }

            Some((*entry).value)
        }
    }
    pub fn table_delete(&mut self, key: *const ObjString) -> bool {
        if self.count == 0 {
            return false;
        }
        unsafe {
            let entry = self.find_entry(self.entries, self.capacity, key);
            if (*entry).key.is_null() {
                return false;
            }

            (*entry).key = ptr::null_mut();
            (*entry).value = BOOL_VAL!(true);
            true
        }
    }

    pub unsafe fn table_set(&mut self, key: *const ObjString, value: Value) -> bool {
        if (self.count + 1) as f32 > self.capacity as f32 * TABLE_MAX_LOAD {
            let capacity = grow_capacity(self.capacity);
            self.adjust_capacity(capacity);
        }

        let mut entry = self.find_entry(self.entries, self.capacity, key);
        let is_new_key = (*entry).key.is_null();
        if is_new_key && (*entry).value.is_nil() {
            self.count += 1;
        }

        (*entry).key = key;
        (*entry).value = value;

        is_new_key
    }

    fn adjust_capacity(&mut self, capacity: usize) {
        unsafe {
            let entries: *mut Entry = allocate(capacity);
            for i in 0..capacity {
                (*entries.add(i)).key = ptr::null_mut();
                (*entries.add(i)).value = NIL_VAL!();
            }

            self.count = 0;
            for i in 0..self.capacity {
                let entry = self.entries.add(i);
                if (*entry).key.is_null() {
                    continue;
                }
                let dest = self.find_entry(entries, capacity, (*entry).key);
                (*dest).key = (*entry).key;
                (*dest).value = (*entry).value;
                self.count += 1;
            }

            free_array(self.entries, self.capacity);
            self.entries = entries;
            self.capacity = capacity;
        }
    }

    fn find_entry(
        &mut self,
        entries: *mut Entry,
        capacity: usize,
        key: *const ObjString,
    ) -> *mut Entry {
        unsafe {
            let mut index = (*key).hash as usize % capacity;
            loop {
                let entry = entries.add(index);
                if (*entry).key.is_null() {
                    if (*entry).value.is_nil() {
                        return if !self.tombstone.is_null() {
                            self.tombstone
                        } else {
                            entry
                        };
                    } else if self.tombstone.is_null() {
                        self.tombstone = entry;
                    }
                } else if (*entry).key == key {
                    return entry;
                }
                index = (index + 1) % capacity;
            }
        }
    }
    unsafe fn _table_add_all(&mut self, other: &mut Table) {
        for i in 0..self.capacity {
            let entry = self.entries.add(i);
            if (*entry).key.is_null() {
                other.table_set((*entry).key, (*entry).value);
            }
        }
    }

    pub(crate) unsafe fn find_string(
        &self,
        chars: *const u8,
        len: usize,
        hash: u32,
    ) -> Option<*const ObjString> {
        if self.count == 0 {
            return None;
        }

        let mut index = hash as usize % self.capacity;
        loop {
            let entry = &*self.entries.add(index);
            if entry.key.is_null() {
                if entry.value.is_nil() {
                    return None;
                }
            } else if (*entry.key).len == len
                && (*entry.key).hash == hash
                && (*entry.key).chars.to_str((*entry.key).len) == chars.to_str(len)
            {
                return Some(entry.key);
            }
            index = (index + 1) % self.capacity;
        }
    }

    pub(crate) unsafe fn remove_white(&mut self) {
        for i in 0..self.capacity {
            let entry = &*self.entries.add(i);
            if !entry.key.is_null() && !(*(*entry).key).obj.is_marked {
                self.table_delete(entry.key);
            }
        }
    }
}

struct Entry {
    key: *const ObjString,
    value: Value,
}

pub unsafe fn mark_table(table: &crate::table::Table) {
    for i in 0..table.capacity {
        let entry = &mut *table.entries.add(i);
        mark_object(entry.key as _);
        mark_value(&mut entry.value);
    }
}
