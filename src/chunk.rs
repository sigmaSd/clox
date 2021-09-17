use crate::{
    memory::{free_array, grow_array, grow_capacity},
    value::Value,
};
use std::{convert::TryFrom, ptr};

use crate::value::ValueArray;

macro_rules! gen_opcode {
($($name: ident)+) => (
    #[derive(Debug)]
    pub enum OpCode {
        $(
            $name,
         )+
    }
    impl TryFrom<u8> for OpCode {
        type Error = String;

        fn try_from(instruction: u8) -> Result<Self, Self::Error> {
            $(
            #[allow(non_upper_case_globals)]
            const $name:u8 = OpCode::$name as u8;
            )+
            #[allow(non_upper_case_globals)]
            match instruction {
                $(
                    $name => Ok(OpCode::$name),
                )+
                instruction => Err(format!("Unknown opcode {}", instruction)),
            }
        }
    }
)}

gen_opcode!(Loop Call Closure CloseUpValue Return Constant Nil True False Pop Equal Greater Less Add Substract Multiply Divide Not Negate Print Jump JumpIfFalse DefineGlobal GetLocal SetLocal GetGlobal SetGlobal GetUpValue SetUpValue);

impl From<OpCode> for u8 {
    fn from(code: OpCode) -> Self {
        code as _
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Chunk {
    pub count: usize,
    capacity: usize,
    pub code: *mut u8,
    pub lines: *mut usize,
    pub constants: ValueArray,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            count: 0,
            capacity: 0,
            lines: ptr::null_mut(),
            code: ptr::null_mut(),
            constants: ValueArray::new(),
        }
    }
    pub fn init(&mut self) {
        self.count = 0;
        self.capacity = 0;
        self.code = ptr::null_mut();
    }
    pub fn write(&mut self, byte: u8, line: usize) {
        unsafe {
            if self.capacity < self.count + 1 {
                let old_capacity = self.capacity;
                self.capacity = grow_capacity(old_capacity);
                self.code = grow_array::<u8>(self.code, old_capacity, self.capacity);
                self.lines = grow_array::<usize>(self.lines, old_capacity, self.capacity);
            }
            *self.code.add(self.count) = byte;
            *self.lines.add(self.count) = line;
            self.count += 1;
        }
    }
    pub fn free(&mut self) {
        free_array::<u8>(self.code, self.capacity);
        free_array::<usize>(self.lines, self.capacity);
        self.constants.free();
        self.init();
    }
    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.write(value);
        self.constants.count - 1
    }
}
