use std::ptr;

use crate::{
    chunk::{Chunk, OpCode},
    value::{print_value, Value},
};

const STACK_MAX: usize = 256;

pub struct Vm {
    chunk: *const Chunk,
    ip: *mut u8,
    stack: [Value; STACK_MAX],
    stack_top: *mut Value,
}

impl Vm {
    pub fn new() -> Self {
        let mut this = Self {
            chunk: ptr::null(),
            ip: ptr::null_mut(),
            stack: unsafe { std::mem::zeroed() },
            stack_top: ptr::null_mut(),
        };
        this.init();
        this
    }
    fn init(&mut self) {
        self.reset_stack();
    }
    pub fn reset_stack(&mut self) {
        self.stack_top = &mut self.stack as *mut _;
    }

    pub fn interpret(&mut self, chunk: &Chunk) -> Result<(), InterpretError> {
        unsafe {
            self.chunk = chunk;
            self.ip = (*self.chunk).code;
            self.run()
        }
    }
    unsafe fn run(&mut self) -> Result<(), InterpretError> {
        macro_rules! READ_BYTE {
            () => {{
                let opcode: OpCode = (*self.ip).try_into().unwrap();
                self.ip = self.ip.add(1);
                opcode
            }};
        }
        macro_rules! READ_CONSTANT {
            () => {
                *(*self.chunk).constants.values.add(READ_BYTE!() as usize)
            };
        }
        macro_rules! BINARY_OP {
                ($op: tt) => {
                    {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a $op b);
                    }
                }
            }

        loop {
            if cfg!(feature = "DEBUG_TRACE_EXECUTION") {
                print!("           ");
                let mut slot: *mut Value = &mut self.stack as _;
                loop {
                    print!("[ ");
                    print_value(&*slot);
                    print!(" ]");

                    slot = slot.add(1);
                    if slot == self.stack_top {
                        break;
                    }
                }
                println!();
                //   disassemble_instruction(self.chunk, self.ip - self.chunk.code);
            }

            match READ_BYTE!() {
                OpCode::Return => {
                    print_value(&self.pop());
                    println!();
                    return Ok(());
                }
                OpCode::Constant => {
                    let constant: Value = READ_CONSTANT!();
                    self.push(constant);
                }
                OpCode::Negate => {
                    let value = self.pop();
                    self.push(-value);
                }
                OpCode::Add => BINARY_OP!(+),
                OpCode::Substract => BINARY_OP!(-),
                OpCode::Multiply => BINARY_OP!(*),
                OpCode::Divide => BINARY_OP!(/),
            }
        }
    }
    fn pop(&mut self) -> Value {
        unsafe {
            self.stack_top = self.stack_top.sub(1);
            *self.stack_top
        }
    }

    fn push(&mut self, value: Value) {
        unsafe {
            *self.stack_top = value;
            self.stack_top = self.stack_top.add(1);
        }
    }
}

#[derive(Debug)]
pub enum InterpretError {
    CompileError,
    RuntimError,
}
