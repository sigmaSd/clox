use std::convert::TryInto;
use std::ptr;

use crate::{
    chunk::{Chunk, OpCode},
    compile,
    value::{print_value, Value},
    AS_BOOL, AS_NUMBER, BOOL_VAL, NIL_VAL, NUMBER_VAL,
};

const STACK_MAX: usize = 256;

macro_rules! runtime_error {
       ($($arg:tt)*) => ({
           eprintln!("{}", format_args!($($arg)*));
           let instruction = VM.ip.offset_from((*VM.chunk).code) - 1;
           let line = *(*VM.chunk).lines.offset(instruction);
           eprintln!("[line {}] in script", line);
           VM.reset_stack();
           })
}

#[derive(Debug)]
pub struct Vm {
    chunk: *const Chunk,
    ip: *mut u8,
    stack: [Value; STACK_MAX],
    stack_top: *mut Value,
}
pub static mut VM: Vm = Vm::new();

impl Vm {
    const fn new() -> Self {
        Self {
            chunk: ptr::null(),
            ip: ptr::null_mut(),
            stack: [Value::Nil; STACK_MAX],
            stack_top: ptr::null_mut(),
        }
    }
    pub fn init(&mut self) {
        self.reset_stack();
    }
    pub fn reset_stack(&mut self) {
        self.stack_top = &mut self.stack as *mut _;
    }

    pub fn interpret(&mut self, source: &str) -> Result<(), InterpretError> {
        unsafe {
            let mut chunk = Chunk::new();
            if compile::compile(source, &mut chunk).is_err() {
                return Err(InterpretError::CompileError);
            }

            self.chunk = &chunk;
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
                ($vtype:path, $op: tt) => {
                    {
                    if !self.peek(0).is_number() || !self.peek(1).is_number() {
                        runtime_error!("Operands must be numbers.");
                        return Err(InterpretError::RuntimError);
                    }
                    let b = AS_NUMBER!(self.pop());
                    let a = AS_NUMBER!(self.pop());
                    self.push($vtype(a $op b));
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
                    if !self.peek(0).is_number() {
                        runtime_error!("Operand must be a number.");
                        return Err(InterpretError::RuntimError);
                    }
                    let value = AS_NUMBER!(self.pop());
                    self.push(NUMBER_VAL!(-value));
                }
                OpCode::Add => BINARY_OP!(Value::Number, +),
                OpCode::Substract => BINARY_OP!(Value::Number, -),
                OpCode::Multiply => BINARY_OP!(Value::Number, *),
                OpCode::Divide => BINARY_OP!(Value::Number, /),
                OpCode::Nil => self.push(NIL_VAL!()),
                OpCode::True => self.push(BOOL_VAL!(true)),
                OpCode::False => self.push(BOOL_VAL!(false)),
                OpCode::Not => {
                    let val = is_falsey(self.pop());
                    self.push(BOOL_VAL!(val));
                }
                OpCode::Equal => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(BOOL_VAL!(a == b));
                }
                OpCode::Greater => BINARY_OP!(Value::Bool, >),
                OpCode::Less => BINARY_OP!(Value::Bool, <),
            }
        }
    }
    fn pop(&mut self) -> Value {
        unsafe {
            self.stack_top = self.stack_top.sub(1);
            *self.stack_top
        }
    }
    unsafe fn peek(&self, distance: isize) -> Value {
        *self.stack_top.offset(-1 - distance)
    }

    fn push(&mut self, value: Value) {
        unsafe {
            *self.stack_top = value;
            self.stack_top = self.stack_top.add(1);
        }
    }
    pub fn free(&mut self) {}
}

fn is_falsey(val: Value) -> bool {
    val.is_nil() || val.is_bool() && !AS_BOOL!(val)
}

#[derive(Debug)]
pub enum InterpretError {
    CompileError,
    RuntimError,
}
