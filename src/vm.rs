use std::ptr;
use std::{alloc::Layout, convert::TryInto};

use crate::compile::{compile, U8_COUNT};
use crate::debug::disassemble_instruction;
use crate::memory::free_object;
use crate::table::Table;
use crate::value::object::ObjFunction;
use crate::value::object::{take_string, NativeFn, ObjNative, ObjType};
use crate::value::{copy_string, Obj};
use crate::{
    chunk::OpCode,
    value::{print_value, Value},
    AS_BOOL, AS_NUMBER, AS_STRING, BOOL_VAL, NIL_VAL, NUMBER_VAL,
};
use crate::{AS_FUNCTION, AS_NATIVE, OBJ_TYPE, OBJ_VAL};

macro_rules! runtime_error {
       ($($arg:tt)*) => ({
           eprintln!("{}", format_args!($($arg)*));
           #[allow(unused_unsafe)]
           unsafe {
            for i in (0..VM.frame_count).rev() {
                   let frame = &VM.frames[i];
                   let function = frame.function;
                   let instruction = frame.ip.offset_from((*function).chunk.code) -1;
                   let line = *(*function).chunk.lines.offset(instruction);
                   eprint!("[line {}] in ", line);
                   if (*function).name.is_null() {
                   eprintln!("script");
                   } else {
                   eprintln!("{}()",(*(*function).name).as_str() );
                   }
               }
            VM.reset_stack();
           }
           })
}

const FRAMES_MAX: usize = 64;
const STACK_MAX: usize = FRAMES_MAX * U8_COUNT;
#[derive(Debug)]
pub struct Vm {
    frames: [CallFrame; FRAMES_MAX],
    frame_count: usize,

    stack: [Value; STACK_MAX],
    stack_top: *mut Value,
    pub objects: *mut Obj,
    pub strings: Table,
    pub globals: Table,
}
pub static mut VM: Vm = Vm::new();

#[derive(Debug, Clone, Copy)]
struct CallFrame {
    function: *mut ObjFunction,
    ip: *mut u8,
    slots: *mut Value,
}
impl CallFrame {
    const fn new() -> Self {
        Self {
            function: ptr::null_mut(),
            ip: ptr::null_mut(),
            slots: ptr::null_mut(),
        }
    }
}

impl Vm {
    const fn new() -> Self {
        Self {
            frames: [CallFrame::new(); FRAMES_MAX],
            frame_count: 0,
            stack: [Value::Nil; STACK_MAX],
            stack_top: ptr::null_mut(),
            objects: ptr::null_mut(),
            strings: Table::new(),
            globals: Table::new(),
        }
    }
    pub fn init(&mut self) {
        self.reset_stack();
        self.define_native("clock", clock_native);
    }
    pub fn reset_stack(&mut self) {
        self.stack_top = &mut self.stack as *mut _;
    }

    pub fn interpret(&mut self, source: &str) -> Result<(), InterpretError> {
        unsafe {
            let function = compile(source);
            if function.is_err() {
                return Err(InterpretError::CompileError);
            }
            let function = function.unwrap();

            self.push(OBJ_VAL!(function));
            let _ = self.call(function, 0);

            self.run()
        }
    }
    unsafe fn run(&mut self) -> Result<(), InterpretError> {
        let mut frame: *mut CallFrame = &mut self.frames[self.frame_count - 1] as _;

        macro_rules! READ_BYTE {
            () => {{
                //let opcode: OpCode = (*self.ip).try_into().unwrap();
                let byte = *(*frame).ip;
                (*frame).ip = (*frame).ip.add(1);
                byte
                //opcode
            }};
        }
        macro_rules! READ_CONSTANT {
            () => {{
                *(*(*frame).function)
                    .chunk
                    .constants
                    .values
                    .add(READ_BYTE!() as usize)
            }};
        }
        macro_rules! READ_SHORT {
            () => {{
                (*frame).ip = (*frame).ip.add(2);
                (((*(*frame).ip.offset(-2) as u16) << 8) | (*(*frame).ip.offset(-1) as u16))
            }};
        }
        macro_rules! READ_STRING {
            () => {
                AS_STRING!(READ_CONSTANT!())
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
                while slot < self.stack_top {
                    print!("[ ");
                    print_value(*slot);
                    print!(" ]");

                    slot = slot.add(1);
                }
                println!();
                disassemble_instruction(
                    &(*(*frame).function).chunk,
                    (*frame).ip.offset_from(((*(*frame).function).chunk).code),
                );
            }

            match READ_BYTE!().try_into().unwrap() {
                OpCode::Return => {
                    let result = self.pop();
                    self.frame_count -= 1;
                    if self.frame_count == 0 {
                        self.pop();
                        return Ok(());
                    }
                    self.stack_top = (*frame).slots;
                    self.push(result);
                    frame = &mut self.frames[self.frame_count - 1];
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
                OpCode::Add => {
                    if self.peek(0).is_string() && self.peek(1).is_string() {
                        self.concatenate();
                    } else if self.peek(0).is_number() && self.peek(1).is_number() {
                        let b = AS_NUMBER!(self.pop());
                        let a = AS_NUMBER!(self.pop());
                        self.push(Value::Number(a + b));
                    } else {
                        runtime_error!("Operands must be two numbers or two strings.");
                        return Err(InterpretError::RuntimError);
                    }
                }
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
                OpCode::Print => {
                    print_value(self.pop());
                    println!();
                }
                OpCode::Pop => {
                    self.pop();
                }
                OpCode::DefineGlobal => {
                    let name = READ_STRING!();
                    // Be careful with Garbage collector
                    self.globals.table_set(name, self.peek(0));
                    self.pop();
                }
                OpCode::GetGlobal => {
                    let name = READ_STRING!();
                    if let Some(value) = self.globals.table_get(name) {
                        self.push(value);
                    } else {
                        runtime_error!("Undefined variable '{}'.", (&*name).as_str(),);
                        return Err(InterpretError::RuntimError);
                    }
                }
                OpCode::SetGlobal => {
                    let name = READ_STRING!();
                    if self.globals.table_set(name, self.peek(0)) {
                        self.globals.table_delete(name);
                        runtime_error!("Undefined variable '{}'.", (&*name).as_str(),);
                        return Err(InterpretError::RuntimError);
                    }
                }
                OpCode::GetLocal => {
                    let slot = READ_BYTE!();
                    self.push(*(*frame).slots.add(slot as usize));
                }
                OpCode::SetLocal => {
                    let slot = READ_BYTE!();
                    *(*frame).slots.add(slot as usize) = self.peek(0);
                }
                OpCode::JumpIfFalse => {
                    let offset = READ_SHORT!();
                    if is_falsey(self.peek(0)) {
                        (*frame).ip = (*frame).ip.add(offset as _);
                    }
                }
                OpCode::Jump => {
                    let offset = READ_SHORT!();
                    (*frame).ip = (*frame).ip.add(offset as _);
                }
                OpCode::Loop => {
                    let offset = READ_SHORT!();
                    (*frame).ip = (*frame).ip.offset(-(offset as isize));
                }
                OpCode::Call => {
                    let arg_count = READ_BYTE!() as isize;
                    if call_value(self.peek(arg_count), arg_count).is_err() {
                        return Err(InterpretError::RuntimError);
                    }
                    frame = &mut self.frames[self.frame_count - 1];
                }
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
    fn define_native(&mut self, name: &str, function: NativeFn) {
        unsafe {
            self.push(OBJ_VAL!(copy_string(name as *const str as _, name.len())));
            self.push(OBJ_VAL!(ObjNative::new(function)));
            self.globals
                .table_set(AS_STRING!(self.stack[0]), self.stack[1]);
            self.pop();
            self.pop();
        }
    }
    pub unsafe fn free(&mut self) {
        self.globals.free_table();
        self.strings.free_table();
        self.free_objectes();
    }

    unsafe fn concatenate(&mut self) {
        let b = *AS_STRING!(self.pop());
        let a = *AS_STRING!(self.pop());

        let len = a.len + b.len;
        let chars = std::alloc::realloc(ptr::null_mut(), Layout::new::<u8>(), len);
        ptr::copy_nonoverlapping(a.chars, chars as _, a.len);
        ptr::copy_nonoverlapping(b.chars, chars.add(a.len) as _, b.len);

        let result = take_string(chars, len);
        self.push(OBJ_VAL!(result));
    }

    unsafe fn free_objectes(&mut self) {
        let mut object = self.objects;
        while !object.is_null() {
            let next = (*object).next;
            free_object(object);
            object = next;
        }
    }
    unsafe fn call(&mut self, function: *mut ObjFunction, arg_count: isize) -> Result<(), ()> {
        if arg_count != (*function).arity.try_into().unwrap() {
            runtime_error!(
                "Expected {} arguments but got {}.",
                (*function).arity,
                arg_count
            );
            return Err(());
        }

        if self.frame_count == FRAMES_MAX {
            runtime_error!("Stack overflow.");
            return Err(());
        }

        let frame = &mut self.frames[self.frame_count];
        self.frame_count += 1;
        frame.function = function;
        frame.ip = (*function).chunk.code;
        frame.slots = self.stack_top.offset(-arg_count - 1);
        Ok(())
    }
}

fn call_value(callee: Value, arg_count: isize) -> Result<(), ()> {
    if callee.is_obj() {
        match OBJ_TYPE!(callee) {
            ObjType::Function => return unsafe { VM.call(AS_FUNCTION!(callee), arg_count) },

            ObjType::Native => unsafe {
                let native = AS_NATIVE!(callee);
                let result = native(
                    arg_count.try_into().unwrap(),
                    VM.stack_top.offset(-arg_count),
                );
                VM.push(result);
                return Ok(());
            },
            _ => (),
        }
    }
    runtime_error!("Can only call functions and classes.");
    Err(())
}

fn is_falsey(val: Value) -> bool {
    val.is_nil() || val.is_bool() && !AS_BOOL!(val)
}

#[derive(Debug)]
pub enum InterpretError {
    CompileError,
    RuntimError,
}

fn clock_native(_arg_count: usize, _args: *const Value) -> Value {
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_secs_f64();
    const CLOCKS_PER_SEC: f64 = 1.;
    NUMBER_VAL!(now / CLOCKS_PER_SEC)
}
