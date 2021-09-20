use std::ptr;
use std::{alloc::Layout, convert::TryInto};

use crate::compile::{compile, U8_COUNT};
use crate::debug::disassemble_instruction;
use crate::memory::free_objectes;
use crate::table::Table;
use crate::utils::PointerDeref;
use crate::value::object::{
    take_string, NativeFn, ObjBoundMethod, ObjInstance, ObjNative, ObjString, ObjType, ObjUpValue,
};
use crate::value::object::{ObjClass, ObjClosure};
use crate::value::{copy_string, Obj};
use crate::{
    chunk::OpCode,
    value::{print_value, Value},
    AS_BOOL, AS_NUMBER, AS_STRING, BOOL_VAL, NIL_VAL, NUMBER_VAL,
};
use crate::{
    AS_BOUND_METHOD, AS_CLASS, AS_CLOSURE, AS_FUNCTION, AS_INSTANCE, AS_NATIVE, OBJ_TYPE, OBJ_VAL,
};

macro_rules! runtime_error {
       ($($arg:tt)*) => ({
           eprintln!("{}", format_args!($($arg)*));
           #[allow(unused_unsafe)]
           unsafe {
            for i in (0..VM.frame_count).rev() {
                   let frame = &VM.frames[i];
                   let function = (*frame.closure).function;
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
    pub frames: [CallFrame; FRAMES_MAX],
    pub frame_count: usize,

    pub stack: [Value; STACK_MAX],
    pub stack_top: *mut Value,

    pub bytes_allocated: usize,
    pub next_gc: usize,

    pub objects: *mut Obj,
    pub strings: Table,
    pub open_upvalues: *mut ObjUpValue,
    pub globals: Table,
    pub gray_count: usize,
    pub gray_capacity: usize,
    pub gray_stack: *mut *mut Obj,
    pub init_string: *mut ObjString,
}
pub static mut VM: Vm = Vm::new();

#[derive(Debug, Clone, Copy)]
pub struct CallFrame {
    pub closure: *mut ObjClosure,
    ip: *mut u8,
    slots: *mut Value,
}
impl CallFrame {
    const fn new() -> Self {
        Self {
            closure: ptr::null_mut(),
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
            open_upvalues: ptr::null_mut(),
            gray_count: 0,
            gray_capacity: 0,
            gray_stack: ptr::null_mut(),
            bytes_allocated: 0,
            next_gc: 1024 * 1024,
            init_string: ptr::null_mut(),
        }
    }
    pub fn init(&mut self) {
        self.reset_stack();
        self.define_native("clock", clock_native);
        self.init_string = ptr::null_mut();
        self.init_string = copy_string("init" as *const str as _, 4) as _;
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
            let closure = ObjClosure::new(function);
            self.pop();
            self.push(OBJ_VAL!(closure));
            let _ = self.call(closure, 0);

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
                *(*(*(*frame).closure).function)
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
                    &(*(*(*frame).closure).function).chunk,
                    (*frame)
                        .ip
                        .offset_from((*(*(*frame).closure).function).chunk.code),
                );
            }

            match READ_BYTE!().try_into().unwrap() {
                OpCode::Return => {
                    let result = self.pop();
                    self.close_upvalues((*frame).slots);
                    //FIXME
                    self.frame_count = self.frame_count.saturating_sub(1);
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
                OpCode::Closure => {
                    let function = AS_FUNCTION!(READ_CONSTANT!());
                    let closure = ObjClosure::new(function);
                    self.push(OBJ_VAL!(closure));
                    for i in 0..(*closure).upvalue_count {
                        let is_local = READ_BYTE!();
                        let index = READ_BYTE!();
                        if is_local != 0 {
                            *(*closure).upvalues.add(i) =
                                capture_upvalue((*frame).slots.add(index as _));
                        } else {
                            *(*closure).upvalues.add(i) =
                                *(*(*frame).closure).upvalues.add(index as _);
                        }
                    }
                }
                OpCode::GetUpValue => {
                    let slot = READ_BYTE!();
                    self.push(*(*(*(*(*frame).closure).upvalues.add(slot as _))).location);
                }
                OpCode::SetUpValue => {
                    let slot = READ_BYTE!();
                    *(*(*(*(*frame).closure).upvalues.add(slot as _))).location = self.peek(0);
                }
                OpCode::CloseUpValue => {
                    self.close_upvalues(self.stack_top.offset(-1));
                    self.pop();
                }
                OpCode::Class => {
                    self.push(OBJ_VAL!(ObjClass::new(READ_STRING!())));
                }
                OpCode::GetProperty => {
                    if !self.peek(0).is_instance() {
                        runtime_error!("Only instances have properties.");
                        return Err(InterpretError::RuntimError);
                    }

                    let instance = AS_INSTANCE!(self.peek(0));
                    let name = READ_STRING!();

                    if let Some(value) = (*instance).fields.table_get(name) {
                        self.pop();
                        self.push(value);
                        continue;
                    }

                    if self.bind_method(instance.deref().klass, name).is_err() {
                        runtime_error!("Undefined property '{}'.", name.deref().as_str());
                        return Err(InterpretError::RuntimError);
                    }
                }
                OpCode::SetProperty => {
                    if !self.peek(1).is_instance() {
                        runtime_error!("Only instances have fields.");
                        return Err(InterpretError::RuntimError);
                    }
                    let instance = AS_INSTANCE!(self.peek(1));
                    (*instance).fields.table_set(READ_STRING!(), self.peek(0));
                    let value = self.pop();
                    self.pop();
                    self.push(value);
                }
                OpCode::Method => {
                    self.define_method(READ_STRING!());
                }
                OpCode::Invoke => {
                    let method = READ_STRING!();
                    let arg_count = READ_BYTE!();
                    if self.invoke(method, arg_count).is_err() {
                        return Err(InterpretError::RuntimError);
                    }
                    frame = &mut self.frames[self.frame_count - 1];
                }
            }
        }
    }
    pub fn pop(&mut self) -> Value {
        unsafe {
            self.stack_top = self.stack_top.sub(1);
            *self.stack_top
        }
    }
    fn peek(&self, distance: isize) -> Value {
        unsafe { *self.stack_top.offset(-1 - distance) }
    }

    pub fn push(&mut self, value: Value) {
        unsafe {
            *self.stack_top = value;
            self.stack_top = self.stack_top.add(1);
        }
    }
    fn define_native(&mut self, name: &str, function: NativeFn) {
        self.push(OBJ_VAL!(copy_string(name as *const str as _, name.len())));
        self.push(OBJ_VAL!(ObjNative::new(function)));
        self.globals
            .table_set(AS_STRING!(self.stack[0]), self.stack[1]);
        self.pop();
        self.pop();
    }
    pub unsafe fn free(&mut self) {
        self.globals.free_table();
        self.strings.free_table();
        self.init_string = ptr::null_mut();
        free_objectes();
    }

    unsafe fn concatenate(&mut self) {
        let b = *AS_STRING!(self.peek(0));
        let a = *AS_STRING!(self.peek(1));

        let len = a.len + b.len;
        let chars = std::alloc::realloc(ptr::null_mut(), Layout::new::<u8>(), len);
        ptr::copy_nonoverlapping(a.chars, chars as _, a.len);
        ptr::copy_nonoverlapping(b.chars, chars.add(a.len) as _, b.len);

        let result = take_string(chars, len);
        self.pop();
        self.pop();
        self.push(OBJ_VAL!(result));
    }

    unsafe fn call(&mut self, closure: *mut ObjClosure, arg_count: isize) -> Result<(), ()> {
        if arg_count != (*(*closure).function).arity.try_into().unwrap() {
            runtime_error!(
                "Expected {} arguments but got {}.",
                (*(*closure).function).arity,
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
        frame.closure = closure;
        frame.ip = (*(*closure).function).chunk.code;
        frame.slots = self.stack_top.offset(-arg_count - 1);
        Ok(())
    }

    pub(crate) fn close_upvalues(&mut self, last: *mut Value) {
        unsafe {
            while !self.open_upvalues.is_null() && (*self.open_upvalues).location >= last {
                let upvalue = self.open_upvalues;
                (*upvalue).closed = *(*upvalue).location;
                (*upvalue).location = &mut (*upvalue).closed;
                self.open_upvalues = (*upvalue).next;
            }
        }
    }

    pub(crate) fn define_method(&mut self, name: *const crate::value::object::ObjString) {
        let method = self.peek(0);
        let class = AS_CLASS!(self.peek(1));
        class.deref_mut().methods.table_set(name, method);
        self.pop();
    }

    pub(crate) fn bind_method(
        &mut self,
        klass: *const ObjClass,
        name: *const crate::value::object::ObjString,
    ) -> Result<(), InterpretError> {
        if let Some(method) = klass.deref().methods.table_get(name) {
            let bound = ObjBoundMethod::new(self.peek(0), AS_CLOSURE!(method));
            self.pop();
            self.push(OBJ_VAL!(bound));
            Ok(())
        } else {
            Err(InterpretError::RuntimError)
        }
    }

    unsafe fn invoke(&mut self, name: *const ObjString, arg_count: u8) -> Result<(), ()> {
        let receiver = self.peek(arg_count as _);

        if !receiver.is_instance() {
            runtime_error!("Only instances have methods.");
            return Err(());
        }
        let instance = AS_INSTANCE!(receiver);

        if let Some(value) = instance.deref().fields.table_get(name) {
            *self.stack_top.offset(-(arg_count as isize) - 1) = value;
            return call_value(value, arg_count as _);
        }

        self.invoke_from_class(instance.deref().klass, name, arg_count)
    }

    unsafe fn invoke_from_class(
        &mut self,
        klass: *const ObjClass,
        name: *const ObjString,
        arg_count: u8,
    ) -> Result<(), ()> {
        if let Some(method) = klass.deref().methods.table_get(name) {
            return self.call(AS_CLOSURE!(method), arg_count as _);
        }
        runtime_error!("Undefined property '{}'.", name.deref().as_str());
        Err(())
    }
}

unsafe fn capture_upvalue(local: *mut Value) -> *mut crate::value::object::ObjUpValue {
    let mut prev_upvalue = ptr::null_mut();
    let mut upvalue = VM.open_upvalues;
    while !upvalue.is_null() && (*upvalue).location > local {
        prev_upvalue = upvalue;
        upvalue = (*upvalue).next;
    }

    if !upvalue.is_null() && (*upvalue).location == local {
        return upvalue;
    }

    let created_upvalue = ObjUpValue::new(local);
    (*created_upvalue).next = upvalue;
    if prev_upvalue.is_null() {
        VM.open_upvalues = created_upvalue;
    } else {
        (*prev_upvalue).next = created_upvalue;
    }
    created_upvalue
}

unsafe fn call_value(callee: Value, arg_count: isize) -> Result<(), ()> {
    if callee.is_obj() {
        match OBJ_TYPE!(callee) {
            ObjType::Closure => return VM.call(AS_CLOSURE!(callee), arg_count),

            ObjType::Native => {
                let native = AS_NATIVE!(callee);
                let result = native(
                    arg_count.try_into().unwrap(),
                    VM.stack_top.offset(-arg_count),
                );
                VM.push(result);
                return Ok(());
            }
            ObjType::Class => {
                let klass = AS_CLASS!(callee);
                *VM.stack_top.offset(-arg_count - 1) = OBJ_VAL!(ObjInstance::new(klass));
                if let Some(initalizer) = klass.deref().methods.table_get(VM.init_string) {
                    return VM.call(AS_CLOSURE!(initalizer), arg_count);
                } else if arg_count != 0 {
                    runtime_error!("Expected 0 arguments but got {}.", arg_count);
                    return Err(());
                }
                return Ok(());
            }
            ObjType::BoundMethod => {
                let bound: *mut ObjBoundMethod = AS_BOUND_METHOD!(callee);
                *VM.stack_top.offset(-arg_count - 1) = bound.deref().receiver;
                return VM.call(bound.deref().method, arg_count);
            }
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
