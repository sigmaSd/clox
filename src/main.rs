mod chunk;
use chunk::Chunk;
use debug::disassemble_chunk;
use value::Value;
use vm::Vm;
mod debug;
pub mod memory;
mod value;
mod vm;

use crate::chunk::OpCode;

fn main() {
    let mut vm = Vm::new();
    let mut c = Chunk::new();

    let constant = c.add_constant(1.2);
    c.write(OpCode::Constant as u8, 123);
    c.write(constant, 123);

    let constant = c.add_constant(3.4);
    c.write(OpCode::Constant.into(), 123);
    c.write(constant, 123);

    c.write(OpCode::Add.into(), 123);

    let constant = c.add_constant(5.6);
    c.write(OpCode::Constant.into(), 123);
    c.write(constant, 123);

    c.write(OpCode::Divide.into(), 123);
    c.write(OpCode::Negate.into(), 123);

    c.write(OpCode::Return as u8, 123);
    //disassemble_chunk(&c, "test chunk");
    vm.interpret(&c).unwrap();
}
