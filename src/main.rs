mod chunk;
use chunk::Chunk;
use debug::disassemble_chunk;
use value::Value;
mod debug;
pub mod memory;
mod value;

use crate::chunk::OpCode;

fn main() {
    let mut c = Chunk::new();
    let constant = c.add_constant(Value(1.2));
    c.write(OpCode::OpConstant as u8, 123);
    c.write(constant, 1);
    c.write(OpCode::OpReturn as u8, 1);
    disassemble_chunk(&c, "test chunk");
}
