use crate::{
    chunk::{Chunk, OpCode},
    value::print_value,
};

pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {} ==", name);

    let mut offset = 0;
    loop {
        if offset as usize >= chunk.count {
            break;
        }
        unsafe {
            offset = disassemble_instruction(chunk, offset);
        }
    }
}

unsafe fn disassemble_instruction(chunk: &Chunk, offset: isize) -> isize {
    print!("{:04} ", offset);
    if offset > 0 && *chunk.lines.offset(offset) == *chunk.lines.offset(offset - 1) {
        print!("  | ");
    } else {
        print!("{} ", *chunk.lines.offset(offset));
    }

    match (*chunk.code.offset(offset)).try_into() {
        Ok(instruction) => match instruction {
            OpCode::Return => simple_instruction("OpReturn", offset),
            OpCode::Constant => constant_instuction("OpConstant", chunk, offset),
            OpCode::Negate => simple_instruction("OpNegate", offset),
            OpCode::Add => simple_instruction("OpAdd", offset),
            OpCode::Substract => simple_instruction("OpSubstract", offset),
            OpCode::Multiply => simple_instruction("OpMultiply", offset),
            OpCode::Divide => simple_instruction("OpDivide", offset),
        },
        Err(e) => {
            println!("{}", e);
            offset + 1
        }
    }
}

fn constant_instuction(name: &str, chunk: &Chunk, offset: isize) -> isize {
    let constant: usize = unsafe { *chunk.code.offset(offset + 1) }.into();
    print!("{} {:} '", name, constant);
    unsafe {
        let value = *chunk.constants.values.add(constant);
        print_value(&value);
    }
    println!("'");
    offset + 2
}

fn simple_instruction(name: &str, offset: isize) -> isize {
    println!("{}", name);
    offset + 1
}
