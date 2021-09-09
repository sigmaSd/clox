use crate::chunk::{Chunk, OpCode};

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
            OpCode::OpReturn => simple_instruction("OpReturn", offset),
            OpCode::OpConstant => constant_instuction("OpConstant", chunk, offset),
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
        (*chunk.constants.values.add(constant)).print();
    }
    println!("'");
    offset + 2
}

fn simple_instruction(name: &str, offset: isize) -> isize {
    println!("{}", name);
    offset + 1
}
