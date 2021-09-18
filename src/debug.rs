use crate::{
    chunk::{Chunk, OpCode},
    value::print_value,
    AS_FUNCTION,
};
use std::convert::TryInto;

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

pub unsafe fn disassemble_instruction(chunk: *const Chunk, mut offset: isize) -> isize {
    print!("{:04} ", offset);
    if offset > 0 && (*chunk).lines.offset(offset) == (*chunk).lines.offset(offset - 1) {
        print!("  | ");
    } else {
        print!("{} ", *(*chunk).lines.offset(offset));
    }

    match (*(*chunk).code.offset(offset)).try_into() {
        Ok(instruction) => match instruction {
            OpCode::Return => simple_instruction("OpReturn", offset),
            OpCode::Constant => constant_instuction("OpConstant", &*chunk, offset),
            OpCode::Negate => simple_instruction("OpNegate", offset),
            OpCode::Add => simple_instruction("OpAdd", offset),
            OpCode::Substract => simple_instruction("OpSubstract", offset),
            OpCode::Multiply => simple_instruction("OpMultiply", offset),
            OpCode::Divide => simple_instruction("OpDivide", offset),
            OpCode::Nil => simple_instruction("OpNil", offset),
            OpCode::True => simple_instruction("OpTrue", offset),
            OpCode::False => simple_instruction("OpFalse", offset),
            OpCode::Not => simple_instruction("OpNot", offset),
            OpCode::Equal => simple_instruction("OpEqual", offset),
            OpCode::Greater => simple_instruction("OpGreater", offset),
            OpCode::Less => simple_instruction("OpLess", offset),
            OpCode::Print => simple_instruction("OpPrint", offset),
            OpCode::Pop => simple_instruction("OpPop", offset),
            OpCode::DefineGlobal => simple_instruction("OpDefineGlobal", offset),
            OpCode::GetGlobal => simple_instruction("OpGetGlobal", offset),
            OpCode::SetGlobal => simple_instruction("OpSetGlobal", offset),
            OpCode::GetLocal => byte_instruction("OpGetLocal", chunk, offset),
            OpCode::SetLocal => byte_instruction("OpSetLocal", chunk, offset),
            OpCode::Jump => jump_instruction("OpJump", 1, chunk, offset),
            OpCode::JumpIfFalse => jump_instruction("OpJumpIfFalse", 1, chunk, offset),
            OpCode::Loop => jump_instruction("OpLoop", -1, chunk, offset),
            OpCode::Call => byte_instruction("OpCall", chunk, offset),
            OpCode::Closure => {
                dbg!(offset);
                offset += 1;
                let constant = *(*chunk).code.offset(offset);
                offset += 1;
                print!("OpClosure {} ", constant);
                //dbg!(*(*chunk).constants.values.add(9));
                print_value(*(*chunk).constants.values.add(constant as _));
                println!();

                let function = AS_FUNCTION!(*(*chunk).constants.values.add(constant as _));
                for _ in 0..(*function).upvalue_count {
                    let is_local: bool = (*(*chunk).code.offset(offset)) == 1;
                    offset += 1;
                    let index = *(*chunk).code.offset(offset);
                    offset += 1;
                    println!(
                        "{}    |              {} {}",
                        offset - 2,
                        if is_local { "local" } else { "upvalue" },
                        index
                    );
                }

                dbg!(offset)
            }
            OpCode::GetUpValue => byte_instruction("OpGetUpValue", chunk, offset),
            OpCode::SetUpValue => byte_instruction("OpSetUpValue", chunk, offset),
            OpCode::CloseUpValue => simple_instruction("OpCloseUpValue", offset),
        },
        Err(e) => {
            println!("{}", e);
            offset + 1
        }
    }
}

unsafe fn jump_instruction(name: &str, sign: isize, chunk: *const Chunk, offset: isize) -> isize {
    let mut jump: u16 = ((*(*chunk).code.offset(offset + 1) as u16) << 8) as _;
    jump |= *(*chunk).code.offset(offset + 2) as u16;
    println!(
        "{} {} -> {}",
        name,
        offset,
        offset + 3 + sign * jump as isize
    );
    offset + 3
}

fn byte_instruction(name: &str, chunk: *const Chunk, offset: isize) -> isize {
    let slot = unsafe { *(*chunk).code.offset(offset + 1) };
    println!("{} {}", name, slot);
    offset + 2
}

fn constant_instuction(name: &str, chunk: &Chunk, offset: isize) -> isize {
    let constant: usize = unsafe { *chunk.code.offset(offset + 1) }.into();
    print!("{} {:} '", name, constant);
    unsafe {
        let value = *chunk.constants.values.add(constant);
        print_value(value);
    }
    println!("'");
    offset + 2
}

fn simple_instruction(name: &str, offset: isize) -> isize {
    println!("{}", name);
    offset + 1
}
