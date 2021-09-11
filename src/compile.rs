use std::slice;

use crate::scanner::{self, TokenType};

pub unsafe fn compile(source: &str) {
    scanner::init_scanner(source);
    let mut line = None;
    loop {
        let token = scanner::scan_token();
        if Some(token.line) != line {
            print!("{:04} ", token.line);
            line = Some(token.line);
        } else {
            print!("   | ");
        }
        println!(
            "{:?} '{}'",
            &token.ttype,
            String::from_utf8_lossy(slice::from_raw_parts(token.start, token.length))
        );
        if token.ttype == TokenType::EOF {
            break;
        }
    }
}
