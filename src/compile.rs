use std::{ops::Index, ptr, slice};

use crate::{
    chunk::{Chunk, OpCode},
    debug::disassemble_chunk,
    scanner::{self, Token, TokenType},
};

pub unsafe fn compile(source: &str, chunk: &mut Chunk) -> Result<(), ()> {
    scanner::init_scanner(source);
    COMPILING_CHUNK = chunk;

    advance();
    expression();
    consume(TokenType::EOF, "Expect end of expression.");
    end_compiler();

    if !parser.had_error {
        Ok(())
    } else {
        Err(())
    }
}

unsafe fn expression() {
    parse_presendence(Presendence::ASSIGNMENT);
}

unsafe fn end_compiler() {
    emit_return();
    if cfg!(feature = "DEBUG_PRINT_CODE") && !parser.had_error {
        disassemble_chunk(&*current_chunk(), "code");
    }
}

unsafe fn binary() {
    let operator_type = parser.previous.ttype;
    let rule = get_rule(operator_type);
    parse_presendence((rule.presendence as u8 + 1).try_into().unwrap());

    use TokenType::*;
    match operator_type {
        PLUS => emit_byte(OpCode::Add.into()),
        MINUS => emit_byte(OpCode::Substract.into()),
        STAR => emit_byte(OpCode::Multiply.into()),
        SLASH => emit_byte(OpCode::Divide.into()),
        _ => unreachable!(),
    }
}

fn get_rule(ttype: TokenType) -> &'static ParseRule {
    &RULES[ttype]
}
unsafe fn unary() {
    let operator_type = parser.previous.ttype;

    parse_presendence(Presendence::UNARY);

    use TokenType::*;
    match operator_type {
        MINUS => emit_byte(OpCode::Negate.into()),
        _ => (),
    }
}

unsafe fn grouping() {
    expression();
    consume(TokenType::RIGHT_PAREN, "Expect ')' after expression.");
}

unsafe fn parse_presendence(presendence: Presendence) {
    advance();
    let prefix_rule = get_rule(parser.previous.ttype).prefix;
    if let Some(prefix_rule) = prefix_rule {
        prefix_rule();
    } else {
        error("Expect expression.");
        return;
    }

    while presendence <= get_rule(parser.current.ttype).presendence {
        advance();
        let infix_rule = get_rule(parser.previous.ttype).infix.unwrap();
        infix_rule();
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[allow(clippy::upper_case_acronyms)]
enum Presendence {
    NONE = 0,
    ASSIGNMENT, // =
    OR,         // or
    AND,        // and
    EQUALITY,   // == !=
    COMPARISON, // < > <= >=
    TERM,       // + -
    FACTOR,     // * /
    UNARY,      // ! -
    CALL,       // . ()
    PRIMARY,
}
impl TryInto<Presendence> for u8 {
    type Error = ();

    fn try_into(self) -> Result<Presendence, Self::Error> {
        use Presendence::*;
        match self {
            0 => Ok(NONE),
            1 => Ok(ASSIGNMENT),
            2 => Ok(OR),
            3 => Ok(AND),
            4 => Ok(EQUALITY),
            5 => Ok(COMPARISON),
            6 => Ok(TERM),
            7 => Ok(FACTOR),
            8 => Ok(UNARY),
            9 => Ok(CALL),
            10 => Ok(PRIMARY),
            _ => Err(()),
        }
    }
}

struct ParseRule {
    prefix: Option<ParseFn>,
    infix: Option<ParseFn>,
    presendence: Presendence,
}
type ParseFn = unsafe fn();

unsafe fn consume(ttype: TokenType, message: &str) {
    if parser.current.ttype == ttype {
        advance();
        return;
    }
    error_at_current(message)
}

unsafe fn number() {
    let value: f64 = parser
        .previous
        .start
        .to_str(parser.previous.length)
        .parse()
        .unwrap();
    emit_constant(value)
}

unsafe fn emit_constant(value: f64) {
    emit_bytes(OpCode::Constant.into(), make_constant(value));
}

unsafe fn make_constant(value: f64) -> u8 {
    (*current_chunk()).add_constant(value)
    // FIXME
    // if constant > u8::MAX {
    //     error("Too many constants in one chunk.");
    //     return 0;
    // }
}

unsafe fn error(message: &str) {
    error_at(&parser.previous, message)
}

unsafe fn emit_return() {
    emit_byte(OpCode::Return.into())
}

struct Parser {
    current: Token,
    previous: Token,
    had_error: bool,
    panic_mode: bool,
}
#[allow(non_upper_case_globals)]
static mut parser: Parser = Parser {
    current: Token::new_uninit(),
    previous: Token::new_uninit(),
    had_error: false,
    panic_mode: false,
};
static mut COMPILING_CHUNK: *mut Chunk = ptr::null_mut();

unsafe fn advance() {
    parser.previous = parser.current;

    loop {
        parser.current = scanner::scan_token();
        if parser.current.ttype != TokenType::ERROR {
            break;
        }
        error_at_current(parser.current.start.to_str(parser.current.length))
    }
}

unsafe fn emit_bytes(byte1: u8, byte2: u8) {
    emit_byte(byte1);
    emit_byte(byte2);
}

unsafe fn emit_byte(byte: u8) {
    (*current_chunk()).write(byte, parser.previous.line)
}

unsafe fn current_chunk() -> *mut Chunk {
    COMPILING_CHUNK
}

unsafe fn error_at_current(message: &str) {
    error_at(&parser.current, message);
}

unsafe fn error_at(token: &Token, message: &str) {
    if parser.panic_mode {
        return;
    }
    parser.panic_mode = true;
    eprint!("[line {}] Error", token.line);

    match token.ttype {
        TokenType::EOF => eprint!(" at end"),
        TokenType::ERROR => (), //nothing
        _ => eprint!(" at '{}'", token.start.to_str(token.length)),
    }
    eprintln!(": {}", message);
    parser.had_error = true;
}

trait Helper {
    fn to_str<'a>(self, len: usize) -> &'a str;
}

impl Helper for *const u8 {
    fn to_str<'a>(self, len: usize) -> &'a str {
        unsafe { std::str::from_utf8_unchecked(slice::from_raw_parts(self, len)) }
    }
}

struct Map<const N: usize>([(TokenType, ParseRule); N]);
impl<const N: usize> Index<TokenType> for Map<N> {
    type Output = ParseRule;

    fn index(&self, index: TokenType) -> &Self::Output {
        &self.0.iter().find(|(ttype, _)| ttype == &index).unwrap().1
    }
}

use TokenType::*;
const RULES: Map<40> = Map([
    (
        LEFT_PAREN,
        ParseRule {
            prefix: Some(grouping),
            infix: None,
            presendence: Presendence::NONE,
        },
    ),
    (
        RIGHT_PAREN,
        ParseRule {
            prefix: None,
            infix: None,
            presendence: Presendence::NONE,
        },
    ),
    (
        LEFT_BRACE,
        ParseRule {
            prefix: None,
            infix: None,
            presendence: Presendence::NONE,
        },
    ),
    (
        RIGHT_BRACE,
        ParseRule {
            prefix: None,
            infix: None,
            presendence: Presendence::NONE,
        },
    ),
    (
        COMMA,
        ParseRule {
            prefix: None,
            infix: None,
            presendence: Presendence::NONE,
        },
    ),
    (
        DOT,
        ParseRule {
            prefix: None,
            infix: None,
            presendence: Presendence::NONE,
        },
    ),
    (
        MINUS,
        ParseRule {
            prefix: Some(unary),
            infix: Some(binary),
            presendence: Presendence::TERM,
        },
    ),
    (
        PLUS,
        ParseRule {
            prefix: None,
            infix: Some(binary),
            presendence: Presendence::TERM,
        },
    ),
    (
        SEMICOLON,
        ParseRule {
            prefix: None,
            infix: None,
            presendence: Presendence::NONE,
        },
    ),
    (
        SLASH,
        ParseRule {
            prefix: None,
            infix: Some(binary),
            presendence: Presendence::FACTOR,
        },
    ),
    (
        STAR,
        ParseRule {
            prefix: None,
            infix: Some(binary),
            presendence: Presendence::FACTOR,
        },
    ),
    (
        BANG,
        ParseRule {
            prefix: None,
            infix: None,
            presendence: Presendence::NONE,
        },
    ),
    (
        BANG_EQUAL,
        ParseRule {
            prefix: None,
            infix: None,
            presendence: Presendence::NONE,
        },
    ),
    (
        EQUAL,
        ParseRule {
            prefix: None,
            infix: None,
            presendence: Presendence::NONE,
        },
    ),
    (
        EQUAL_EQUAL,
        ParseRule {
            prefix: None,
            infix: None,
            presendence: Presendence::NONE,
        },
    ),
    (
        GREATER,
        ParseRule {
            prefix: None,
            infix: None,
            presendence: Presendence::NONE,
        },
    ),
    (
        GREATER_EQUAL,
        ParseRule {
            prefix: None,
            infix: None,
            presendence: Presendence::NONE,
        },
    ),
    (
        LESS,
        ParseRule {
            prefix: None,
            infix: None,
            presendence: Presendence::NONE,
        },
    ),
    (
        LESS_EQUAL,
        ParseRule {
            prefix: None,
            infix: None,
            presendence: Presendence::NONE,
        },
    ),
    (
        IDENTIFIER,
        ParseRule {
            prefix: None,
            infix: None,
            presendence: Presendence::NONE,
        },
    ),
    (
        STRING,
        ParseRule {
            prefix: None,
            infix: None,
            presendence: Presendence::NONE,
        },
    ),
    (
        NUMBER,
        ParseRule {
            prefix: Some(number),
            infix: None,
            presendence: Presendence::NONE,
        },
    ),
    (
        AND,
        ParseRule {
            prefix: None,
            infix: None,
            presendence: Presendence::NONE,
        },
    ),
    (
        CLASS,
        ParseRule {
            prefix: None,
            infix: None,
            presendence: Presendence::NONE,
        },
    ),
    (
        ELSE,
        ParseRule {
            prefix: None,
            infix: None,
            presendence: Presendence::NONE,
        },
    ),
    (
        FALSE,
        ParseRule {
            prefix: None,
            infix: None,
            presendence: Presendence::NONE,
        },
    ),
    (
        FOR,
        ParseRule {
            prefix: None,
            infix: None,
            presendence: Presendence::NONE,
        },
    ),
    (
        FUN,
        ParseRule {
            prefix: None,
            infix: None,
            presendence: Presendence::NONE,
        },
    ),
    (
        IF,
        ParseRule {
            prefix: None,
            infix: None,
            presendence: Presendence::NONE,
        },
    ),
    (
        NIL,
        ParseRule {
            prefix: None,
            infix: None,
            presendence: Presendence::NONE,
        },
    ),
    (
        OR,
        ParseRule {
            prefix: None,
            infix: None,
            presendence: Presendence::NONE,
        },
    ),
    (
        PRINT,
        ParseRule {
            prefix: None,
            infix: None,
            presendence: Presendence::NONE,
        },
    ),
    (
        RETURN,
        ParseRule {
            prefix: None,
            infix: None,
            presendence: Presendence::NONE,
        },
    ),
    (
        SUPER,
        ParseRule {
            prefix: None,
            infix: None,
            presendence: Presendence::NONE,
        },
    ),
    (
        THIS,
        ParseRule {
            prefix: None,
            infix: None,
            presendence: Presendence::NONE,
        },
    ),
    (
        TRUE,
        ParseRule {
            prefix: None,
            infix: None,
            presendence: Presendence::NONE,
        },
    ),
    (
        VAR,
        ParseRule {
            prefix: None,
            infix: None,
            presendence: Presendence::NONE,
        },
    ),
    (
        WHILE,
        ParseRule {
            prefix: None,
            infix: None,
            presendence: Presendence::NONE,
        },
    ),
    (
        ERROR,
        ParseRule {
            prefix: None,
            infix: None,
            presendence: Presendence::NONE,
        },
    ),
    (
        EOF,
        ParseRule {
            prefix: None,
            infix: None,
            presendence: Presendence::NONE,
        },
    ),
]);
