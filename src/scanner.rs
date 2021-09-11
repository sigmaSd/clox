use core::slice;
use std::ptr;

#[derive(Debug)]
pub struct Scanner {
    start: *const u8,
    end: *const u8,
    current: *const u8,
    line: usize,
}

impl Scanner {
    const fn new() -> Self {
        Self {
            start: ptr::null(),
            end: ptr::null(),
            current: ptr::null(),
            line: 0,
        }
    }
}
#[allow(non_upper_case_globals)]
pub static mut scanner: Scanner = Scanner::new();

pub unsafe fn init_scanner(source: &str) {
    scanner.start = source as *const str as *const u8;
    scanner.end = scanner.start.add(source.len());
    scanner.current = source as *const str as *const u8;
    scanner.line = 1;
}

#[derive(Debug)]
pub struct Token {
    pub ttype: TokenType,
    pub start: *const u8,
    pub length: usize,
    pub line: usize,
}
pub unsafe fn scan_token() -> Token {
    use TokenType::*;
    skip_whitespace();

    scanner.start = scanner.current;
    if is_at_end() {
        return make_token(EOF);
    }

    let c = advance();
    match c {
        '(' => make_token(LEFT_PAREN),
        ')' => make_token(RIGHT_PAREN),
        '{' => make_token(LEFT_BRACE),
        '}' => make_token(RIGHT_BRACE),
        ';' => make_token(SEMICOLON),
        ',' => make_token(COMMA),
        '.' => make_token(DOT),
        '-' => make_token(MINUS),
        '+' => make_token(PLUS),
        '/' => make_token(SLASH),
        '*' => make_token(STAR),

        '!' => make_token(if tmatch('=') { BANG_EQUAL } else { BANG }),
        '=' => make_token(if tmatch('=') { EQUAL_EQUAL } else { EQUAL }),
        '<' => make_token(if tmatch('=') { LESS_EQUAL } else { LESS }),
        '>' => make_token(if tmatch('=') { GREATER_EQUAL } else { GREATER }),
        '"' => string(),
        c if c.is_digit(10) => number(),
        c if is_alpha(c) => identifier(),

        _ => error_token("Unexpected character."),
    }
}

fn is_alpha(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

unsafe fn identifier() -> Token {
    while is_alpha(peek()) || peek().is_digit(10) {
        advance();
    }
    make_token(identifier_type())
}

unsafe fn identifier_type() -> TokenType {
    use TokenType::*;
    match *scanner.start as char {
        'a' => check_keyword(1, 2, "nd", AND),
        'c' => check_keyword(1, 4, "lass", CLASS),
        'e' => check_keyword(1, 3, "lse", ELSE),
        'f' if scanner.current.offset_from(scanner.start) > 1 => {
            match *scanner.start.add(1) as char {
                'a' => check_keyword(2, 3, "lse", FALSE),
                'o' => check_keyword(2, 1, "r", FOR),
                'n' => check_keyword(2, 1, "n", FUN),
                _ => IDENTIFIER,
            }
        }
        'i' => check_keyword(1, 1, "f", IF),
        'n' => check_keyword(1, 2, "il", NIL),
        'o' => check_keyword(1, 1, "r", OR),
        'p' => check_keyword(1, 4, "rint", PRINT),
        'r' => check_keyword(1, 5, "eturn", RETURN),
        's' => check_keyword(1, 4, "uper", SUPER),
        't' if scanner.current.offset_from(scanner.start) > 1 => {
            match *scanner.start.add(1) as char {
                'h' => check_keyword(2, 2, "is", THIS),
                'r' => check_keyword(2, 2, "ue", TRUE),
                _ => IDENTIFIER,
            }
        }
        'v' => check_keyword(1, 2, "ar", VAR),
        'w' => check_keyword(1, 4, "hile", WHILE),
        _ => TokenType::IDENTIFIER,
    }
}

unsafe fn check_keyword(start: isize, len: isize, rest: &str, ttype: TokenType) -> TokenType {
    if scanner.current.offset_from(scanner.start) == start + len
        && slice::from_raw_parts(scanner.start.offset(start), len as usize) == rest.as_bytes()
    {
        return ttype;
    }
    TokenType::IDENTIFIER
}

unsafe fn number() -> Token {
    while peek().is_digit(10) {
        advance();
    }
    if peek() == '.' && peek_next().map(|c| c.is_digit(10)).unwrap_or(false) {
        advance();
        while peek().is_digit(10) {
            advance();
        }
    }
    make_token(TokenType::NUMBER)
}

unsafe fn string() -> Token {
    while peek() != '"' && !is_at_end() {
        if peek() == '\n' {
            scanner.line += 1;
        }
        advance();
    }
    if is_at_end() {
        return error_token("Unterminated string.");
    };

    advance();
    make_token(TokenType::STRING)
}

unsafe fn skip_whitespace() {
    loop {
        let c = peek();
        match c {
            ' ' | '\r' | '\t' => {
                let _ = advance();
            }
            '\n' => {
                scanner.line += 1;
                let _ = advance();
            }
            '/' => {
                if peek_next() == Some('/') {
                    while peek() != '\n' && !is_at_end() {
                        advance();
                    }
                } else {
                    return;
                }
            }
            _ => break,
        }
    }
}

unsafe fn peek_next() -> Option<char> {
    if is_at_end() {
        None
    } else {
        Some(*scanner.current.add(1) as _)
    }
}

unsafe fn peek() -> char {
    *scanner.current as _
}

unsafe fn tmatch(expected: char) -> bool {
    if is_at_end() {
        return false;
    }
    if *scanner.current as char != expected {
        return false;
    }
    scanner.current = scanner.current.add(1);
    true
}

unsafe fn advance() -> char {
    scanner.current = scanner.current.add(1);
    *scanner.current.sub(1) as char
}

unsafe fn error_token(message: &str) -> Token {
    Token {
        ttype: TokenType::ERROR,
        start: message as *const str as _,
        length: message.len(),
        line: scanner.line,
    }
}

unsafe fn make_token(ttype: TokenType) -> Token {
    Token {
        ttype,
        start: scanner.start,
        length: scanner.current.offset_from(scanner.start) as usize,
        line: scanner.line,
    }
}

unsafe fn is_at_end() -> bool {
    scanner.current >= scanner.end
}

#[allow(non_camel_case_types)]
#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenType {
    // Single-character tokens.
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,

    // One or two character tokens.
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,

    // Literals.
    IDENTIFIER,
    STRING,
    NUMBER,

    // Keywords.
    AND,
    CLASS,
    ELSE,
    FALSE,
    FUN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,

    EOF,

    // Error
    ERROR,
}
