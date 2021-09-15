use std::{ops::Index, ptr};

use crate::{
    chunk::{Chunk, OpCode},
    debug::disassemble_chunk,
    scanner::{self, Token, TokenType},
    utils::Helper,
    value::{copy_string, Obj, Value},
    NUMBER_VAL, OBJ_VAL,
};

pub unsafe fn compile(source: &str, chunk: &mut Chunk) -> Result<(), ()> {
    scanner::init_scanner(source);
    init_compiler(&mut current);
    COMPILING_CHUNK = chunk;

    advance();

    while !tmatch(TokenType::EOF) {
        declaration();
    }

    consume(TokenType::EOF, "Expect end of expression.");
    end_compiler();

    if !parser.had_error {
        Ok(())
    } else {
        Err(())
    }
}

unsafe fn declaration() {
    if tmatch(TokenType::VAR) {
        var_declaration();
    } else {
        statement();
    }

    if parser.panic_mode {
        synchronize();
    }
}

unsafe fn var_declaration() {
    let global: u8 = parse_variable("Expect variable name.");

    if tmatch(TokenType::EQUAL) {
        expression();
    } else {
        emit_byte(OpCode::Nil.into());
    }
    consume(
        TokenType::SEMICOLON,
        "Expect ';' after variable declaration.",
    );
    define_variable(global);
}

unsafe fn define_variable(global: u8) {
    if current.scope_depth > 0 {
        mark_initialized();
        return;
    }

    emit_bytes(OpCode::DefineGlobal.into(), global);
}

unsafe fn mark_initialized() {
    current.locals[current.local_count - 1].depth = Some(current.scope_depth);
}

unsafe fn parse_variable(error_message: &str) -> u8 {
    consume(TokenType::IDENTIFIER, error_message);

    declare_variable();
    if current.scope_depth > 0 {
        return 0;
    }

    identifier_constant(&parser.previous)
}

unsafe fn declare_variable() {
    if current.scope_depth == 0 {
        return;
    }

    let name = parser.previous;

    for i in (0..current.local_count).rev() {
        let local = &current.locals[i];
        if local.depth.is_some() && local.depth.unwrap() < current.scope_depth {
            break;
        }

        if identifiers_equal(&name, &local.name) {
            error("Already a variable with this name in this scope.");
        }
    }

    add_local(name);
}

unsafe fn identifiers_equal(name1: &Token, name2: &Token) -> bool {
    if name1.length != name2.length {
        return false;
    }
    name1.start.to_str(name1.length) == name2.start.to_str(name2.length)
}

unsafe fn add_local(name: Token) {
    if current.local_count == U8_COUNT {
        error("Too many local variables in function.");
        return;
    }

    let local = &mut current.locals[current.local_count];
    current.local_count += 1;
    local.name = name;
    local.depth = None;
}

unsafe fn identifier_constant(name: &Token) -> u8 {
    make_constant(OBJ_VAL!(copy_string(name.start, name.length)))
}

unsafe fn synchronize() {
    parser.panic_mode = false;
    while parser.current.ttype != TokenType::EOF {
        if parser.previous.ttype == TokenType::SEMICOLON {
            return;
        }
        match parser.current.ttype {
            TokenType::CLASS
            | TokenType::FUN
            | TokenType::VAR
            | TokenType::FOR
            | TokenType::IF
            | TokenType::WHILE
            | TokenType::PRINT
            | TokenType::RETURN => return,
            _ => (),
        }
        advance();
    }
}

unsafe fn statement() {
    if tmatch(TokenType::PRINT) {
        print_statement();
    } else if tmatch(TokenType::LEFT_BRACE) {
        begin_scope();
        block();
        end_scope();
    } else {
        expression_statement();
    }
}

unsafe fn end_scope() {
    current.scope_depth -= 1;

    while current.local_count > 0
        && current.locals[current.local_count - 1]
            .depth
            .map(|depth| depth > current.scope_depth)
            .unwrap_or(false)
    {
        emit_byte(OpCode::Pop.into());
        current.local_count -= 1;
    }
}

unsafe fn begin_scope() {
    current.scope_depth += 1;
}

unsafe fn block() {
    while !check(TokenType::RIGHT_BRACE) && !check(TokenType::EOF) {
        declaration();
    }
    consume(TokenType::RIGHT_BRACE, "Expect '}' after block.");
}

unsafe fn expression_statement() {
    expression();
    consume(TokenType::SEMICOLON, "Expect ; after value.");
    emit_byte(OpCode::Pop.into());
}

unsafe fn print_statement() {
    expression();
    consume(TokenType::SEMICOLON, "Expect ; after value.");
    emit_byte(OpCode::Print.into());
}

fn tmatch(ttype: TokenType) -> bool {
    unsafe {
        if !check(ttype) {
            return false;
        }
        advance();
        true
    }
}

fn check(ttype: TokenType) -> bool {
    unsafe { parser.current.ttype == ttype }
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

unsafe fn binary(_can_assign: bool) {
    let operator_type = parser.previous.ttype;
    let rule = get_rule(operator_type);
    parse_presendence((rule.presendence as u8 + 1).try_into().unwrap());

    use TokenType::*;
    match operator_type {
        BANG_EQUAL => emit_bytes(OpCode::Equal.into(), OpCode::Not.into()),
        EQUAL_EQUAL => emit_byte(OpCode::Equal.into()),
        GREATER => emit_byte(OpCode::Greater.into()),
        GREATER_EQUAL => emit_bytes(OpCode::Less.into(), OpCode::Not.into()),
        LESS => emit_byte(OpCode::Less.into()),
        LESS_EQUAL => emit_bytes(OpCode::Greater.into(), OpCode::Not.into()),
        PLUS => emit_byte(OpCode::Add.into()),
        MINUS => emit_byte(OpCode::Substract.into()),
        STAR => emit_byte(OpCode::Multiply.into()),
        SLASH => emit_byte(OpCode::Divide.into()),
        _ => unreachable!(),
    }
}
unsafe fn literal(_can_assign: bool) {
    match parser.previous.ttype {
        TokenType::FALSE => emit_byte(OpCode::False.into()),
        TokenType::NIL => emit_byte(OpCode::Nil.into()),
        TokenType::TRUE => emit_byte(OpCode::True.into()),
        _ => unreachable!(),
    }
}

fn get_rule(ttype: TokenType) -> &'static ParseRule {
    &RULES[ttype]
}
unsafe fn unary(_can_assign: bool) {
    let operator_type = parser.previous.ttype;

    parse_presendence(Presendence::UNARY);

    use TokenType::*;
    match operator_type {
        BANG => emit_byte(OpCode::Not.into()),
        MINUS => emit_byte(OpCode::Negate.into()),
        _ => (),
    }
}

unsafe fn grouping(_can_assign: bool) {
    expression();
    consume(TokenType::RIGHT_PAREN, "Expect ')' after expression.");
}

unsafe fn parse_presendence(presendence: Presendence) {
    advance();
    let prefix_rule = if let Some(prefix_rule) = get_rule(parser.previous.ttype).prefix {
        prefix_rule
    } else {
        error("Expect expression.");
        return;
    };

    let can_assign = presendence <= Presendence::ASSIGNMENT;
    prefix_rule(can_assign);

    while presendence <= get_rule(parser.current.ttype).presendence {
        advance();
        let infix_rule = get_rule(parser.previous.ttype).infix.unwrap();
        infix_rule(can_assign);
    }

    if can_assign && tmatch(TokenType::EQUAL) {
        error("Invalid assignment target.");
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
type ParseFn = unsafe fn(bool);

const U8_COUNT: usize = u8::MAX as usize + 1;

#[derive(Debug)]
struct Compiler {
    locals: [Local; U8_COUNT],
    local_count: usize,
    scope_depth: usize,
}

#[derive(Debug, Clone, Copy)]
struct Local {
    name: Token,
    depth: Option<usize>,
}

#[allow(non_upper_case_globals)]
static mut current: Compiler = Compiler {
    locals: [Local {
        name: Token::new_uninit(),
        depth: Some(0),
    }; U8_COUNT],
    local_count: 0,
    scope_depth: 0,
};

unsafe fn init_compiler(compiler: &mut Compiler) {
    compiler.local_count = 0;
    compiler.scope_depth = 0;
}

unsafe fn consume(ttype: TokenType, message: &str) {
    if parser.current.ttype == ttype {
        advance();
        return;
    }
    error_at_current(message)
}

unsafe fn number(_can_assign: bool) {
    let value: f64 = parser
        .previous
        .start
        .to_str(parser.previous.length)
        .parse()
        .unwrap();
    emit_constant(NUMBER_VAL!(value))
}

unsafe fn string(_can_assign: bool) {
    let string = OBJ_VAL!(copy_string(
        parser.previous.start.add(1),
        parser.previous.length - 2,
    ));
    emit_constant(string);
}

unsafe fn variable(can_assign: bool) {
    named_variable(parser.previous, can_assign);
}

unsafe fn named_variable(name: Token, can_assign: bool) {
    let mut arg = resolve_local(&current, &name);

    let get_op;
    let set_op;
    if arg.is_some() {
        get_op = OpCode::GetLocal;
        set_op = OpCode::SetLocal;
    } else {
        arg = Some(identifier_constant(&name));
        get_op = OpCode::GetGlobal;
        set_op = OpCode::SetGlobal;
    }
    let arg = arg.unwrap();

    if can_assign && tmatch(TokenType::EQUAL) {
        expression();
        emit_bytes(set_op.into(), arg);
    } else {
        emit_bytes(get_op.into(), arg);
    }
}

unsafe fn resolve_local(compiler: &Compiler, name: &Token) -> Option<u8> {
    for i in (0..compiler.local_count).rev() {
        let local = &compiler.locals[i];
        if identifiers_equal(name, &local.name) {
            if local.depth.is_none() {
                error("Can't read local variable in its own initializer.");
            }
            // locals fit in u8
            return Some(i.try_into().unwrap());
        }
    }
    None
}

unsafe fn emit_constant(value: Value) {
    emit_bytes(OpCode::Constant.into(), make_constant(value));
}

unsafe fn make_constant(value: Value) -> u8 {
    (*current_chunk()).add_constant(value)
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
            prefix: Some(unary),
            infix: None,
            presendence: Presendence::NONE,
        },
    ),
    (
        BANG_EQUAL,
        ParseRule {
            prefix: None,
            infix: Some(binary),
            presendence: Presendence::EQUALITY,
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
            infix: Some(binary),
            presendence: Presendence::EQUALITY,
        },
    ),
    (
        GREATER,
        ParseRule {
            prefix: None,
            infix: Some(binary),
            presendence: Presendence::COMPARISON,
        },
    ),
    (
        GREATER_EQUAL,
        ParseRule {
            prefix: None,
            infix: Some(binary),
            presendence: Presendence::COMPARISON,
        },
    ),
    (
        LESS,
        ParseRule {
            prefix: None,
            infix: Some(binary),
            presendence: Presendence::COMPARISON,
        },
    ),
    (
        LESS_EQUAL,
        ParseRule {
            prefix: None,
            infix: Some(binary),
            presendence: Presendence::COMPARISON,
        },
    ),
    (
        IDENTIFIER,
        ParseRule {
            prefix: Some(variable),
            infix: None,
            presendence: Presendence::NONE,
        },
    ),
    (
        STRING,
        ParseRule {
            prefix: Some(string),
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
            prefix: Some(literal),
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
            prefix: Some(literal),
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
            prefix: Some(literal),
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
