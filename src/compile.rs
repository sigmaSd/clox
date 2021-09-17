use std::{ops::Index, ptr};

use crate::{
    chunk::{Chunk, OpCode},
    debug::disassemble_chunk,
    scanner::{self, Token, TokenType},
    utils::Helper,
    value::{copy_string, object::ObjFunction, Obj, Value},
    NUMBER_VAL, OBJ_VAL,
};

pub unsafe fn compile(source: &str) -> Result<*mut ObjFunction, ()> {
    scanner::init_scanner(source);
    let mut compiler = Compiler::new();
    init_compiler(&mut compiler, FunctionType::Script);

    advance();

    while !tmatch(TokenType::EOF) {
        declaration();
    }

    consume(TokenType::EOF, "Expect end of expression.");
    let function = end_compiler();

    if !parser.had_error {
        Ok(function)
    } else {
        Err(())
    }
}

unsafe fn declaration() {
    if tmatch(TokenType::FUN) {
        fun_declaration();
    } else if tmatch(TokenType::VAR) {
        var_declaration();
    } else {
        statement();
    }

    if parser.panic_mode {
        synchronize();
    }
}

unsafe fn fun_declaration() {
    let global = parse_variable("Expect function name.");
    mark_initialized();
    function(FunctionType::Function);
    define_variable(global);
}

unsafe fn function(ftype: FunctionType) {
    let mut compiler = Compiler::new();
    init_compiler(&mut compiler, ftype);
    begin_scope();

    consume(TokenType::LEFT_PAREN, "Expect '(' after function name.");
    if !check(TokenType::RIGHT_PAREN) {
        do_while(
            &mut || {
                (*(*current).function).arity += 1;
                if (*(*current).function).arity > 255 {
                    error_at_current("Can't have more than 255 parameters.");
                }
                let constant = parse_variable("Expect parameter name.");
                define_variable(constant);
            },
            &|| tmatch(TokenType::COMMA),
        );
    }
    consume(TokenType::RIGHT_PAREN, "Expect ')' after parameters.");
    consume(TokenType::LEFT_BRACE, "Expect '{' before function body.");
    block();

    let function = end_compiler();
    emit_bytes(OpCode::Closure.into(), make_constant(OBJ_VAL!(function)));

    for i in 0..(*function).upvalue_count {
        emit_byte(if compiler.upvalues[i].is_local { 1 } else { 0 });
        emit_byte(compiler.upvalues[i].index);
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
    if (*current).scope_depth > 0 {
        mark_initialized();
        return;
    }

    emit_bytes(OpCode::DefineGlobal.into(), global);
}

unsafe fn and_(_can_assign: bool) {
    let end_jump = emit_jump(OpCode::JumpIfFalse.into());

    emit_byte(OpCode::Pop.into());
    parse_presendence(Presendence::AND);

    patch_jump(end_jump);
}

unsafe fn or_(_can_assign: bool) {
    let else_jump = emit_jump(OpCode::JumpIfFalse.into());
    let end_jump = emit_jump(OpCode::Jump.into());

    patch_jump(else_jump);
    emit_byte(OpCode::Pop.into());

    parse_presendence(Presendence::OR);
    patch_jump(end_jump);
}

unsafe fn mark_initialized() {
    if (*current).scope_depth == 0 {
        return;
    }
    (*current).locals[(*current).local_count - 1].depth = Some((*current).scope_depth);
}

unsafe fn parse_variable(error_message: &str) -> u8 {
    consume(TokenType::IDENTIFIER, error_message);

    declare_variable();
    if (*current).scope_depth > 0 {
        return 0;
    }

    identifier_constant(&parser.previous)
}

unsafe fn declare_variable() {
    if (*current).scope_depth == 0 {
        return;
    }

    let name = parser.previous;

    for i in (0..(*current).local_count).rev() {
        let local = &(*current).locals[i];
        if local.depth.is_some() && local.depth.unwrap() < (*current).scope_depth {
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
    if (*current).local_count == U8_COUNT {
        error("Too many local variables in function.");
        return;
    }

    let local = &mut (*current).locals[(*current).local_count];
    (*current).local_count += 1;
    local.name = name;
    local.depth = None;
    local.is_captured = false;
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
    } else if tmatch(TokenType::FOR) {
        for_statement();
    } else if tmatch(TokenType::IF) {
        if_statement();
    } else if tmatch(TokenType::RETURN) {
        return_statement();
    } else if tmatch(TokenType::WHILE) {
        while_statement();
    } else if tmatch(TokenType::LEFT_BRACE) {
        begin_scope();
        block();
        end_scope();
    } else {
        expression_statement();
    }
}

unsafe fn return_statement() {
    if (*current).ftype == FunctionType::Script {
        error("Can't return from top-level code.");
    }

    if tmatch(TokenType::SEMICOLON) {
        emit_return();
    } else {
        expression();
        consume(TokenType::SEMICOLON, "Expect ';' after return value.");
        emit_byte(OpCode::Return.into());
    }
}

unsafe fn for_statement() {
    begin_scope();
    consume(TokenType::LEFT_PAREN, "Expect '(' after 'for'.");
    if tmatch(TokenType::SEMICOLON) {
        // no initializer
    } else if tmatch(TokenType::VAR) {
        var_declaration();
    } else {
        expression_statement();
    }

    let mut loop_start = (*current_chunk()).count;
    let mut exit_jump = None;
    if !tmatch(TokenType::SEMICOLON) {
        expression();
        consume(TokenType::SEMICOLON, "Expect ';' after loop condition.");
        exit_jump = Some(emit_jump(OpCode::JumpIfFalse.into()));
        emit_byte(OpCode::Pop.into());
    }

    if !tmatch(TokenType::RIGHT_PAREN) {
        let body_jump = emit_jump(OpCode::Jump.into());
        let increment_start = (*current_chunk()).count;
        expression();
        emit_byte(OpCode::Pop.into());
        consume(TokenType::RIGHT_PAREN, "Expect ')' after for clauses.");

        emit_loop(loop_start);
        loop_start = increment_start;
        patch_jump(body_jump);
    }

    statement();
    emit_loop(loop_start);

    if let Some(exit_jump) = exit_jump {
        patch_jump(exit_jump);
        emit_byte(OpCode::Pop.into());
    }

    end_scope();
}

unsafe fn if_statement() {
    consume(TokenType::LEFT_PAREN, "Expect '(' after 'if'.");
    expression();
    consume(TokenType::RIGHT_PAREN, "Expect ')' after condition.");

    let then_jump = emit_jump(OpCode::JumpIfFalse.into());
    emit_byte(OpCode::Pop.into());
    statement();

    let else_jump = emit_jump(OpCode::Jump.into());
    patch_jump(then_jump);
    emit_byte(OpCode::Pop.into());

    if tmatch(TokenType::ELSE) {
        statement();
    }
    patch_jump(else_jump);
}

unsafe fn while_statement() {
    let loop_start = (*current_chunk()).count;
    consume(TokenType::LEFT_PAREN, "Expect '(' after 'while'.");
    expression();
    consume(TokenType::RIGHT_PAREN, "Expect ')' after condition.");

    let exit_jump = emit_jump(OpCode::JumpIfFalse.into());
    emit_byte(OpCode::Pop.into());
    statement();
    emit_loop(loop_start);

    patch_jump(exit_jump);
    emit_byte(OpCode::Pop.into());
}

unsafe fn emit_loop(loop_start: usize) {
    emit_byte(OpCode::Loop.into());

    let offset = (*current_chunk()).count - loop_start + 2;
    if offset > u16::MAX as _ {
        error("Loop body too large.");
    }

    emit_byte(((offset >> 8) & 0xff) as _);
    emit_byte((offset & 0xff) as _);
}

unsafe fn patch_jump(offset: usize) {
    let jump = (*current_chunk()).count - offset - 2;

    if jump > u16::MAX as usize {
        error("Too much code to jump over.");
    }

    *(*current_chunk()).code.add(offset) = ((jump >> 8) & 0xff) as u8;
    *(*current_chunk()).code.add(offset + 1) = (jump & 0xff) as u8;
}

unsafe fn emit_jump(instruction: u8) -> usize {
    emit_byte(instruction);
    emit_byte(0xff);
    emit_byte(0xff);
    (*current_chunk()).count - 2
}
unsafe fn end_scope() {
    (*current).scope_depth -= 1;

    while (*current).local_count > 0
        && (*current).locals[(*current).local_count - 1]
            .depth
            .map(|depth| depth > (*current).scope_depth)
            .unwrap_or(false)
    {
        if (*current).locals[(*current).local_count - 1].is_captured {
            emit_byte(OpCode::CloseUpValue.into());
        } else {
            emit_byte(OpCode::Pop.into());
        }
        (*current).local_count -= 1;
    }
}

unsafe fn begin_scope() {
    (*current).scope_depth += 1;
}

unsafe fn block() {
    while !check(TokenType::RIGHT_BRACE) && !check(TokenType::EOF) {
        declaration();
    }
    consume(TokenType::RIGHT_BRACE, "Expect '}' after block.");
}

unsafe fn expression_statement() {
    expression();
    consume(TokenType::SEMICOLON, "Expect ';' after expression.");
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

unsafe fn end_compiler() -> *mut ObjFunction {
    emit_return();
    let function = (*current).function;

    if cfg!(feature = "DEBUG_PRINT_CODE") && !parser.had_error {
        disassemble_chunk(
            &*current_chunk(),
            if !(*function).name.is_null() {
                (*(*function).name).as_str()
            } else {
                "<script>"
            },
        );
    }

    current = (*current).enclosing;

    function
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
unsafe fn call(_can_assign: bool) {
    let arg_count = argument_list();
    emit_bytes(OpCode::Call.into(), arg_count);
}

unsafe fn argument_list() -> u8 {
    let mut arg_count = 0;
    if !check(TokenType::RIGHT_PAREN) {
        do_while(
            &mut || {
                expression();
                if arg_count == 255 {
                    error("Can't have more than 255 arguments.");
                }
                arg_count += 1;
            },
            &|| tmatch(TokenType::COMMA),
        );
    }
    consume(TokenType::RIGHT_PAREN, "Expect ')' after arguments.");
    arg_count
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

pub const U8_COUNT: usize = u8::MAX as usize + 1;

#[derive(Debug)]
struct Compiler {
    enclosing: *mut Compiler,
    function: *mut ObjFunction,
    ftype: FunctionType,
    locals: [Local; U8_COUNT],
    upvalues: [UpValue; U8_COUNT],
    local_count: usize,
    scope_depth: usize,
}

#[derive(Clone, Copy, Debug)]
struct UpValue {
    index: u8,
    is_local: bool,
}

impl UpValue {
    const fn new() -> Self {
        Self {
            index: 0,
            is_local: false,
        }
    }
}

impl Compiler {
    fn new() -> Self {
        Self {
            enclosing: ptr::null_mut(),
            function: ptr::null_mut(),
            ftype: FunctionType::Script,
            upvalues: [UpValue::new(); U8_COUNT],
            locals: [Local::new(); U8_COUNT],
            local_count: 0,
            scope_depth: 0,
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FunctionType {
    Function,
    Script,
}

#[derive(Debug, Clone, Copy)]
struct Local {
    name: Token,
    depth: Option<usize>,
    is_captured: bool,
}

impl Local {
    const fn new() -> Self {
        Self {
            name: Token::new_uninit(),
            depth: Some(0),
            is_captured: false,
        }
    }
}

#[allow(non_upper_case_globals)]
static mut current: *mut Compiler = ptr::null_mut();
//Compiler {
//    enclosing: ptr::null_mut(),
//    function: ptr::null_mut(),
//    ftype: FunctionType::Script,
//    locals: [Local {
//        name: Token::new_uninit(),
//        depth: Some(0),
//    }; U8_COUNT],
//    local_count: 0,
//    scope_depth: 0,
//};

unsafe fn init_compiler(compiler: *mut Compiler, ftype: FunctionType) {
    (*compiler).enclosing = current;
    // For GC
    (*compiler).function = ptr::null_mut();
    (*compiler).ftype = ftype;

    (*compiler).local_count = 0;
    (*compiler).scope_depth = 0;
    (*compiler).function = ObjFunction::new();

    current = compiler;

    if ftype != FunctionType::Script {
        (*(*current).function).name =
            copy_string(parser.previous.start, parser.previous.length) as *mut _;
    }

    let local = &mut (*current).locals[(*current).local_count];
    (*current).local_count += 1;
    local.depth = Some(0);
    local.is_captured = false;
    local.name.start = "" as *const str as _;
    local.name.length = 0
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
    let s = copy_string(parser.previous.start.add(1), parser.previous.length - 2);
    let string = OBJ_VAL!(s);
    emit_constant(string);
}

unsafe fn variable(can_assign: bool) {
    named_variable(parser.previous, can_assign);
}

unsafe fn named_variable(name: Token, can_assign: bool) {
    let mut arg = resolve_local(&(*current), &name);

    let get_op;
    let set_op;
    if arg.is_some() {
        get_op = OpCode::GetLocal;
        set_op = OpCode::SetLocal;
    } else if resolve_upvalue(&mut (*current), &name).is_some() {
        arg = resolve_upvalue(&mut (*current), &name);
        get_op = OpCode::GetUpValue;
        set_op = OpCode::SetUpValue;
    } else {
        arg = Some(identifier_constant(&name) as _);
        get_op = OpCode::GetGlobal;
        set_op = OpCode::SetGlobal;
    }
    let arg = arg.unwrap().try_into().unwrap();

    if can_assign && tmatch(TokenType::EQUAL) {
        expression();
        emit_bytes(set_op.into(), arg);
    } else {
        emit_bytes(get_op.into(), arg);
    }
}

fn resolve_upvalue(compiler: &mut Compiler, name: &Token) -> Option<usize> {
    if compiler.enclosing.is_null() {
        return None;
    }

    if let Some(local) = unsafe { resolve_local(&*compiler.enclosing, name) } {
        unsafe {
            (*compiler.enclosing).locals[local].is_captured = true;
        }
        return Some(add_upvalue(compiler, local as u8, true));
    }

    if let Some(upvalue) = unsafe { resolve_upvalue(&mut *compiler.enclosing, name) } {
        return Some(add_upvalue(compiler, upvalue as u8, false));
    }

    None
}

fn add_upvalue(compiler: &mut Compiler, index: u8, is_local: bool) -> usize {
    unsafe {
        let upvalue_count = (*compiler.function).upvalue_count;

        for i in 0..upvalue_count {
            let upvalue = &compiler.upvalues[i];
            if upvalue.index == index && upvalue.is_local == is_local {
                return i;
            }
        }

        if upvalue_count == U8_COUNT {
            error("Too many closure variables in function.");
            return 0;
        }

        compiler.upvalues[upvalue_count].is_local = is_local;
        compiler.upvalues[upvalue_count].index = index;

        let v = (*compiler.function).upvalue_count;
        (*compiler.function).upvalue_count += 1;
        v
    }
}

unsafe fn resolve_local(compiler: &Compiler, name: &Token) -> Option<usize> {
    for i in (0..compiler.local_count).rev() {
        let local = &compiler.locals[i];
        if identifiers_equal(name, &local.name) {
            if local.depth.is_none() {
                error("Can't read local variable in its own initializer.");
            }
            return Some(i);
        }
    }
    None
}

unsafe fn emit_constant(value: Value) {
    emit_bytes(OpCode::Constant.into(), make_constant(value));
}

unsafe fn make_constant(value: Value) -> u8 {
    let constant = (*current_chunk()).add_constant(value);
    if constant > u8::MAX as usize {
        error("Too many constants in one chunk.");
        return 0;
    }
    constant as u8
}

unsafe fn error(message: &str) {
    error_at(&parser.previous, message)
}

unsafe fn emit_return() {
    emit_byte(OpCode::Nil.into());
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
    &mut (*(*current).function).chunk
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
            infix: Some(call),
            presendence: Presendence::CALL,
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
            infix: Some(and_),
            presendence: Presendence::AND,
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
            infix: Some(or_),
            presendence: Presendence::OR,
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

fn do_while(f: &mut dyn FnMut(), cond: &dyn Fn() -> bool) {
    f();
    while cond() {
        f();
    }
}
