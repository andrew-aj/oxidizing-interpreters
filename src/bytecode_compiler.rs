use std::{borrow::BorrowMut, mem::transmute, ops::Add};

use crate::{
    bytecode,
    chunk::Chunk,
    scanner,
    scanner::TokenType,
    utils::{Ref, RefCreate},
    value::{self, Value},
};

#[derive(PartialEq, Eq)]
pub enum FunctionType {
    Function,
    Initializer,
    Method,
    Script,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
#[repr(i8)]
pub enum Precedence {
    None = 0,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

impl Precedence {
    fn succ(&self) -> Precedence {
        match self {
            Precedence::Primary => Precedence::None,
            _ => unsafe { transmute(self.clone() as i8 + 1) },
        }
    }
}

#[macro_export]
macro_rules! emit {
    ($self:ident, $opcode:expr) => {
        $self.emit_code($opcode, $self.previous().line)
    };
}

type ParseFn<'b> = fn(&mut Compiler<'b>, bool) -> Result<(), scanner::Error>;

pub struct ParseRule<'b> {
    prefix: Option<ParseFn<'b>>,
    infix: Option<ParseFn<'b>>,
    precedence: Precedence,
}

#[derive(Copy, Clone)]
struct Local<'a> {
    name: scanner::Token<'a>,
    depth: i64,
    is_captured: bool,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Upvalue {
    Upvalue(usize),
    Local(usize),
}

pub struct Compiler<'a> {
    tokens: Vec<scanner::Token<'a>>,
    token_index: usize,
    current_class: Option<ClassCompiler>,
    scopes: Vec<Scope<'a>>,
    scope_index: usize,
}

impl<'a> Default for Compiler<'a> {
    fn default() -> Self {
        Compiler {
            tokens: Vec::new(),
            token_index: 0,
            current_class: None,
            scopes: vec![Scope::init_scope("", FunctionType::Script)],
            scope_index: 0,
        }
    }
}

struct Scope<'a> {
    scope_depth: i64,
    locals: Vec<Local<'a>>,
    upvalues: Vec<Upvalue>,
    function: value::Function,
    function_type: FunctionType,
}

impl<'a> Scope<'a> {
    fn init_scope(func_name: &str, func_type: FunctionType) -> Scope {
        Scope {
            scope_depth: 0,
            locals: vec![Local {
                name: if func_type == FunctionType::Function {
                    scanner::Token {
                        ty: TokenType::Identifier,
                        text: "this",
                        line: 0,
                        col: 0,
                    }
                } else {
                    scanner::Token {
                        ty: TokenType::Identifier,
                        text: "",
                        line: 0,
                        col: 0,
                    }
                },
                depth: 0,
                is_captured: false,
            }],
            upvalues: Default::default(),
            function: value::Function {
                arity: 0,
                chunk: Default::default(),
                name: Ref::create(func_name.to_string()),
            },
            function_type: func_type,
        }
    }
}

#[derive(Clone)]
struct ClassCompiler {
    has_superclass: bool,
}

impl<'a> Compiler<'a> {
    pub fn compile(source: &'a String) -> Result<value::Function, scanner::Error> {
        let mut compiler: Compiler<'a> = Default::default();

        match scanner::scan_tokens(&source) {
            Ok(tokens) => {
                compiler.tokens = tokens;

                while !compiler.match_(TokenType::EOF) {
                    compiler.declaration()?;
                }

                compiler.emit_return();

                compiler.current_chunk().disassemble_chunk("");

                Ok(std::mem::take(&mut compiler.current_scope().function))
            }
            Err(err) => Err(err),
        }
    }

    fn enclosing_scope(&mut self) -> &mut Scope<'a> {
        self.scopes
            .get_mut(self.scope_index - 1)
            .expect("Error in enclosing scope implementation")
    }

    fn current_scope(&mut self) -> &mut Scope<'a> {
        self.scopes
            .get_mut(self.scope_index)
            .expect("Error in current scope implementation")
    }

    fn current_scope_imm(&self) -> &Scope<'a> {
        self.scopes
            .get(self.scope_index)
            .expect("Error in current scope implementation")
    }

    fn current_chunk(&mut self) -> &mut Chunk {
        &mut self.current_scope().function.borrow_mut().chunk
    }

    fn scope_depth(&mut self) -> i64 {
        self.current_scope().scope_depth
    }

    fn consume(&mut self, token: scanner::TokenType, error: &str) -> Result<(), scanner::Error> {
        if self.check(token) {
            self.advance();
            return Ok(());
        }

        Err(scanner::Error {
            message: format!(
                "Expect '{:?}' but found '{:?}': {}",
                token,
                self.peek(),
                error
            ),
            line: self.peek().line,
            col: self.peek().col,
        })
    }

    fn previous(&self) -> &scanner::Token<'a> {
        assert!(self.token_index != 0);
        &self.tokens[self.token_index - 1]
    }

    fn advance(&mut self) {
        match self.peek().ty {
            scanner::TokenType::EOF => (),
            _ => self.token_index += 1,
        };
    }

    fn match_(&mut self, token: scanner::TokenType) -> bool {
        match !self.check(token) {
            true => false,
            false => {
                self.advance();
                true
            }
        }
    }

    fn check(&self, token: scanner::TokenType) -> bool {
        self.peek().ty == token
    }

    fn peek(&self) -> &scanner::Token {
        &self.tokens[self.token_index]
    }

    fn emit_return(&mut self) {
        if self.current_scope().function_type == FunctionType::Initializer {
            self.emit_code(bytecode::OpCode::GetLocal(0), self.peek().line);
        } else {
            self.emit_code(bytecode::OpCode::Nil, self.peek().line);
        }

        self.emit_code(bytecode::OpCode::Return, self.peek().line);
    }

    fn emit_code(&mut self, op: bytecode::OpCode, line: usize) {
        self.current_chunk().write_chunk((op, line));
    }

    fn emit_jump(&mut self, op: bytecode::OpCode) -> usize {
        self.emit_code(op, self.previous().line);
        self.current_chunk().code.len() - 1
    }

    fn emit_loop(&mut self, loop_start: usize) {
        let offset = self.current_chunk().code.len() - loop_start + 2;
        self.emit_code(bytecode::OpCode::Loop(offset), self.previous().line);
    }

    fn patch_jump(&mut self, offset: usize) -> Result<(), scanner::Error> {
        let jump = self.current_chunk().code.len() - offset - 1;

        self.current_chunk().code[offset] = match self.current_chunk().code[offset] {
            (bytecode::OpCode::JumpIfFalse(_), i) => (bytecode::OpCode::JumpIfFalse(jump), i),
            (bytecode::OpCode::Jump(_), i) => (bytecode::OpCode::Jump(jump), i),
            (_, _) => {
                return Err(scanner::Error {
                    message: "Could not find jump!".to_string(),
                    line: self.previous().line,
                    col: self.previous().col,
                })
            }
        };

        Ok(())
    }

    fn begin_scope(&mut self) {
        self.current_scope().scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.current_scope().scope_depth -= 1;

        let scope_depth = self.current_scope().scope_depth;

        let line = self.previous().line;

        let old_locals: Vec<Local<'_>> = self
            .current_scope_imm()
            .locals
            .clone()
            .into_iter()
            .rev()
            .take_while(|local| local.depth > scope_depth)
            .collect();

        let mut size = 0;

        for local in old_locals {
            if local.is_captured {
                self.emit_code(bytecode::OpCode::CloseUpvalue, line);
            } else {
                self.emit_code(bytecode::OpCode::Pop, line);
            }

            size += 1;
        }

        for _ in 0..size {
            self.current_scope().locals.pop();
        }
    }

    fn identifier_constant(&mut self, name: value::Value) -> usize {
        self.current_chunk().add_constant(name)
    }

    fn add_local(&mut self, name: scanner::Token<'a>) {
        self.current_scope().locals.push(Local {
            name,
            depth: -1,
            is_captured: false,
        })
    }

    fn declare_variable(&mut self) -> Result<(), scanner::Error> {
        if self.current_scope().scope_depth == 0 {
            return Ok(());
        }

        let name = self.previous().clone();

        let scope_depth = self.current_scope().scope_depth;

        let scope_error =
            self.current_scope().locals.iter().rev().any(|var| {
                var.depth != -1 && var.depth < scope_depth && var.name.text == name.text
            });

        if scope_error {
            return Err(scanner::Error {
                message: format!("Variable {} redeclared in the same scope", name.text),
                line: name.line,
                col: name.col,
            });
        }

        self.add_local(name);
        Ok(())
    }

    fn parse_variable(&mut self, error_message: &str) -> Result<usize, scanner::Error> {
        self.consume(TokenType::Identifier, error_message)?;

        self.declare_variable()?;

        if self.scope_depth() > 0 {
            return Ok(0);
        }

        Ok(self.identifier_constant(Value::str_to_value(self.previous().text)))
    }

    fn mark_initialized(&mut self) {
        if self.current_scope().scope_depth == 0 {
            return;
        }

        self.current_scope()
            .locals
            .last_mut()
            .expect("Local variables list is empty")
            .depth = self.current_scope().scope_depth;
    }

    fn define_variable(&mut self, global: usize) {
        self.emit_code(bytecode::OpCode::DefineGlobal(global), self.previous().line);
    }

    fn argument_list(&mut self) -> Result<u8, scanner::Error> {
        let mut arg_count = 0;
        if !self.check(TokenType::RightParen) {
            loop {
                self.expression()?;
                if arg_count == 255 {
                    return Err(scanner::Error {
                        message: "Can't have more than 255 parameters".to_string(),
                        line: self.previous().line,
                        col: self.previous().col,
                    });
                }
                arg_count += 1;
                if !self.match_(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after arguments.")?;
        Ok(arg_count)
    }

    fn get_rule(token: TokenType) -> ParseRule<'a> {
        match token {
            TokenType::LeftParen => ParseRule {
                prefix: Some(Compiler::grouping),
                infix: Some(Compiler::call),
                precedence: Precedence::Call,
            },
            TokenType::RightParen => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::LeftBrace => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::RightBrace => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Comma => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Dot => ParseRule {
                prefix: None,
                infix: Some(Compiler::dot),
                precedence: Precedence::Call,
            },
            TokenType::Minus => ParseRule {
                prefix: Some(Compiler::unary),
                infix: Some(Compiler::binary),
                precedence: Precedence::Term,
            },
            TokenType::Plus => ParseRule {
                prefix: None,
                infix: Some(Compiler::binary),
                precedence: Precedence::Term,
            },
            TokenType::SemiColon => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Slash => ParseRule {
                prefix: None,
                infix: Some(Compiler::binary),
                precedence: Precedence::Factor,
            },
            TokenType::Star => ParseRule {
                prefix: None,
                infix: Some(Compiler::binary),
                precedence: Precedence::Factor,
            },
            TokenType::Bang => ParseRule {
                prefix: Some(Compiler::unary),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::BangEqual => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Equal => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::EqualEqual => ParseRule {
                prefix: None,
                infix: Some(Compiler::binary),
                precedence: Precedence::Equality,
            },
            TokenType::Greater => ParseRule {
                prefix: None,
                infix: Some(Compiler::binary),
                precedence: Precedence::Comparison,
            },
            TokenType::GreaterEqual => ParseRule {
                prefix: None,
                infix: Some(Compiler::binary),
                precedence: Precedence::Comparison,
            },
            TokenType::Less => ParseRule {
                prefix: None,
                infix: Some(Compiler::binary),
                precedence: Precedence::Comparison,
            },
            TokenType::LessEqual => ParseRule {
                prefix: None,
                infix: Some(Compiler::binary),
                precedence: Precedence::Comparison,
            },
            TokenType::Identifier => ParseRule {
                prefix: Some(Compiler::variable),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::String => ParseRule {
                prefix: Some(Compiler::string),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Number => ParseRule {
                prefix: Some(Compiler::number),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::And => ParseRule {
                prefix: None,
                infix: Some(Compiler::and),
                precedence: Precedence::And,
            },
            TokenType::Class => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Else => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::False => ParseRule {
                prefix: Some(Compiler::literal),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::For => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Fun => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::If => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Nil => ParseRule {
                prefix: Some(Compiler::literal),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Or => ParseRule {
                prefix: None,
                infix: Some(Compiler::or),
                precedence: Precedence::Or,
            },
            TokenType::Print => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Return => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Super => ParseRule {
                prefix: Some(Compiler::super_),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::This => ParseRule {
                prefix: Some(Compiler::this),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::True => ParseRule {
                prefix: Some(Compiler::literal),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Var => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::While => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::EOF => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<(), scanner::Error> {
        self.advance();
        let can_assign = precedence <= Precedence::Assignment;
        match Compiler::get_rule(self.previous().ty).prefix {
            Some(rule) => rule(self, can_assign),
            None => {
                return Err(scanner::Error {
                    message: "Expected expression.".to_string(),
                    line: self.previous().line,
                    col: self.previous().col,
                })
            }
        }?;

        while precedence <= Compiler::get_rule(self.peek().ty).precedence {
            self.advance();
            match Compiler::get_rule(self.previous().ty).infix {
                Some(infix_rule) => infix_rule(self, can_assign),
                None => {
                    return Err(scanner::Error {
                        message: format!("Cannot find infix rule for token {:?}", self.previous()),
                        line: self.previous().line,
                        col: self.previous().col,
                    })
                }
            }?;
        }

        Ok(())
    }

    fn declaration(&mut self) -> Result<(), scanner::Error> {
        if self.match_(TokenType::Class) {
            self.class_declaration()
        } else if self.match_(TokenType::Fun) {
            self.fun_declaration()
        } else if self.match_(TokenType::Var) {
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    fn class_declaration(&mut self) -> Result<(), scanner::Error> {
        self.consume(TokenType::Identifier, "Expected class name.")?;
        let class_name = self.previous().clone();

        let name_constant = self.identifier_constant(Value::str_to_value(class_name.text));
        self.declare_variable()?;

        self.emit_code(bytecode::OpCode::Class(name_constant), self.previous().line);
        self.define_variable(name_constant);

        let class_compiler = self.current_class.clone();
        self.current_class = Some(ClassCompiler {
            has_superclass: false,
        });

        if self.match_(TokenType::Less) {
            self.consume(TokenType::Identifier, "Expected superclass name.")?;
            self.variable(false)?;

            if class_name.text == self.previous().text {
                return Err(scanner::Error {
                    message: "Class cannot inherit from itself.".to_string(),
                    line: self.previous().line,
                    col: self.previous().col,
                });
            }

            self.named_variable(class_name.clone(), false)?;
            self.emit_code(bytecode::OpCode::Inherit, self.previous().line);

            if let Some(class) = &mut self.current_class {
                class.has_superclass = true;
            } else {
                return Err(scanner::Error {
                    message: "Implementation error, no outside class".to_string(),
                    line: self.previous().line,
                    col: self.previous().col,
                });
            }

            self.begin_scope();
            self.add_local(Compiler::synthetic_token("super"));
            self.define_variable(0);
        }

        self.named_variable(class_name.clone(), false)?;
        self.consume(TokenType::LeftBrace, "Expect '{' before class body.")?;

        while !self.check(TokenType::RightBrace) && !self.check(TokenType::EOF) {
            self.method()?;
        }

        self.consume(TokenType::RightBrace, "Expect '}' after class body.")?;
        self.emit_code(bytecode::OpCode::Pop, self.previous().line);

        if let Some(class) = &self.current_class {
            if class.has_superclass {
                self.end_scope();
            }
        }

        self.current_class = class_compiler;

        Ok(())
    }

    fn block(&mut self) -> Result<(), scanner::Error> {
        while !self.check(TokenType::RightBrace) && !self.check(TokenType::EOF) {
            self.declaration()?;
        }

        self.consume(TokenType::RightBrace, "Expect '}' after block.")?;

        Ok(())
    }

    fn function(&mut self, func_type: FunctionType) -> Result<(), scanner::Error> {
        let scope = Scope::init_scope(self.previous().text, func_type);
        self.scopes.push(scope);
        self.begin_scope();

        self.consume(TokenType::LeftParen, "Expect '(' after function name.")?;
        if !self.check(TokenType::RightParen) {
            loop {
                if self.current_scope().function.arity == 255 {
                    return Err(scanner::Error {
                        message: "Can't have more than 255 parameters".to_string(),
                        line: self.previous().line,
                        col: self.previous().col,
                    });
                }
                self.current_scope().function.arity += 1;

                let constant = self.parse_variable("Expect parameter name.")?;
                self.define_variable(constant);

                if !self.match_(TokenType::Comma) {
                    break;
                }
            }
        }

        self.consume(TokenType::RightParen, "Expect ')' after parameters.")?;
        self.consume(TokenType::LeftBrace, "Expected '{' before function body.")?;
        self.block()?;

        self.emit_return();
        let function = std::mem::take(&mut self.current_scope().function);
        let upvals = std::mem::take(&mut self.current_scope().upvalues);
        self.scopes.pop();

        let closure = value::Closure {
            function: Ref::create(function),
            upvalues: Vec::new(),
        };
        let constant = self
            .current_chunk()
            .add_constant(Value::Closure(Ref::create(closure)));

        self.emit_code(
            bytecode::OpCode::Closure(constant, upvals),
            self.previous().line,
        );

        Ok(())
    }

    fn method(&mut self) -> Result<(), scanner::Error> {
        self.consume(TokenType::Identifier, "Expect method name.")?;

        let constant = self.identifier_constant(Value::str_to_value(self.previous().text));

        let func_type = match self.previous().text {
            "init" => FunctionType::Initializer,
            _ => FunctionType::Method,
        };

        self.function(func_type)?;
        self.emit_code(bytecode::OpCode::Method(constant), self.previous().line);

        Ok(())
    }

    fn fun_declaration(&mut self) -> Result<(), scanner::Error> {
        let global = self.parse_variable("Expect function name.")?;
        self.mark_initialized();
        self.function(FunctionType::Function)?;
        self.define_variable(global);

        Ok(())
    }

    fn var_declaration(&mut self) -> Result<(), scanner::Error> {
        let global = self.parse_variable("Expect variable name.")?;

        if self.match_(TokenType::Equal) {
            self.expression()?;
        } else {
            self.emit_code(bytecode::OpCode::Nil, self.previous().line);
        }

        self.consume(
            TokenType::SemiColon,
            "Expect ';' after variable declaration.",
        )?;
        self.define_variable(global);

        Ok(())
    }

    fn statement(&mut self) -> Result<(), scanner::Error> {
        if self.match_(TokenType::Print) {
            self.print_statement()?;
        } else if self.match_(TokenType::For) {
            self.for_statement()?;
        } else if self.match_(TokenType::If) {
            self.if_statement()?;
        } else if self.match_(TokenType::Return) {
            self.return_statement()?;
        } else if self.match_(TokenType::While) {
            self.while_statement()?;
        } else if self.match_(TokenType::LeftBrace) {
            self.begin_scope();
            self.block()?;
            self.end_scope();
        } else {
            self.expression_statement()?;
        }

        Ok(())
    }

    fn expression_statement(&mut self) -> Result<(), scanner::Error> {
        self.expression()?;
        self.consume(TokenType::SemiColon, "Expect ';' after expression.")?;
        self.emit_code(bytecode::OpCode::Pop, self.previous().line);

        Ok(())
    }

    fn return_statement(&mut self) -> Result<(), scanner::Error> {
        if self.current_scope().function_type == FunctionType::Script {
            return Err(scanner::Error {
                message: "Can't return from top-level code.".to_string(),
                line: self.previous().line,
                col: self.previous().col,
            });
        }

        if self.match_(TokenType::SemiColon) {
            self.emit_return();
        } else {
            if self.current_scope().function_type == FunctionType::Initializer {
                return Err(scanner::Error {
                    message: "Can't return from a value from an initializer.".to_string(),
                    line: self.previous().line,
                    col: self.previous().col,
                });
            }

            self.expression()?;
            self.consume(TokenType::SemiColon, "Expect ';' after return value.")?;
            self.emit_code(bytecode::OpCode::Return, self.previous().line);
        }

        Ok(())
    }

    fn while_statement(&mut self) -> Result<(), scanner::Error> {
        let loop_start = self.current_chunk().code.len();
        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.")?;
        self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after condition.")?;

        let exit_jump = self.emit_jump(bytecode::OpCode::JumpIfFalse(0));
        self.emit_code(bytecode::OpCode::Pop, self.previous().line);
        self.statement()?;
        self.emit_loop(loop_start);

        self.patch_jump(exit_jump)?;
        self.emit_code(bytecode::OpCode::Pop, self.previous().line);

        Ok(())
    }

    fn if_statement(&mut self) -> Result<(), scanner::Error> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.")?;
        self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after condition.")?;

        let then_jump = self.emit_jump(bytecode::OpCode::JumpIfFalse(0));
        self.emit_code(bytecode::OpCode::Pop, self.previous().line);
        self.statement()?;

        let else_jump = self.emit_jump(bytecode::OpCode::Jump(0));

        self.patch_jump(then_jump)?;
        self.emit_code(bytecode::OpCode::Pop, self.previous().line);

        if self.match_(TokenType::Else) {
            self.statement()?;
        }

        self.patch_jump(else_jump)?;

        Ok(())
    }

    fn for_statement(&mut self) -> Result<(), scanner::Error> {
        self.begin_scope();
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.")?;

        if self.match_(TokenType::SemiColon) {
        } else if self.match_(TokenType::Var) {
            self.var_declaration()?;
        } else {
            self.expression_statement()?;
        }

        let mut loop_start = self.current_chunk().code.len();
        let mut exit_jump = None;
        if !self.match_(TokenType::SemiColon) {
            self.expression()?;
            self.consume(TokenType::SemiColon, "Expect ';' after loop condition.")?;

            exit_jump = Some(self.emit_jump(bytecode::OpCode::JumpIfFalse(0)));
            self.emit_code(bytecode::OpCode::Pop, self.previous().line);
        }

        if !self.match_(TokenType::RightParen) {
            let body_jump = self.emit_jump(bytecode::OpCode::Jump(0));
            let increment_start = self.current_chunk().code.len() + 1;

            self.expression()?;
            self.emit_code(bytecode::OpCode::Pop, self.previous().line);
            self.consume(TokenType::RightParen, "Expect ')' after for clauses.")?;

            self.emit_loop(loop_start);
            loop_start = increment_start;
            self.patch_jump(body_jump)?;
        }

        self.statement()?;
        self.emit_loop(loop_start);

        if let Some(i) = exit_jump {
            self.patch_jump(i)?;
            self.emit_code(bytecode::OpCode::Pop, self.previous().line);
        }

        self.end_scope();

        Ok(())
    }

    fn print_statement(&mut self) -> Result<(), scanner::Error> {
        self.expression()?;
        self.consume(TokenType::SemiColon, "Expect ';' after value.")?;
        self.emit_code(bytecode::OpCode::Print, self.previous().line);

        Ok(())
    }

    fn expression(&mut self) -> Result<(), scanner::Error> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn grouping(self: &mut Compiler<'a>, _can_assign: bool) -> Result<(), scanner::Error> {
        self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after expression.")?;
        Ok(())
    }

    fn call(&mut self, _can_assign: bool) -> Result<(), scanner::Error> {
        let arg_count = self.argument_list()?;
        self.emit_code(bytecode::OpCode::Call(arg_count), self.previous().line);
        Ok(())
    }

    fn dot(&mut self, can_assign: bool) -> Result<(), scanner::Error> {
        self.consume(TokenType::Identifier, "Expect property name after '.'.")?;
        let name = self.identifier_constant(Value::str_to_value(self.previous().text));

        if can_assign && self.match_(TokenType::Equal) {
            self.expression()?;
            self.emit_code(bytecode::OpCode::SetProperty(name), self.previous().line);
        } else if self.match_(TokenType::LeftParen) {
            let arg_count = self.argument_list()?;
            self.emit_code(
                bytecode::OpCode::Invoke(name, arg_count),
                self.previous().line,
            );
        } else {
            self.emit_code(bytecode::OpCode::GetProperty(name), self.previous().line);
        }
        Ok(())
    }

    fn unary(&mut self, _can_assign: bool) -> Result<(), scanner::Error> {
        let operator_type = self.previous().ty;

        self.parse_precedence(Precedence::Unary)?;

        match operator_type {
            TokenType::Bang => self.emit_code(bytecode::OpCode::Not, self.previous().line),
            TokenType::Minus => self.emit_code(bytecode::OpCode::Negate, self.previous().line),
            _ => {
                return Err(scanner::Error {
                    message: format!("Invalid unary operator token {:?}", operator_type),
                    line: self.previous().line,
                    col: self.previous().col,
                })
            }
        }
        Ok(())
    }

    fn binary(&mut self, _can_assign: bool) -> Result<(), scanner::Error> {
        let operator_type = self.previous().ty;
        let rule = Compiler::get_rule(operator_type);

        self.parse_precedence(rule.precedence.succ())?;

        match operator_type {
            TokenType::BangEqual => {
                emit!(self, bytecode::OpCode::Equal);
                emit!(self, bytecode::OpCode::Not)
            }
            TokenType::EqualEqual => emit!(self, bytecode::OpCode::Equal),
            TokenType::Greater => emit!(self, bytecode::OpCode::Greater),
            TokenType::GreaterEqual => {
                emit!(self, bytecode::OpCode::Less);
                emit!(self, bytecode::OpCode::Not);
            }
            TokenType::Less => emit!(self, bytecode::OpCode::Less),
            TokenType::LessEqual => {
                emit!(self, bytecode::OpCode::Greater);
                emit!(self, bytecode::OpCode::Not);
            }
            TokenType::Plus => emit!(self, bytecode::OpCode::Add),
            TokenType::Minus => emit!(self, bytecode::OpCode::Subtract),
            TokenType::Star => emit!(self, bytecode::OpCode::Multiply),
            TokenType::Slash => emit!(self, bytecode::OpCode::Divide),
            _ => {
                return Err(scanner::Error {
                    message: format!("Invalid binaryoperator token {:?}", operator_type),
                    line: self.previous().line,
                    col: self.previous().col,
                })
            }
        }

        Ok(())
    }

    fn add_upvalue(&mut self, val: Upvalue) -> usize {
        self.current_scope()
            .upvalues
            .iter()
            .position(|upval| *upval == val)
            .unwrap_or({
                self.current_scope().upvalues.push(val);
                self.current_scope().upvalues.len() - 1
            })
    }

    fn resolve_upvalue(&mut self, name: &scanner::Token) -> Result<Option<usize>, scanner::Error> {
        if self.scope_index < 1 {
            return Ok(None);
        }

        let local = self.resolve_local(name)?;
        if let Some(local) = local {
            self.enclosing_scope().locals[local].is_captured = true;
            return Ok(Some(self.add_upvalue(Upvalue::Local(local))));
        }

        self.scope_index -= 1;
        let mut upval_index = None;

        if let Some(index) = self.resolve_upvalue(name)? {
            upval_index = Some(self.add_upvalue(Upvalue::Upvalue(index)));
        }

        self.scope_index += 1;
        return Ok(upval_index);
    }

    fn resolve_local(&mut self, name: &scanner::Token) -> Result<Option<usize>, scanner::Error> {
        let index = self
            .current_scope()
            .locals
            .iter()
            .rposition(|local| local.name.text == name.text);

        match index {
            Some(i) => {
                let local = self.current_scope().locals[i];
                if local.depth == -1 {
                    Err(scanner::Error {
                        message: format!(
                            "Can't read local variable {} in its own initializer.",
                            name.text
                        ),
                        line: name.line,
                        col: name.col,
                    })
                } else {
                    Ok(Some(i))
                }
            }
            None => Ok(None),
        }
    }

    fn named_variable(
        &mut self,
        name: scanner::Token,
        can_assign: bool,
    ) -> Result<(), scanner::Error> {
        let get_op: bytecode::OpCode;
        let set_op: bytecode::OpCode;

        let arg = self.resolve_local(&name)?;
        match arg {
            Some(i) => {
                get_op = bytecode::OpCode::GetLocal(i);
                set_op = bytecode::OpCode::SetLocal(i);
            }
            None => {
                if let Some(i) = self.resolve_upvalue(&name)? {
                    get_op = bytecode::OpCode::GetUpvalue(i);
                    set_op = bytecode::OpCode::SetUpvalue(i);
                } else {
                    let i = self.identifier_constant(Value::str_to_value(name.text));
                    get_op = bytecode::OpCode::GetUpvalue(i);
                    set_op = bytecode::OpCode::SetUpvalue(i);
                }
            }
        };

        if can_assign && self.match_(TokenType::Equal) {
            self.expression()?;
            self.emit_code(set_op, name.line);
        } else {
            self.emit_code(get_op, name.line);
        }

        Ok(())
    }

    fn variable(&mut self, can_assign: bool) -> Result<(), scanner::Error> {
        self.named_variable(self.previous().clone(), can_assign)
    }

    fn string(&mut self, _can_assign: bool) -> Result<(), scanner::Error> {
        let token = self.previous().clone();
        match token.ty {
            TokenType::String => {
                let text = &token.text[1..token.text.len() - 1];
                let val = Value::str_to_value(text);
                let index = self.current_chunk().add_constant(val);
                self.emit_code(bytecode::OpCode::Constant(index), token.line);
                Ok(())
            }
            _ => Err(scanner::Error {
                message: "Cannot parse token that is not a string literal.".to_string(),
                line: self.previous().line,
                col: self.previous().col,
            }),
        }
    }

    fn number(&mut self, _can_assign: bool) -> Result<(), scanner::Error> {
        match self.previous().ty {
            TokenType::Number => {
                let parsed: f64 = match self.previous().text.parse::<f64>() {
                    Ok(i) => i,
                    Err(_) => {
                        return Err(scanner::Error {
                            message: "Cannot parse number.".to_string(),
                            line: self.previous().line,
                            col: self.previous().col,
                        })
                    }
                };

                let constant_index = self
                    .current_chunk()
                    .add_constant(Value::num_to_value(parsed));
                self.emit_code(
                    bytecode::OpCode::Constant(constant_index),
                    self.previous().line,
                );

                Ok(())
            }
            _ => Err(scanner::Error {
                message: "Cannot parse token that is not a number.".to_string(),
                line: self.previous().line,
                col: self.previous().col,
            }),
        }
    }

    fn and(&mut self, _can_assign: bool) -> Result<(), scanner::Error> {
        let end_jump = self.emit_jump(bytecode::OpCode::JumpIfFalse(0));
        emit!(self, bytecode::OpCode::Pop);
        self.parse_precedence(Precedence::And)?;
        self.patch_jump(end_jump)?;
        Ok(())
    }

    fn or(&mut self, _can_assign: bool) -> Result<(), scanner::Error> {
        let else_jump = self.emit_jump(bytecode::OpCode::JumpIfFalse(0));
        let end_jump = self.emit_jump(bytecode::OpCode::JumpIfFalse(0));

        self.patch_jump(else_jump)?;
        emit!(self, bytecode::OpCode::Pop);

        self.parse_precedence(Precedence::Or)?;
        self.patch_jump(end_jump)?;
        Ok(())
    }

    fn literal(&mut self, _can_assign: bool) -> Result<(), scanner::Error> {
        match self.previous().ty {
            TokenType::False => emit!(self, bytecode::OpCode::False),
            TokenType::Nil => emit!(self, bytecode::OpCode::Nil),
            TokenType::True => emit!(self, bytecode::OpCode::True),
            _ => {
                return Err(scanner::Error {
                    message: "Invalid literal.".to_string(),
                    line: self.previous().line,
                    col: self.previous().col,
                })
            }
        }
        Ok(())
    }

    fn synthetic_token(text: &str) -> scanner::Token {
        scanner::Token {
            ty: TokenType::Identifier,
            text,
            line: 0,
            col: 0,
        }
    }

    fn super_(&mut self, can_assign: bool) -> Result<(), scanner::Error> {
        Ok(())
    }

    fn this(&mut self, _can_assign: bool) -> Result<(), scanner::Error> {
        if let Some(_) = self.current_class {
            self.variable(false)?;
        } else {
            return Err(scanner::Error {
                message: "Can't use 'this' outside of a class.".to_string(),
                line: self.previous().line,
                col: self.previous().col,
            });
        }
        Ok(())
    }
}
