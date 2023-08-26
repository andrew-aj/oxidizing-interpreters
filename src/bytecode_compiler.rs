use crate::scanner::TokenType;

pub enum FunctionType {
    Function,
    Initializer,
    Method,
    Script,
}

pub enum Precedence {
    None,
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

type ParseFn = fn(&mut Compiler, bool);

pub struct ParseRule {
    prefix: Option<ParseFn>,
    infix: Option<ParseFn>,
    precedence: Precedence,
}

pub struct Compiler {}

impl Compiler {
    fn get_rule(token: TokenType) -> ParseRule {
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

    fn grouping(&mut self, can_assign: bool) {}

    fn call(&mut self, can_assign: bool) {}

    fn dot(&mut self, can_assign: bool) {}

    fn unary(&mut self, can_assign: bool) {}

    fn binary(&mut self, can_assign: bool) {}

    fn variable(&mut self, can_assign: bool) {}

    fn string(&mut self, can_assign: bool) {}

    fn number(&mut self, can_assign: bool) {}

    fn and(&mut self, can_assign: bool) {}

    fn or(&mut self, can_assign: bool) {}

    fn literal(&mut self, can_assign: bool) {}

    fn super_(&mut self, can_assign: bool) {}

    fn this(&mut self, can_assign: bool) {}
}
