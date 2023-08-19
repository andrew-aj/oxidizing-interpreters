use core::fmt;
use std::iter::Peekable;
use std::{collections::HashMap, str::CharIndices};

#[derive(Eq, PartialEq, Debug, Clone, Copy)]
pub enum TokenType {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    SemiColon,
    Slash,
    Star,

    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreatEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    String,
    Number,

    // Keywords
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Error,
    EOF,
}

#[derive(Debug)]
pub struct Token<'a> {
    pub ty: TokenType,
    pub text: &'a str,
    pub line: u64,
    pub col: u64,
}

struct Scanner<'a> {
    source: Peekable<CharIndices<'a>>,
    original: &'a str,
    tokens: Vec<Token<'a>>,
    start: usize,
    current: usize,
    line: u64,
    col: u64,
    keywords: HashMap<&'static str, TokenType>,
}

pub struct Error {
    pub what: String,
    pub line: u64,
    pub col: u64,
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "[Line {} Column {}] Error: {}",
            self.line, self.col, self.what
        )
    }
}

pub fn scan_tokens<'a>(source: &'a String) -> Result<Vec<Token<'a>>, Error> {
    let mut scanner = Scanner::new(&source);

    let result = scanner.scan_tokens();

    match result {
        Some(err) => Err(err),
        None => Ok(scanner.tokens),
    }
}

impl Scanner<'_> {
    fn new<'a>(input: &'a String) -> Scanner<'a> {
        Scanner {
            source: input.char_indices().peekable(),
            original: &input,
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
            col: 0,
            keywords: vec![
                ("and", TokenType::And),
                ("class", TokenType::Class),
                ("else", TokenType::Else),
                ("false", TokenType::False),
                ("for", TokenType::For),
                ("fun", TokenType::Fun),
                ("if", TokenType::If),
                ("nil", TokenType::Nil),
                ("or", TokenType::Or),
                ("print", TokenType::Print),
                ("return", TokenType::Return),
                ("super", TokenType::Super),
                ("this", TokenType::This),
                ("true", TokenType::True),
                ("var", TokenType::Var),
                ("while", TokenType::While),
            ]
            .into_iter()
            .collect(),
        }
    }

    fn is_at_end(&mut self) -> bool {
        self.source.peek().is_none()
    }

    fn advance(&mut self) -> char {
        self.col += 1;

        let (_, character) = self.source.next().unwrap();

        self.current = match self.source.peek() {
            Some((x, _)) => *x,
            None => self.original.len(),
        };

        character
    }

    fn peek(&mut self) -> char {
        match self.source.peek() {
            Some((_, c)) => *c,
            None => '\0',
        }
    }

    fn match_val(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }

        if self.source.peek().unwrap().1 != expected {
            return false;
        }

        self.advance();
        return true;
    }

    fn skip_whitespace(&mut self, c: char) {
        loop {
            match c {
                ' ' | '\r' | '\t' => {}
                '\n' => {
                    self.line += 1;
                }
                '/' => {
                    if self.peek() == '/' {
                        while self.peek() != '\n' && !self.is_at_end() {
                            self.advance();
                        }
                    } else {
                        self.make_token(TokenType::Slash);
                    }
                },
                _ => return,
            }
        }
    }

    fn make_token(&mut self, ty: TokenType) {
        let text = &self.original[self.start..self.current];

        self.tokens.push(Token {
            ty,
            text,
            line: self.line,
            col: self.col,
        })
    }

    fn is_alpha(c: char) -> bool {
        c.is_alphabetic()
    }

    fn is_digit(c: char) -> bool {
        c.is_numeric()
    }

    fn scan_tokens(&mut self) -> Option<Error> {
        let c = self.advance();

        unimplemented!()
    }
}
