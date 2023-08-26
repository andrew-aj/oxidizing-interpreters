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
    GreaterEqual,
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

    EOF,
}

#[derive(Debug, PartialEq)]
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
    err: Option<Error>,
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

pub fn scan_tokens(source: &String) -> Result<Vec<Token>, Error> {
    let mut scanner = Scanner::new(source);

    scanner.scan_tokens();

    match scanner.err {
        Some(err) => Err(err),
        None => Ok(scanner.tokens),
    }
}

impl Scanner<'_> {
    fn new(input: &String) -> Scanner {
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
            err: None,
        }
    }

    fn scan_tokens(&mut self) {
        while !self.is_done() {
            self.scan_token();
        }

        match self.err {
            Some(_) => {}
            None => self.tokens.push(Token {
                ty: TokenType::EOF,
                text: "",
                line: self.line,
                col: self.col,
            }),
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

    fn peek_next(&mut self) -> char {
        if let Some((index, _)) = self.source.peek() {
            let next = self.original[*index..].chars().next().unwrap().len_utf8();
            if let Some(next_char) = self.original[index + next..].chars().next() {
                next_char
            } else {
                '\0'
            }
        } else {
            '\0'
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

    fn skip_whitespace(&mut self) {
        loop {
            match self.peek() {
                ' ' | '\r' | '\t' => {
                    self.advance();
                }
                '\n' => {
                    self.line += 1;
                    self.col = 0;
                    self.advance();
                }
                '/' => {
                    if self.peek_next() == '/' {
                        while self.peek() != '\n' && !self.is_at_end() {
                            self.advance();
                        }
                    } else {
                        break;
                    }
                }
                _ => {
                    break;
                }
            }
        }
    }

    fn make_token(&mut self, ty: TokenType) {
        let text = &self.original[self.start..self.current];

        self.tokens.push(Token {
            ty,
            text,
            line: self.line,
            col: self.col - text.len() as u64 + 1,
        })
    }

    fn is_alpha(c: char) -> bool {
        c.is_alphabetic()
    }

    fn is_digit(c: char) -> bool {
        c.is_numeric()
    }

    fn is_done(&mut self) -> bool {
        self.err.is_some() || self.is_at_end()
    }

    fn scan_token(&mut self) {
        self.skip_whitespace();

        if self.is_done() {
            return;
        }

        self.start = self.current;
        let c = self.advance();

        match c {
            '(' => self.make_token(TokenType::LeftParen),
            ')' => self.make_token(TokenType::RightParen),
            '{' => self.make_token(TokenType::LeftBrace),
            '}' => self.make_token(TokenType::RightBrace),
            ';' => self.make_token(TokenType::SemiColon),
            ',' => self.make_token(TokenType::Comma),
            '.' => self.make_token(TokenType::Dot),
            '-' => self.make_token(TokenType::Minus),
            '+' => self.make_token(TokenType::Plus),
            '/' => self.make_token(TokenType::Slash),
            '*' => self.make_token(TokenType::Star),
            '!' => {
                if self.match_val('=') {
                    self.make_token(TokenType::BangEqual);
                } else {
                    self.make_token(TokenType::Bang);
                }
            }
            '=' => {
                if self.match_val('=') {
                    self.make_token(TokenType::EqualEqual);
                } else {
                    self.make_token(TokenType::Equal);
                }
            }
            '<' => {
                if self.match_val('=') {
                    self.make_token(TokenType::LessEqual);
                } else {
                    self.make_token(TokenType::Less);
                }
            }
            '>' => {
                if self.match_val('=') {
                    self.make_token(TokenType::GreaterEqual);
                } else {
                    self.make_token(TokenType::Greater);
                }
            }
            '"' => self.string(),
            _ => {
                if Scanner::is_alpha(c) {
                    self.identifier();
                } else if Scanner::is_digit(c) {
                    self.number();
                } else {
                    self.err = Some(Error {
                        what: format!("Unexpected character {}", c),
                        line: self.line,
                        col: self.col,
                    });
                }
            }
        };
    }

    fn string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }

            self.advance();
        }

        if self.is_at_end() {
            self.err = Some(Error {
                what: "Unterminated string.".to_string(),
                line: self.line,
                col: self.col,
            });

            return;
        }

        self.advance();

        self.make_token(TokenType::String);
    }

    fn identifier(&mut self) {
        while Scanner::is_alpha(self.peek()) || Scanner::is_digit(self.peek()) {
            self.advance();
        }

        let ident_text = &self.original[self.start..self.current];

        let token_type: TokenType = *self
            .keywords
            .get(&ident_text)
            .unwrap_or(&TokenType::Identifier);

        self.make_token(token_type);
    }

    fn number(&mut self) {
        while Scanner::is_digit(self.peek()) {
            self.advance();
        }

        if self.peek() == '.' && Scanner::is_digit(self.peek_next()) {
            self.advance();

            while Scanner::is_digit(self.peek()) {
                self.advance();
            }
        }

        self.make_token(TokenType::Number);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty() {
        let text = String::from("");
        let result = scan_tokens(&text);
        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            vec![Token {
                ty: TokenType::EOF,
                col: 0,
                line: 1,
                text: ""
            }]
        );
    }

    #[test]
    fn test_string_with_whitespace() {
        let text = String::from("\"hello\" \n");
        let result = scan_tokens(&text);
        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            vec![
                Token {
                    ty: TokenType::String,
                    col: 1,
                    line: 1,
                    text: "\"hello\"",
                },
                Token {
                    ty: TokenType::EOF,
                    col: 1,
                    line: 2,
                    text: ""
                }
            ]
        );
    }

    #[test]
    fn test_string() {
        let text = String::from("\"hello\"");
        let result = scan_tokens(&text);
        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            vec![
                Token {
                    ty: TokenType::String,
                    col: 1,
                    line: 1,
                    text: "\"hello\"",
                },
                Token {
                    ty: TokenType::EOF,
                    col: 7,
                    line: 1,
                    text: ""
                }
            ]
        );
    }

    #[test]
    fn test_num() {
        let text = String::from("42.abc");
        let result = scan_tokens(&text);
        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            vec![
                Token {
                    ty: TokenType::Number,
                    col: 1,
                    line: 1,
                    text: "42",
                },
                Token {
                    ty: TokenType::Dot,
                    col: 3,
                    line: 1,
                    text: ".",
                },
                Token {
                    ty: TokenType::Identifier,
                    col: 4,
                    line: 1,
                    text: "abc",
                },
                Token {
                    ty: TokenType::EOF,
                    col: 6,
                    line: 1,
                    text: ""
                }
            ]
        );
    }

    #[test]
    fn test_keywords() {
        let text =
            String::from("and class else for fun if nil or print return super this true var while");
        let result = scan_tokens(&text);
        assert!(result.is_ok());
        let list = vec![
            Token {
                ty: TokenType::And,
                col: 1,
                line: 1,
                text: "and",
            },
            Token {
                ty: TokenType::Class,
                col: 5,
                line: 1,
                text: "class",
            },
            Token {
                ty: TokenType::Else,
                col: 11,
                line: 1,
                text: "else",
            },
            Token {
                ty: TokenType::For,
                col: 16,
                line: 1,
                text: "for",
            },
            Token {
                ty: TokenType::Fun,
                col: 20,
                line: 1,
                text: "fun",
            },
            Token {
                ty: TokenType::If,
                col: 24,
                line: 1,
                text: "if",
            },
            Token {
                ty: TokenType::Nil,
                col: 27,
                line: 1,
                text: "nil",
            },
            Token {
                ty: TokenType::Or,
                col: 31,
                line: 1,
                text: "or",
            },
            Token {
                ty: TokenType::Print,
                col: 34,
                line: 1,
                text: "print",
            },
            Token {
                ty: TokenType::Return,
                col: 40,
                line: 1,
                text: "return",
            },
            Token {
                ty: TokenType::Super,
                col: 47,
                line: 1,
                text: "super",
            },
            Token {
                ty: TokenType::This,
                col: 53,
                line: 1,
                text: "this",
            },
            Token {
                ty: TokenType::True,
                col: 58,
                line: 1,
                text: "true",
            },
            Token {
                ty: TokenType::Var,
                col: 63,
                line: 1,
                text: "var",
            },
            Token {
                ty: TokenType::While,
                col: 67,
                line: 1,
                text: "while",
            },
            Token {
                ty: TokenType::EOF,
                col: 71,
                line: 1,
                text: "",
            },
        ];
        assert_eq!(result.unwrap(), list);
    }

    #[test]
    fn test_literals() {
        let text = String::from("\"hello world!\" 23.23 23 abcd123");
        let result = scan_tokens(&text);
        assert!(result.is_ok());

        let list = vec![
            Token {
                ty: TokenType::String,
                col: 1,
                line: 1,
                text: "\"hello world!\"",
            },
            Token {
                ty: TokenType::Number,
                col: 16,
                line: 1,
                text: "23.23",
            },
            Token {
                ty: TokenType::Number,
                col: 22,
                line: 1,
                text: "23",
            },
            Token {
                ty: TokenType::Identifier,
                col: 25,
                line: 1,
                text: "abcd123",
            },
            Token {
                ty: TokenType::EOF,
                col: 31,
                line: 1,
                text: "",
            },
        ];
        assert_eq!(result.unwrap(), list);
    }

    #[test]
    fn test_single_character_tokens() {
        let text = String::from("(){},.-+;/*");
        let result = scan_tokens(&text);
        assert!(result.is_ok());

        let list = vec![
            Token {
                ty: TokenType::LeftParen,
                col: 1,
                line: 1,
                text: "(",
            },
            Token {
                ty: TokenType::RightParen,
                col: 2,
                line: 1,
                text: ")",
            },
            Token {
                ty: TokenType::LeftBrace,
                col: 3,
                line: 1,
                text: "{",
            },
            Token {
                ty: TokenType::RightBrace,
                col: 4,
                line: 1,
                text: "}",
            },
            Token {
                ty: TokenType::Comma,
                col: 5,
                line: 1,
                text: ",",
            },
            Token {
                ty: TokenType::Dot,
                col: 6,
                line: 1,
                text: ".",
            },
            Token {
                ty: TokenType::Minus,
                col: 7,
                line: 1,
                text: "-",
            },
            Token {
                ty: TokenType::Plus,
                col: 8,
                line: 1,
                text: "+",
            },
            Token {
                ty: TokenType::SemiColon,
                col: 9,
                line: 1,
                text: ";",
            },
            Token {
                ty: TokenType::Slash,
                col: 10,
                line: 1,
                text: "/",
            },
            Token {
                ty: TokenType::Star,
                col: 11,
                line: 1,
                text: "*",
            },
            Token {
                ty: TokenType::EOF,
                col: 11,
                line: 1,
                text: "",
            },
        ];
        assert_eq!(result.unwrap(), list);
    }

    #[test]
    fn test_double_character_tokens() {
        let text = String::from("!!====>>=<<=");
        let result = scan_tokens(&text);
        assert!(result.is_ok());

        let list = vec![
            Token {
                ty: TokenType::Bang,
                col: 1,
                line: 1,
                text: "!",
            },
            Token {
                ty: TokenType::BangEqual,
                col: 2,
                line: 1,
                text: "!=",
            },
            Token {
                ty: TokenType::EqualEqual,
                col: 4,
                line: 1,
                text: "==",
            },
            Token {
                ty: TokenType::Equal,
                col: 6,
                line: 1,
                text: "=",
            },
            Token {
                ty: TokenType::Greater,
                col: 7,
                line: 1,
                text: ">",
            },
            Token {
                ty: TokenType::GreaterEqual,
                col: 8,
                line: 1,
                text: ">=",
            },
            Token {
                ty: TokenType::Less,
                col: 10,
                line: 1,
                text: "<",
            },
            Token {
                ty: TokenType::LessEqual,
                col: 11,
                line: 1,
                text: "<=",
            },
            Token {
                ty: TokenType::EOF,
                col: 12,
                line: 1,
                text: "",
            },
        ];
        assert_eq!(result.unwrap(), list);
    }
}
