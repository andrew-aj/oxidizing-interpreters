use std::fmt::{format, Debug};

use crate::{
    ast::*,
    scanner::{scan_tokens, Error, Token, TokenType},
};

#[derive(Default)]
pub struct Compiler<'a> {
    tokens: Vec<Token<'a>>,
    token_index: usize,
}

impl<'a> Compiler<'a> {
    pub fn compile(&mut self, source: &'a String) -> Result<Expression<'a>, Error> {
        match scan_tokens(&source) {
            Ok(tokens) => {
                self.tokens = tokens;

                self.expression()
            }
            Err(err) => Err(err),
        }
    }

    fn expression(&mut self) -> Result<Expression<'a>, Error> {
        self.primary()
    }

    fn primary(&mut self) -> Result<Expression<'a>, Error> {
        let token = *self.peek();
        match token.ty {
            TokenType::LeftParen => {
                self.advance();
                let expr = self.expression()?;
                self.advance();
                Ok(expr)
            }
            TokenType::True | TokenType::False => Ok(Expression::Primary(Literal::Bool(token))),
            TokenType::Nil => Ok(Expression::Primary(Literal::Nil(token))),
            TokenType::This => Ok(Expression::Primary(Literal::This(token))),
            TokenType::Number => Ok(Expression::Primary(Literal::Number(token))),
            TokenType::String => Ok(Expression::Primary(Literal::String(token))),
            TokenType::Identifier => Ok(Expression::Primary(Literal::Identifier(token))),
            TokenType::Super => {
                self.advance();
                self.expect(TokenType::Dot)?;

                let ident = *self.peek();

                self.expect(TokenType::Identifier)?;

                Ok(Expression::Primary(Literal::Super(SuperExpr {
                    token,
                    ident,
                })))
            }
            _ => Err(Error {
                message: format!("Error: Expected primary but got {:?}", self.peek().ty),
                col: self.peek().col,
                line: self.peek().line,
            }),
        }
    }

    fn expect(&mut self, token: TokenType) -> Result<(), Error> {
        if !self.match_(token) {
            Err(Error {
                message: format!("Error: Expected {:?} but got {:?}", token, self.peek().ty),
                col: self.peek().col,
                line: self.peek().line,
            })
        } else {
            Ok(())
        }
    }

    fn match_(&mut self, token: TokenType) -> bool {
        match !self.check(token) {
            true => false,
            false => {
                self.advance();
                true
            }
        }
    }

    fn advance(&mut self) {
        match self.peek_type() {
            TokenType::EOF => (),
            _ => self.token_index += 1,
        };
    }

    fn check(&self, token: TokenType) -> bool {
        self.peek().ty == token
    }

    fn peek(&self) -> &Token<'a> {
        &self.tokens[self.token_index]
    }

    fn peek_type(&self) -> TokenType {
        self.tokens[self.token_index].ty
    }
}
