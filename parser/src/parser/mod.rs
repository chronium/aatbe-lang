#[macro_use]
pub mod macros;
pub mod expression;

use crate::{
    ast::AST,
    token::{Token, TokenKind},
};

#[derive(Debug, PartialEq, Eq)]
pub enum ParseError {
    InvalidEOF,
    UnexpectedToken(Option<Token>),
    ExpectedType,
    ExpectedAtom,
    ExpectedIdent,
    ExpectedExpression,
    ExpectedOperator,
}

pub type ParseResult<T> = Result<T, ParseError>;

pub struct Parser {
    pub tt: Vec<Token>,
    pub pt: Option<AST>,
    pub index: usize,
}

impl Parser {
    pub fn new(tt: Vec<Token>) -> Self {
        Self {
            tt,
            pt: None,
            index: 0usize,
        }
    }

    pub fn pt(&self) -> &Option<AST> {
        &self.pt
    }

    pub fn next(&mut self) -> Option<&Token> {
        if (self.index) >= self.tt.len() {
            None
        } else {
            let ret = peek!(self.tt, self.index);
            self.index += 1;
            ret.clone()
        }
    }

    pub fn peek(&self) -> Option<&Token> {
        peek!(self.tt, self.index)
    }

    pub fn parse(&mut self) -> ParseResult<()> {
        let mut res = Vec::new();
        let err = loop {
            match self.peek() {
                Some(tok) if tok.kind == TokenKind::EOF => break Ok(()),
                Some(tok) => tok,
                None => break Err(ParseError::InvalidEOF),
            };

            res.push(self.parse_function().unwrap_or_else(|r| {
                panic!("{:?}", r);
            }));
        };

        self.pt = Some(AST::File(res));

        err
    }
}
