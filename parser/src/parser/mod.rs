#[macro_use]
pub mod macros;
pub mod atom;
pub mod conditionals;
pub mod expression;
pub mod pass;

use crate::{
    ast::AST,
    token::{Symbol, Token, TokenKind},
};

#[derive(Debug, PartialEq, Eq)]
pub enum ParseError {
    InvalidEOF,
    UnexpectedToken,
    ExpectedType,
    ExpectedAtom,
    ExpectedIdent,
    ExpectedExpression,
    ExpectedOperator,
    NotEnoughArguments(String),
    UnexpectedEOF,
    ExpectedElseExpression,
    ExpectedCondition,
    ExpectedThenExpression,
    ExpectedPath,
    ExpectedLValue,
    Continue,
}

pub type ParseResult<T> = Result<T, ParseError>;

pub struct Parser {
    pub tt: Vec<Token>,
    pub index: usize,
    pub variants: Vec<String>,
}

impl Parser {
    pub fn new(tt: Vec<Token>) -> Self {
        Self {
            tt,
            index: 0usize,
            variants: Vec::new(),
        }
    }

    pub fn next(&mut self) -> Option<Token> {
        if (self.index) >= self.tt.len() {
            None
        } else {
            let ret = self.peek().cloned();
            self.index += 1;
            ret
        }
    }

    pub fn peek_rel(&self, offs: isize) -> Option<&Token> {
        if self.index as isize + offs < 0 {
            panic!("Peek relative < 0");
        }
        peek!(self.tt, (self.index as isize + offs) as usize)
    }

    pub fn peek(&mut self) -> Option<&Token> {
        loop {
            match peek!(self.tt, self.index).map_or(None, |t| t.comm()) {
                Some(_) => self.index += 1,
                None => {}
            }

            match peek!(self.tt, self.index).map_or(false, |t| {
                t.kind == TokenKind::EOL || t.kind == TokenKind::SEP
            }) {
                true => self.index += 1,
                false => break,
            }
        }
        peek!(self.tt, self.index)
    }

    pub fn eof(&self) -> bool {
        match peek!(self.tt, self.index) {
            Some(tok) if tok.kind == TokenKind::EOF => true,
            None => true,
            _ => false,
        }
    }

    pub fn nl(&mut self) -> bool {
        match peek!(self.tt, self.index) {
            Some(tok) if tok.kind == TokenKind::EOL => {
                self.index += 1;
                true
            }
            None => true,
            _ => false,
        }
    }

    pub fn sep(&mut self) -> bool {
        match peek!(self.tt, self.index) {
            Some(tok) if tok.kind == TokenKind::SEP => {
                self.index += 1;
                true
            }
            None => true,
            _ => false,
        }
    }

    pub fn peek_ident(&mut self) -> Option<String> {
        self.peek().and_then(|t| t.ident())
    }

    pub fn peek_str(&mut self) -> Option<String> {
        self.peek().and_then(|t| t.st())
    }

    pub fn peek_symbol(&mut self, symbol: Symbol) -> Option<bool> {
        self.peek()
            .and_then(|t| t.sym())
            .map_or(Some(false), |t| Some(t == symbol))
    }

    pub fn parse(&mut self) -> ParseResult<AST> {
        let mut res = vec![];
        loop {
            match self.peek() {
                Some(tok) if tok.kind == TokenKind::EOF => break Ok(()),
                Some(tok) => tok,
                None => break Err(ParseError::InvalidEOF),
            };

            res.push(
                capture!(res parse_use, self)
                    .or_else(|_| capture!(res parse_function, self))
                    .or_else(|_| capture!(res parse_record, self))
                    .or_else(|_| capture!(res parse_const_global, self))
                    .or_else(|_| capture!(res parse_typedef, self))
                    .unwrap_or_else(|r| panic!("{:?} {:?}", r, self.peek())),
            );
        }?;

        Ok(pass::type_resolution(&self.variants, &AST::File(res)))
    }
}
