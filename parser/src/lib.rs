use std::iter::Peekable;

mod ast;
mod lexer;

use ast::AST;
use lexer::token::{Token, TokenKind};

#[derive(Debug, PartialEq, Eq)]
pub enum ParseError {
  InvalidEOF,
}

type ParseResult = Result<(), ParseError>;

pub struct Parser<Iter>
where
  Iter: Iterator<Item = Token>,
{
  tt: Peekable<Iter>,
  pt: Option<AST>,
}

impl<Iter> Parser<Iter>
where
  Iter: Iterator<Item = Token>,
{
  fn new(tt: Iter) -> Self {
    Self {
      tt: tt.peekable(),
      pt: None,
    }
  }

  fn pt(&self) -> &Option<AST> {
    &self.pt
  }

  fn parse(&mut self) -> ParseResult {
    loop {
      let t = match self.tt.next() {
        Some(tok) if tok.kind == TokenKind::EOF => break Ok(()),
        Some(tok) => tok,
        None => break Err(ParseError::InvalidEOF),
      };
    }
  }
}

#[cfg(test)]
mod parser_tests {
  use super::{lexer::Lexer, ParseError, Parser};

  fn tt(code: &'static str) -> Lexer<'static> {
    let mut lexer = Lexer::new(code);
    lexer.lex();

    lexer
  }

  #[test]
  fn invalid_eof() {
    let mut parser = Parser::new(vec![].into_iter());
    let res = parser.parse();
    assert_eq!(
      res.expect_err("Expected InvalidEOF"),
      ParseError::InvalidEOF
    );
  }

  #[test]
  fn eof() {
    let mut parser = Parser::new(tt("").into_iter());
    let res = parser.parse();

    assert_eq!(res, Ok(()))
  }
}
