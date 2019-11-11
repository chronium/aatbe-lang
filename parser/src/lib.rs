use std::iter::Peekable;

mod ast;
mod lexer;

use ast::AST;
use lexer::token::{Keyword, Token, TokenKind};

#[derive(Debug, PartialEq, Eq)]
pub enum ParseError {
  InvalidEOF,
  UnexpectedToken(Token),
}

type ParseResult = Result<(), ParseError>;

pub struct Parser<Tokens>
where
  Tokens: Iterator<Item = Token>,
{
  tt: Peekable<Tokens>,
  pt: Option<AST>,
}

impl<Tokens> Parser<Tokens>
where
  Tokens: Iterator<Item = Token>,
{
  fn new(tt: Tokens) -> Self {
    Self {
      tt: tt.peekable(),
      pt: None,
    }
  }

  fn pt(&self) -> &Option<AST> {
    &self.pt
  }

  fn parse(&mut self) -> ParseResult {
    let mut res = Vec::new();
    let err = loop {
      let t = match self.tt.next() {
        Some(tok) if tok.kind == TokenKind::EOF => break Ok(()),
        Some(tok) => tok,
        None => break Err(ParseError::InvalidEOF),
      };

      match t.kind {
        TokenKind::Keyword(Keyword::True) => res.push(AST::True),
        TokenKind::Keyword(Keyword::False) => res.push(AST::False),
        _ => break Err(ParseError::UnexpectedToken(t)),
      }
    };

    self.pt = Some(AST::File(res));

    err
  }
}

#[cfg(test)]
mod parser_tests {
  use super::{lexer::Lexer, ParseError, Parser, AST};

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

  #[test]
  fn true_false() {
    let mut parser = Parser::new(tt("true false").into_iter());
    let res = parser.parse();
    let pt = parser.pt().as_ref().expect("TF Test Failed");

    assert_eq!(res, Ok(()));

    assert_eq!(pt, &AST::File(vec![AST::True, AST::False]));
  }
}
