mod ast;
mod lexer;

use ast::{PrimitiveType, AST};
use lexer::token::{Boolean, Keyword, Symbol, Token, TokenKind};

macro_rules! peek {
  ($tt: expr, $ind: expr) => {
    if ($ind) >= $tt.len() {
      None
    } else {
      Some(&$tt[$ind])
    }
  };
}

// Used to clone a token reference for an Option for parser errors. May not be needed.
macro_rules! deref_opt {
  ($opt: ident) => {
    $opt.map(|t| t.clone())
  };
}

macro_rules! capture {
  ($self:ident, $opt:ident) => {{
    let prev_ind = $self.index;
    match $self.$opt() {
      None => {
        $self.index = prev_ind;
        None
      }
      capt => capt,
    }
  }};
  ($self:ident, $opt:ident, $($opts:ident),+) => {{
    match capture!($self, $opt) {
      None => capture!($self, $($opts),+),
      Some(ast) => Some(ast),
    }
    .unwrap_or(AST::Error)
  }};
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParseError {
  InvalidEOF,
  UnexpectedToken(Token),
  ExpectedType(Option<Token>),
  ExpectedBoolean(Option<Token>),
}

type ParseResult<T> = Result<T, ParseError>;

pub struct Parser {
  tt: Vec<Token>,
  pt: Option<AST>,
  index: usize,
}

impl Parser {
  fn new(tt: Vec<Token>) -> Self {
    Self {
      tt,
      pt: None,
      index: 0usize,
    }
  }

  fn pt(&self) -> &Option<AST> {
    &self.pt
  }

  fn next(&mut self) -> Option<&Token> {
    if (self.index) >= self.tt.len() {
      None
    } else {
      let ret = peek!(self.tt, self.index);
      self.index += 1;
      ret.clone()
    }
  }

  fn peek(&self) -> Option<&Token> {
    peek!(self.tt, self.index)
  }

  fn parse_type(&mut self) -> Option<AST> {
    let token = self.next();
    if let Some(tok) = token {
      if let Some(Symbol::Unit) = tok.sym() {
        return Some(AST::Type(PrimitiveType::Unit));
      }
    }

    None
  }

  fn parse_boolean(&mut self) -> Option<AST> {
    let token = self.next();

    if let Some(tok) = token {
      match tok.bl() {
        Some(Boolean::True) => Some(AST::True),
        Some(Boolean::False) => Some(AST::False),
        _ => None,
      }
    } else {
      None
    }
  }

  fn parse(&mut self) -> ParseResult<()> {
    let mut res = Vec::new();
    let err = loop {
      match self.peek() {
        Some(tok) if tok.kind == TokenKind::EOF => break Ok(()),
        Some(tok) => tok,
        None => break Err(ParseError::InvalidEOF),
      };

      res.push(capture!(self, parse_type, parse_boolean));
    };

    self.pt = Some(AST::File(res));

    err
  }
}

#[cfg(test)]
mod parser_tests {
  use super::{
    lexer::{token::Token, Lexer},
    ParseError, Parser, PrimitiveType, AST,
  };

  fn tt(code: &'static str) -> Vec<Token> {
    let mut lexer = Lexer::new(code);
    lexer.lex();

    lexer.tt()
  }

  #[test]
  fn invalid_eof() {
    let mut parser = Parser::new(vec![]);
    let res = parser.parse();
    assert_eq!(
      res.expect_err("Expected InvalidEOF"),
      ParseError::InvalidEOF
    );
  }

  #[test]
  fn eof() {
    let mut parser = Parser::new(tt(""));
    let res = parser.parse();

    assert_eq!(res, Ok(()))
  }

  #[test]
  fn true_false() {
    let mut parser = Parser::new(tt("true false"));
    let res = parser.parse();
    let pt = parser.pt().as_ref().expect("TF Test Failed");

    assert_eq!(res, Ok(()));

    assert_eq!(pt, &AST::File(vec![AST::True, AST::False]));
  }

  #[test]
  fn unit_type() {
    let mut parser = Parser::new(tt("()"));
    let res = parser.parse();
    let pt = parser.pt().as_ref().expect("unit_type Test Failed");

    assert_eq!(res, Ok(()));

    assert_eq!(pt, &AST::File(vec![AST::Type(PrimitiveType::Unit)]));
  }
}
