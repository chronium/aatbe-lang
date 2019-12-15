#![feature(box_syntax)]
#![allow(dead_code)]

mod ast;
mod lexer;

use ast::{AtomKind, Boolean, Expression, PrimitiveType, AST};
use lexer::{
  token,
  token::{Keyword, Symbol, Token, TokenKind},
};

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
  ($self:ident, $opt:ident, $($opts:ident),*) => {{
    match capture!($self, $opt) {
      None => capture!($self, $($opts),*),
      Some(ast) => Some(ast),
    }
  }};
  (expect $opt:ident, err $err:ident, $self:ident) => {{
    match capture!($self, $opt) {
      None => return Err(ParseError::$err),
      Some(res) => res,
    }
  }};
  (box $opt:ident, err $err:ident, $self:ident) => {{
    match capture!($self, $opt) {
      None => return Err(ParseError::$err),
      Some(res) => Some(box res),
    }
  }};
}

macro_rules! kw {
  ($kw:ident, $self:ident) => {{
    let token = $self.next();
    if let Some(tok) = token {
      match tok.kw() {
        Some(kw) if kw == Keyword::$kw => {}
        _ => return Err(ParseError::UnexpectedToken(deref_opt!(token))),
      }
    }
  }};
  (bool $kw:ident, $self:ident) => {{
    let prev_ind = $self.index;
    let token = $self.next();
    if let Some(tok) = token {
      match tok.kw() {
        Some(kw) if kw == Keyword::$kw => true,
        _ => {
          $self.index = prev_ind;
          false
        }
      }
    } else {
      false
    }
  }};
}

macro_rules! ident {
  (required $self:ident) => {{
    let token = $self.next();
    if let Some(tok) = token {
      match tok.ident() {
        Some(id) => id,
        _ => return Err(ParseError::ExpectedIdent),
      }
    } else {
      return Err(ParseError::ExpectedIdent);
    }
  }};
  ($self:ident) => {{
    let token = $self.next();
    if let Some(tok) = token {
      tok.ident()
    } else {
      None
    }
  }};
}

macro_rules! sym {
  (required $sym:ident, $self:ident) => {{
    let token = $self.next();
    if let Some(tok) = token {
      match tok.sym() {
        Some(kw) if kw == Symbol::$sym => {}
        _ => return Err(ParseError::UnexpectedToken(deref_opt!(token))),
      }
    }
  }};
  ($sym:ident, $self:ident) => {{
    let token = $self.next();
    if let Some(tok) = token {
      match tok.sym() {
        Some(kw) if kw == Symbol::$sym => {}
        _ => return None,
      }
    }
  }};
  (bool $sym:ident, $self:ident) => {{
    let prev_ind = $self.index;
    let token = $self.next();
    if let Some(tok) = token {
      match tok.sym() {
        Some(sym) if sym == Symbol::$sym => true,
        _ => {
          $self.index = prev_ind;
          false
        }
      }
    } else {
      false
    }
  }};
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParseError {
  InvalidEOF,
  UnexpectedToken(Option<Token>),
  ExpectedType,
  ExpectedAtom,
  ExpectedIdent,
  ExpectedExpression,
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

  fn parse_type(&mut self) -> Option<PrimitiveType> {
    let token = self.next();
    if let Some(tok) = token {
      if let Some(Symbol::Unit) = tok.sym() {
        return Some(PrimitiveType::Unit);
      }
    }

    None
  }

  fn parse_boolean(&mut self) -> Option<AtomKind> {
    let token = self.next();

    if let Some(tok) = token {
      match tok.bl() {
        Some(token::Boolean::True) => Some(AtomKind::Bool(Boolean::True)),
        Some(token::Boolean::False) => Some(AtomKind::Bool(Boolean::False)),
        _ => None,
      }
    } else {
      None
    }
  }

  fn parse_number(&mut self) -> Option<AtomKind> {
    let token = self.next();
    if let Some(tok) = token {
      if let Some(val) = tok.int() {
        return Some(AtomKind::Integer(val));
      }
    }

    None
  }

  fn parse_unit(&mut self) -> Option<AtomKind> {
    let token = self.next();
    if let Some(tok) = token {
      if let Some(Symbol::Unit) = tok.sym() {
        return Some(AtomKind::Unit);
      }
    }

    None
  }

  fn parse_expression(&mut self) -> Option<Expression> {
    match capture!(self, parse_boolean, parse_number, parse_unit) {
      None => None,
      Some(at) => Some(Expression::Atom(at)),
    }
  }

  fn parse_attribute(&mut self) -> Option<String> {
    sym!(At, self);
    ident!(self)
  }

  fn parse_function(&mut self) -> ParseResult<AST> {
    let mut attributes = Vec::new();

    loop {
      match capture!(self, parse_attribute) {
        None => break,
        Some(attr) => attributes.push(attr),
      }
    }

    let ext = kw!(bool Extern, self);
    kw!(Fn, self);
    let name = ident!(required self);

    sym!(required Unit, self);
    sym!(required Arrow, self);

    let ret_ty = capture!(expect parse_type, err ExpectedType, self);

    let mut body = None;
    if sym!(bool Assign, self) {
      body = capture!(box parse_expression, err ExpectedExpression, self);
    }

    Ok(AST::Expr(Expression::Function {
      name,
      attributes,
      body,
      ty: PrimitiveType::Function {
        ext,
        ret_ty: box ret_ty,
      },
    }))
  }

  fn parse(&mut self) -> ParseResult<()> {
    let mut res = Vec::new();
    let err = loop {
      match self.peek() {
        Some(tok) if tok.kind == TokenKind::EOF => break Ok(()),
        Some(tok) => tok,
        None => break Err(ParseError::InvalidEOF),
      };

      res.push(self.parse_function().unwrap_or_else(|r| {
        panic!(r);
      }));
    };

    self.pt = Some(AST::File(res));

    err
  }
}

#[cfg(test)]
mod parser_tests {
  use super::{
    lexer::{token::Token, Lexer},
    AtomKind, Boolean, Expression, ParseError, Parser, PrimitiveType, AST,
  };

  fn tt(code: &'static str) -> Vec<Token> {
    let mut lexer = Lexer::new(code);
    lexer.lex();

    lexer.tt()
  }

  fn attr(attribs: Vec<&'static str>) -> Vec<String> {
    attribs.iter().map(|attr| attr.to_string()).collect()
  }

  macro_rules! parse_test {
    ($tt:expr, $name:expr) => {{
      let mut parser = Parser::new(tt($tt));
      let res = parser.parse();
      let pt = parser.pt().as_ref().expect($name);

      assert_eq!(res, Ok(()));

      pt.clone()
    }};
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
  fn unit_function() {
    let pt = parse_test!("fn test () -> ()", "Unit Function");

    assert_eq!(
      pt,
      AST::File(vec![AST::Expr(Expression::Function {
        name: "test".to_string(),
        attributes: Vec::new(),
        body: None,
        ty: PrimitiveType::Function {
          ext: false,
          ret_ty: box PrimitiveType::Unit
        }
      }),])
    );
  }

  #[test]
  fn extern_function() {
    let pt = parse_test!("extern fn test () -> ()", "Extern Function");

    assert_eq!(
      pt,
      AST::File(vec![AST::Expr(Expression::Function {
        name: "test".to_string(),
        attributes: Vec::new(),
        body: None,
        ty: PrimitiveType::Function {
          ext: true,
          ret_ty: box PrimitiveType::Unit
        }
      }),])
    );
  }

  #[test]
  fn entry_function() {
    let pt = parse_test!(
      "
@entry
fn main () -> ()
",
      "Entry Function"
    );

    assert_eq!(
      pt,
      AST::File(vec![AST::Expr(Expression::Function {
        name: "main".to_string(),
        attributes: attr(vec!["entry"]),
        body: None,
        ty: PrimitiveType::Function {
          ext: false,
          ret_ty: box PrimitiveType::Unit
        }
      }),])
    );
  }

  #[test]
  fn expr_function() {
    let pt = parse_test!(
      "
@entry
fn main () -> () = ()
",
      "Entry Function"
    );

    assert_eq!(
      pt,
      AST::File(vec![AST::Expr(Expression::Function {
        name: "main".to_string(),
        attributes: attr(vec!["entry"]),
        body: Some(box Expression::Atom(AtomKind::Unit)),
        ty: PrimitiveType::Function {
          ext: false,
          ret_ty: box PrimitiveType::Unit
        }
      }),])
    );
  }
}
