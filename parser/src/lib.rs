#![feature(box_syntax)]

mod ast;
mod lexer;

use ast::{AtomKind, Boolean, PrimitiveType, AST};
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
  (expect $self:ident, $opt:ident, $err:ident) => {{
    match capture!($self, $opt) {
      None => return Err(ParseError::$err),
      Some(res) => res,
    }
  }};
}

macro_rules! kw {
  ($self:ident, $kw:ident) => {{
    let token = $self.next();
    if let Some(tok) = token {
      match tok.kw() {
        Some(kw) if kw == Keyword::$kw => {}
        _ => return Err(ParseError::UnexpectedToken(deref_opt!(token))),
      }
    }
  }};
  (bool $self:ident, $kw:ident) => {{
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
  ($self:ident) => {{
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
  (silent $self:ident) => {{
    let token = $self.next();
    if let Some(tok) = token {
      tok.ident()
    } else {
      None
    }
  }};
}

macro_rules! sym {
  ($self:ident, $sym:ident) => {{
    let token = $self.next();
    if let Some(tok) = token {
      match tok.sym() {
        Some(kw) if kw == Symbol::$sym => {}
        _ => return Err(ParseError::UnexpectedToken(deref_opt!(token))),
      }
    }
  }};
  (silent $self:ident, $sym:ident) => {{
    let token = $self.next();
    if let Some(tok) = token {
      match tok.sym() {
        Some(kw) if kw == Symbol::$sym => {}
        _ => return None,
      }
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

  fn parse_atom(&mut self) -> ParseResult<AST> {
    match capture!(self, parse_boolean, parse_number, parse_unit) {
      None => Err(ParseError::ExpectedAtom),
      Some(at) => Ok(AST::Atom(at)),
    }
  }

  fn parse_attribute(&mut self) -> Option<String> {
    sym!(silent self, At);
    ident!(silent self)
  }

  fn parse_function(&mut self) -> ParseResult<AST> {
    let mut attributes = Vec::new();

    loop {
      match capture!(self, parse_attribute) {
        None => break,
        Some(attr) => attributes.push(attr),
      }
    }

    let ext = kw!(bool self, Extern);
    kw!(self, Fn);
    let name = ident!(self);

    sym!(self, Unit);
    sym!(self, Arrow);

    let ret_ty = capture!(expect self, parse_type, ExpectedType);

    Ok(AST::Function {
      name,
      attributes,
      ty: PrimitiveType::Function {
        ext,
        ret_ty: box ret_ty,
      },
    })
  }

  fn parse(&mut self) -> ParseResult<()> {
    let mut res = Vec::new();
    let err = loop {
      match self.peek() {
        Some(tok) if tok.kind == TokenKind::EOF => break Ok(()),
        Some(tok) => tok,
        None => break Err(ParseError::InvalidEOF),
      };

      res.push(
        self
          .parse_atom()
          .or_else(|_| self.parse_function())
          .unwrap_or_else(|r| {
            panic!(r);
          }),
      );
    };

    self.pt = Some(AST::File(res));

    err
  }
}

#[cfg(test)]
mod parser_tests {
  use super::{
    lexer::{token::Token, Lexer},
    AtomKind, Boolean, ParseError, Parser, PrimitiveType, AST,
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
  fn true_false() {
    let pt = parse_test!("true false", "TF");

    assert_eq!(
      pt,
      AST::File(vec![
        AST::Atom(AtomKind::Bool(Boolean::True)),
        AST::Atom(AtomKind::Bool(Boolean::False))
      ])
    );
  }

  #[test]
  fn unit() {
    let pt = parse_test!("()", "Unit value");

    assert_eq!(pt, AST::File(vec![AST::Atom(AtomKind::Unit)]));
  }

  #[test]
  fn int() {
    let pt = parse_test!("0xdeadbeef", "DeadBeef");

    assert_eq!(
      pt,
      AST::File(vec![AST::Atom(AtomKind::Integer(0xdeadbeef)),])
    );
  }

  #[test]
  fn unit_function() {
    let pt = parse_test!("fn test () -> ()", "Unit Function");

    assert_eq!(
      pt,
      AST::File(vec![AST::Function {
        name: "test".to_string(),
        attributes: Vec::new(),
        ty: PrimitiveType::Function {
          ext: false,
          ret_ty: box PrimitiveType::Unit
        }
      },])
    );
  }

  #[test]
  fn extern_function() {
    let pt = parse_test!("extern fn test () -> ()", "Extern Function");

    assert_eq!(
      pt,
      AST::File(vec![AST::Function {
        name: "test".to_string(),
        attributes: Vec::new(),
        ty: PrimitiveType::Function {
          ext: true,
          ret_ty: box PrimitiveType::Unit
        }
      },])
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
      AST::File(vec![AST::Function {
        name: "main".to_string(),
        attributes: attr(vec!["entry"]),
        ty: PrimitiveType::Function {
          ext: false,
          ret_ty: box PrimitiveType::Unit
        }
      },])
    );
  }
}
