use std::str::FromStr;

#[derive(Debug, Eq, PartialEq)]
pub struct Token {
  pub kind: TokenKind,
  pub position: Position,
}

pub type Position = (usize, usize);

#[derive(Debug, Eq, PartialEq)]
pub enum TokenKind {
  EOF,
  Symbol(Symbol),
  Comment(String),
  NumberLiteral(u64),
  Identifier(String),
  Keyword(Keyword),
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum Symbol {
  LParen,
  RParen,
  Arrow,
  LCurly,
  RCurly,
  At,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum Keyword {
  Fn,
}

impl Token {
  pub fn new(kind: TokenKind, position: Position) -> Self {
    Self { kind, position }
  }

  pub fn keyword(kw: &str) -> Option<TokenKind> {
    match Keyword::from_str(kw) {
      Ok(k) => Some(TokenKind::Keyword(k)),
      Err(_) => None,
    }
  }

  pub fn kw(&self) -> Option<Keyword> {
    match self.kind {
      TokenKind::Keyword(kw) => Some(kw),
      _ => None,
    }
  }

  pub fn sym(&self) -> Option<Symbol> {
    match self.kind {
      TokenKind::Symbol(sym) => Some(sym),
      _ => None,
    }
  }
}

impl FromStr for Keyword {
  type Err = ();

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "fn" => Ok(Self::Fn),
      _ => Err(()),
    }
  }
}
