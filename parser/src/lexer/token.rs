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
  LParen,
  RParen,
  Arrow,
  LCurly,
  RCurly,
  At,
  Comment(String),
  NumberLiteral(u64),
  Identifier(String),
  Keyword(Keyword),
}

#[derive(Debug, Eq, PartialEq)]
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
}

impl FromStr for Keyword {
  type Err = ();

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let sl = s;
    match sl {
      "fn" => Ok(Self::Fn),
      _ => Err(()),
    }
  }
}
