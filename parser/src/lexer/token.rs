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
  Comment(String),
}

impl Token {
  pub fn new(kind: TokenKind, position: Position) -> Self {
    Self { kind, position }
  }
}
