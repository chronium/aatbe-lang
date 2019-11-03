use std::{iter::Peekable, str::Chars};

pub mod token;

use token::{Position, Token, TokenKind};

pub struct Lexer<'c> {
  tokens: Vec<Token>,
  chars: Peekable<Chars<'c>>,
  col: usize,
  row: usize,
}

impl<'c> Lexer<'c> {
  pub fn new(code: &'c str) -> Self {
    Self {
      tokens: Vec::new(),
      chars: code.chars().peekable(),
      col: 0,
      row: 1,
    }
  }

  fn read(&mut self) -> Option<char> {
    if let Some('\n') = self.chars.peek() {
      self.col = 0;
      self.row += 1;
    }

    self.col += 1;
    self.chars.next()
  }

  fn read_eol(&mut self) -> String {
    let mut buf = String::new();
    loop {
      let c = self.read();
      match c {
        Some(ch) if ch.is_ascii_control() => break,
        Some(ch) => buf.push(ch),
        _ => break,
      }
    }
    buf
  }

  fn push_token(&mut self, token: TokenKind, pos: Position) {
    self.tokens.push(Token::new(token, pos));
  }

  pub fn lex(&mut self) {
    loop {
      let pos = (self.col, self.row);
      match self.chars.peek() {
        Some('(') => {
          self.read();
          self.push_token(TokenKind::LParen, pos);
        }
        Some(')') => {
          self.read();
          self.push_token(TokenKind::RParen, pos);
        }
        Some('/') => {
          self.read();
          match self.read() {
            Some('/') => {
              let comm = self.read_eol();
              self.push_token(TokenKind::Comment(comm), pos);
            }
            _ => panic!("Expected /, *, ="),
          }
        }
        _ => {
          self.push_token(TokenKind::EOF, pos);
          break;
        }
      };
    }
  }
}

impl IntoIterator for Lexer<'_> {
  type Item = Token;
  type IntoIter = ::std::vec::IntoIter<Self::Item>;

  fn into_iter(self) -> Self::IntoIter {
    self.tokens.into_iter()
  }
}

#[cfg(test)]
mod tests {
  use super::{Lexer, TokenKind};
  #[test]
  fn end_of_file() {
    let mut lexer = Lexer::new("");
    lexer.lex();
    let mut tokens = lexer.into_iter();

    assert_eq!(tokens.next().unwrap().kind, TokenKind::EOF);
  }

  #[test]
  fn parentheses() {
    let mut lexer = Lexer::new("()");
    lexer.lex();
    let mut tokens = lexer.into_iter();

    assert_eq!(tokens.next().unwrap().kind, TokenKind::LParen);
    assert_eq!(tokens.next().unwrap().kind, TokenKind::RParen);
  }

  #[test]
  fn single_line_comment() {
    let mut lexer = Lexer::new("//Test");
    lexer.lex();
    let mut tokens = lexer.into_iter();

    assert_eq!(
      tokens.next().unwrap().kind,
      TokenKind::Comment(String::from("Test"))
    );
  }
}
