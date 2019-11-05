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

  fn advance(&mut self) {
    self.read();
  }

  fn is_next_insensitive(&mut self, peek: char) -> bool {
    let result = self.chars.peek().map(|c| c.to_lowercase().to_string())
      == Some(peek.to_lowercase().to_string());
    if result {
      self.advance();
    }
    result
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

  fn eat_whitespace(&mut self) {
    loop {
      if self.chars.peek().map(|c| c.is_whitespace()).unwrap_or(false) {
        self.advance();
      } else {
        break;
      }
    }
  }

  pub fn lex(&mut self) {
    loop {
      self.eat_whitespace();
      let pos = (self.col, self.row);
      match self.read() {
        Some('(') => {
          self.push_token(TokenKind::LParen, pos);
        }
        Some(')') => {
          self.push_token(TokenKind::RParen, pos);
        }
        Some('/') => match self.read() {
          Some('/') => {
            let comm = self.read_eol();
            self.push_token(TokenKind::Comment(comm), pos);
          }
          _ => panic!("Expected /, *, ="),
        },
        Some('0') => {
          let mut buf = String::new();
          let num = if self.is_next_insensitive('x') {
            while let Some(c) = self.chars.peek() {
              if c.is_digit(16) {
                buf.push(self.read().expect("Lexer died @hex"));
              } else if *c == '_' {
                self.advance();
              } else {
                break;
              }
            }
            u64::from_str_radix(&buf, 16).expect("Lexer died @hex -> u64")
          } else {
            while let Some(ch) = self.chars.peek() {
              match ch {
                '_' => self.advance(),
                _ if ch.is_digit(10) => buf.push(self.read().expect("Lexer died @digits")),
                _ => break,
              };
            }
            u64::from_str_radix(&buf, 10).expect("Lexer died @hex -> u64")
          };
          self.push_token(TokenKind::NumberLiteral(num), pos);
        }
        Some(c) if c.is_digit(10) => {
          let mut buf = c.to_string();
          while let Some(ch) = self.chars.peek() {
            match ch {
              '_' => self.advance(),
              _ if ch.is_digit(10) => buf.push(self.read().expect("Lexer died @digits")),
              _ => break,
            };
          }

          self.push_token(
            TokenKind::NumberLiteral(
              u64::from_str_radix(&buf, 10).expect("Lexer died @digits -> u64"),
            ),
            pos,
          );
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

  #[test]
  fn single_line_comment_with_whitespace() {
    let mut lexer = Lexer::new("                 //Test");
    lexer.lex();
    let mut tokens = lexer.into_iter();

    assert_eq!(
      tokens.next().unwrap().kind,
      TokenKind::Comment(String::from("Test"))
    );
  }

  #[test]
  fn single_number_literal() {
    let mut lexer = Lexer::new("18_446_744_073_709_551_614");
    lexer.lex();
    let mut tokens = lexer.into_iter();

    assert_eq!(
      tokens.next().unwrap().kind,
      TokenKind::NumberLiteral(18_446_744_073_709_551_614u64),
    )
  }

  #[test]
  fn single_number_literal_zero() {
    let mut lexer = Lexer::new("0123456789");
    lexer.lex();
    let mut tokens = lexer.into_iter();

    assert_eq!(
      tokens.next().unwrap().kind,
      TokenKind::NumberLiteral(0123456789),
    )
  }

  #[test]
  fn single_number_literal_hex() {
    let mut lexer = Lexer::new("0xdeadbeef");
    lexer.lex();
    let mut tokens = lexer.into_iter();
    assert_eq!(
      tokens.next().unwrap().kind,
      TokenKind::NumberLiteral(0xdeadbeef),
    )
  }

  #[test]
  fn single_number_literal_hex_separator() {
    let mut lexer = Lexer::new("0xdead_beef");
    lexer.lex();
    let mut tokens = lexer.into_iter();
    assert_eq!(
      tokens.next().unwrap().kind,
      TokenKind::NumberLiteral(0xdeadbeef),
    )
  }
}
