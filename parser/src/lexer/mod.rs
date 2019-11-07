use std::{iter::Peekable, str::Chars};

pub mod token;

use token::{Keyword, Position, Symbol, Token, TokenKind};

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
    if self
      .chars
      .peek()
      .map(|c| c.to_lowercase())
      .map(|s| peek.to_lowercase().eq(s))
      .unwrap_or(false)
    {
      self.advance();
      true
    } else {
      false
    }
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

  fn push_symbol(&mut self, sym: Symbol, pos: Position) {
    self.tokens.push(Token::new(TokenKind::Symbol(sym), pos));
  }

  fn eat_whitespace(&mut self) {
    loop {
      if self
        .chars
        .peek()
        .map(|c| c.is_whitespace())
        .unwrap_or(false)
      {
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

      let c = match self.read() {
        Some(c) => c,
        None => {
          self.push_token(TokenKind::EOF, pos);
          break;
        }
      };

      match c {
        '(' => {
          self.push_symbol(Symbol::LParen, pos);
        }
        ')' => {
          self.push_symbol(Symbol::RParen, pos);
        }
        '{' => {
          self.push_symbol(Symbol::LCurly, pos);
        }
        '}' => {
          self.push_symbol(Symbol::RCurly, pos);
        }
        '-' => match self.chars.peek() {
          Some('>') => {
            self.advance();
            self.push_symbol(Symbol::Arrow, pos)
          }
          _ => {}
        },
        '@' => {
          self.push_symbol(Symbol::At, pos);
        }
        '/' => match self.read() {
          Some('/') => {
            let comm = self.read_eol();
            self.push_token(TokenKind::Comment(comm), pos);
          }
          _ => panic!("Expected /, *, ="),
        },
        'a'..='z' | 'A'..='Z' | '_' => {
          let mut buf = c.to_string();
          while let Some(c) = self.chars.peek() {
            match c {
              'a'..='z' | 'A'..='Z' | '_' | '-' | '0'..='9' => {
                buf.push(*c);
                self.advance();
              }
              _ => break,
            }
          }

          self.push_token(
            Token::keyword(buf.as_ref()).unwrap_or(TokenKind::Identifier(buf)),
            pos,
          );
        }
        '0' => {
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
        c if c.is_digit(10) => {
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
        _ => panic!("Unhandled token"),
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
  use super::{Keyword, Lexer, Symbol, TokenKind};
  #[test]
  fn end_of_file() {
    let mut lexer = Lexer::new("");
    lexer.lex();
    let mut tokens = lexer.into_iter();

    assert_eq!(tokens.next().unwrap().kind, TokenKind::EOF);
  }

  #[test]
  fn symbols() {
    let mut lexer = Lexer::new("@()->{}");
    lexer.lex();
    let mut tokens = lexer.into_iter();

    assert_eq!(tokens.next().unwrap().sym(), Some(Symbol::At));
    assert_eq!(tokens.next().unwrap().sym(), Some(Symbol::LParen));
    assert_eq!(tokens.next().unwrap().sym(), Some(Symbol::RParen));
    assert_eq!(tokens.next().unwrap().sym(), Some(Symbol::Arrow));
    assert_eq!(tokens.next().unwrap().sym(), Some(Symbol::LCurly));
    assert_eq!(tokens.next().unwrap().sym(), Some(Symbol::RCurly));
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

  #[test]
  fn keyword_identifier() {
    let mut lexer = Lexer::new("fn main");
    lexer.lex();
    let mut tokens = lexer.into_iter();

    assert_eq!(tokens.next().unwrap().kw(), Some(Keyword::Fn));
    assert_eq!(
      tokens.next().unwrap().kind,
      TokenKind::Identifier(String::from("main")),
    );
  }
}
