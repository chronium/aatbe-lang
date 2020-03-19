use std::{iter::Peekable, str::Chars, str::FromStr};

pub mod token;

use token::{Position, Symbol, Token, TokenKind};

mod tests;

pub struct Lexer<'c> {
    tokens: Vec<Token>,
    chars: Peekable<Chars<'c>>,
    col: usize,
    row: usize,
}

impl<'c> Lexer<'c> {
    pub fn new(code: &'c str) -> Self {
        Self {
            tokens: vec![],
            chars: code.chars().peekable(),
            col: 1,
            row: 1,
        }
    }

    pub fn tt(&self) -> Vec<Token> {
        self.tokens.clone()
    }

    fn read(&mut self) -> Option<char> {
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
        let mut eaten = false;
        loop {
            if self
                .chars
                .peek()
                .map(|c| c.is_whitespace())
                .unwrap_or(false)
            {
                eaten = true;
                self.advance();
            } else {
                break;
            }
        }

        if eaten {
            self.push_token(TokenKind::SEP, (self.col, self.row));
        }
    }

    pub fn escape_char(&mut self) -> char {
        match self.chars.peek() {
            Some('n') => {
                self.advance();
                '\n'
            }
            Some('t') => {
                self.advance();
                '\t'
            }
            Some('"') => {
                self.advance();
                '"'
            }
            Some(c) => *c,
            None => '\\',
        }
    }

    pub fn lex(&mut self) {
        loop {
            match self.chars.peek() {
                Some('\n') => {
                    self.advance();
                    let pos = (self.col, self.row);
                    self.col = 1;
                    self.row += 1;
                    self.push_token(TokenKind::EOL, pos);
                }
                Some('\r') => {
                    if let Some('\n') = self.chars.peek() {
                        self.advance();
                        let pos = (self.col, self.row);
                        self.col = 1;
                        self.row += 1;
                        self.push_token(TokenKind::EOL, pos);
                    } else {
                        let pos = (self.col, self.row);
                        panic!("Expected \\n after \\r at {:?}", pos);
                    }
                }
                _ => {}
            }
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
                '(' => match self.chars.peek() {
                    Some(')') => {
                        self.advance();
                        self.push_symbol(Symbol::Unit, pos)
                    }
                    _ => self.push_symbol(Symbol::LParen, pos),
                },
                ')' => {
                    self.push_symbol(Symbol::RParen, pos);
                }
                '{' => {
                    self.push_symbol(Symbol::LCurly, pos);
                }
                '}' => {
                    self.push_symbol(Symbol::RCurly, pos);
                }
                '[' => {
                    self.push_symbol(Symbol::LBracket, pos);
                }
                ']' => {
                    self.push_symbol(Symbol::RBracket, pos);
                }
                '.' => match self.chars.peek() {
                    Some('.') => {
                        self.advance();
                        match self.chars.peek() {
                            Some('.') => {
                                self.advance();
                                self.push_symbol(Symbol::GoDot, pos)
                            }
                            _ => self.push_symbol(Symbol::DoDot, pos),
                        }
                    }
                    _ => self.push_symbol(Symbol::Dot, pos),
                },
                '=' => match self.chars.peek() {
                    Some('=') => {
                        self.advance();
                        self.push_symbol(Symbol::Equal, pos)
                    }
                    _ => self.push_symbol(Symbol::Assign, pos),
                },
                '!' => match self.chars.peek() {
                    Some('=') => {
                        self.advance();
                        self.push_symbol(Symbol::NotEqual, pos)
                    }
                    _ => self.push_symbol(Symbol::Not, pos),
                },
                '>' => match self.chars.peek() {
                    Some('=') => {
                        self.advance();
                        self.push_symbol(Symbol::GreaterEqual, pos)
                    }
                    _ => self.push_symbol(Symbol::Greater, pos),
                },
                '<' => match self.chars.peek() {
                    Some('=') => {
                        self.advance();
                        self.push_symbol(Symbol::LowerEqual, pos)
                    }
                    _ => self.push_symbol(Symbol::Lower, pos),
                },
                '+' => {
                    self.push_symbol(Symbol::Plus, pos);
                }
                '*' => {
                    self.push_symbol(Symbol::Star, pos);
                }
                '$' => {
                    self.push_symbol(Symbol::Dollar, pos);
                }
                ',' => {
                    self.push_symbol(Symbol::Comma, pos);
                }
                ':' => {
                    self.push_symbol(Symbol::Colon, pos);
                }
                '%' => {
                    self.push_symbol(Symbol::Modulo, pos);
                }
                '-' => match self.chars.peek() {
                    Some('>') => {
                        self.advance();
                        self.push_symbol(Symbol::Arrow, pos);
                    }
                    _ => self.push_symbol(Symbol::Minus, pos),
                },
                '@' => {
                    self.push_symbol(Symbol::At, pos);
                }
                '|' => match self.chars.peek() {
                    Some('|') => {
                        self.advance();
                        self.push_symbol(Symbol::Or, pos)
                    }
                    _ => self.push_symbol(Symbol::Pipe, pos),
                },
                '&' => match self.chars.peek() {
                    Some('&') => {
                        self.advance();
                        self.push_symbol(Symbol::And, pos)
                    }
                    _ => self.push_symbol(Symbol::Ampersand, pos),
                },
                '^' => self.push_symbol(Symbol::Xor, pos),
                '/' => match self.chars.peek() {
                    Some('/') => {
                        self.advance();
                        let comm = self.read_eol();
                        self.push_token(TokenKind::Comment(comm), pos);
                    }
                    _ => self.push_symbol(Symbol::Slash, pos),
                },
                '\'' => {
                    let c = self.escape_char();
                    self.advance();
                    match self.chars.peek() {
                        Some('\'') => {
                            self.advance();
                            self.push_token(TokenKind::CharLiteral(c), pos);
                        }
                        _ => panic!("Expected ' at {:?}", pos),
                    }
                }
                '"' => {
                    let mut buf = String::default();
                    loop {
                        match self.chars.peek() {
                            Some('"') => {
                                self.advance();
                                break;
                            }
                            Some(c) => match c {
                                '\\' => {
                                    self.advance();
                                    buf.push(self.escape_char());
                                }
                                _ => {
                                    buf.push(*c);
                                    self.advance();
                                }
                            },
                            None => panic!("Unexpected EOF at {:?}", pos),
                        }
                    }
                    self.push_token(TokenKind::StringLiteral(buf), pos);
                }
                'a'..='z' | 'A'..='Z' | '_' => {
                    let mut buf = c.to_string();
                    while let Some(c) = self.chars.peek() {
                        match c {
                            'a'..='z' | 'A'..='Z' | '_' | '-' | '0'..='9' | '.' => {
                                buf.push(*c);
                                self.advance();
                            }
                            _ => break,
                        }
                    }

                    self.push_token(
                        Token::keyword(buf.as_ref()).unwrap_or(
                            Token::boolean(buf.as_ref()).unwrap_or(
                                Token::r#type(buf.as_ref()).unwrap_or(TokenKind::Identifier(buf)),
                            ),
                        ),
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
                        TokenKind::IntLiteral(
                            u64::from_str_radix(&buf, 16).expect("Lexer died @hex -> u64"),
                        )
                    } else {
                        buf.push('0');
                        while let Some(ch) = self.chars.peek() {
                            match ch {
                                '_' => self.advance(),
                                _ if ch.is_digit(10) | (*ch == '.') => {
                                    buf.push(self.read().expect("Lexer died @digits"))
                                }
                                _ => break,
                            };
                        }
                        if buf.contains(".") {
                            TokenKind::FloatLiteral(f64::from_str(&buf).expect(
                                format!("{} is not a floating point literal", buf).as_ref(),
                            ))
                        } else {
                            TokenKind::IntLiteral(
                                u64::from_str_radix(&buf, 10).expect("Lexer died @digits -> u64"),
                            )
                        }
                    };
                    self.push_token(num, pos);
                }
                c if c.is_digit(10) => {
                    let mut buf = c.to_string();
                    while let Some(ch) = self.chars.peek() {
                        match ch {
                            '_' => self.advance(),
                            _ if ch.is_digit(10) | (*ch == '.') => {
                                buf.push(self.read().expect("Lexer died @digits"))
                            }
                            _ => break,
                        };
                    }

                    self.push_token(
                        if buf.contains(".") {
                            TokenKind::FloatLiteral(f64::from_str(&buf).expect(
                                format!("{} is not a floating point literal", buf).as_ref(),
                            ))
                        } else {
                            TokenKind::IntLiteral(
                                u64::from_str_radix(&buf, 10).expect("Lexer died @digits -> u64"),
                            )
                        },
                        pos,
                    );
                }
                t => panic!("Unhandled token {} pos {:?}", t, pos),
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
