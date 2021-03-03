use std::{iter::Peekable, str::Chars, str::FromStr};

pub mod token;

use token::{Symbol, Token, TokenKind};

mod tests;

macro_rules! token {
    ($tokens:ident, $token:expr, $pos:expr) => {
        $tokens.push(Token::new($token, $pos));
    };
}

macro_rules! symbol {
    ($tokens:ident, $sym:expr, $pos:expr) => {
        $tokens.push(Token::new(TokenKind::Symbol($sym), $pos));
    };
}

pub struct Lexer<'c> {
    chars: Peekable<Chars<'c>>,
    col: usize,
    row: usize,
}

impl<'c> Lexer<'c> {
    pub fn new(code: &'c str) -> Self {
        Self {
            chars: code.chars().peekable(),
            col: 1,
            row: 1,
        }
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

    fn eat_whitespace(&mut self, tokens: &mut Vec<Token>) {
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
            token!(tokens, TokenKind::SEP, (self.col, self.row));
        }
    }

    pub fn escape_char(&mut self) -> char {
        match self.chars.clone().peek() {
            Some('\\') => {
                self.advance();
                match self.chars.clone().peek() {
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
                    Some('0') => {
                        self.advance();
                        '0'
                    }
                    Some(c) => {
                        self.advance();
                        *c
                    }
                    None => '\\',
                }
            }
            Some(c) => {
                self.advance();
                *c
            }
            None => panic!("Unexpected EOF"),
        }
    }

    pub fn lex(&mut self) -> Vec<Token> {
        let mut tokens = vec![];
        loop {
            match self.chars.peek() {
                Some('\n') => {
                    self.advance();
                    let pos = (self.col, self.row);
                    self.col = 1;
                    self.row += 1;
                    token!(tokens, TokenKind::EOL, pos);
                }
                Some('\r') => {
                    if let Some('\n') = self.chars.peek() {
                        self.advance();
                        let pos = (self.col, self.row);
                        self.col = 1;
                        self.row += 1;
                        token!(tokens, TokenKind::EOL, pos);
                    } else {
                        let pos = (self.col, self.row);
                        panic!("Expected \\n after \\r at {:?}", pos);
                    }
                }
                _ => {}
            }
            self.eat_whitespace(&mut tokens);
            let pos = (self.col, self.row);

            let c = match self.read() {
                Some(c) => c,
                None => {
                    token!(tokens, TokenKind::EOF, pos);
                    break;
                }
            };

            match c {
                '(' => match self.chars.peek() {
                    Some(')') => {
                        self.advance();
                        symbol!(tokens, Symbol::Unit, pos)
                    }
                    _ => symbol!(tokens, Symbol::LParen, pos),
                },
                ')' => {
                    symbol!(tokens, Symbol::RParen, pos);
                }
                '{' => {
                    symbol!(tokens, Symbol::LCurly, pos);
                }
                '}' => {
                    symbol!(tokens, Symbol::RCurly, pos);
                }
                '[' => {
                    symbol!(tokens, Symbol::LBracket, pos);
                }
                ']' => {
                    symbol!(tokens, Symbol::RBracket, pos);
                }
                '.' => match self.chars.peek() {
                    Some('.') => {
                        self.advance();
                        match self.chars.peek() {
                            Some('.') => {
                                self.advance();
                                symbol!(tokens, Symbol::GoDot, pos)
                            }
                            _ => symbol!(tokens, Symbol::DoDot, pos),
                        }
                    }
                    _ => symbol!(tokens, Symbol::Dot, pos),
                },
                '=' => match self.chars.peek() {
                    Some('=') => {
                        self.advance();
                        symbol!(tokens, Symbol::Equal, pos)
                    }
                    _ => symbol!(tokens, Symbol::Assign, pos),
                },
                '!' => match self.chars.peek() {
                    Some('=') => {
                        self.advance();
                        symbol!(tokens, Symbol::NotEqual, pos)
                    }
                    _ => symbol!(tokens, Symbol::Not, pos),
                },
                '>' => match self.chars.peek() {
                    Some('=') => {
                        self.advance();
                        symbol!(tokens, Symbol::GreaterEqual, pos)
                    }
                    _ => symbol!(tokens, Symbol::Greater, pos),
                },
                '<' => match self.chars.peek() {
                    Some('=') => {
                        self.advance();
                        symbol!(tokens, Symbol::LowerEqual, pos)
                    }
                    _ => symbol!(tokens, Symbol::Lower, pos),
                },
                '+' => {
                    symbol!(tokens, Symbol::Plus, pos);
                }
                '*' => {
                    symbol!(tokens, Symbol::Star, pos);
                }
                '$' => {
                    symbol!(tokens, Symbol::Dollar, pos);
                }
                ',' => {
                    symbol!(tokens, Symbol::Comma, pos);
                }
                ':' => match self.chars.peek() {
                    Some(':') => {
                        self.advance();
                        symbol!(tokens, Symbol::Doubly, pos)
                    }
                    _ => symbol!(tokens, Symbol::Colon, pos),
                },
                '%' => {
                    symbol!(tokens, Symbol::Modulo, pos);
                }
                '-' => match self.chars.peek() {
                    Some('>') => {
                        self.advance();
                        symbol!(tokens, Symbol::Arrow, pos);
                    }
                    _ => symbol!(tokens, Symbol::Minus, pos),
                },
                '@' => {
                    symbol!(tokens, Symbol::At, pos);
                }
                '|' => match self.chars.peek() {
                    Some('|') => {
                        self.advance();
                        symbol!(tokens, Symbol::Or, pos)
                    }
                    _ => symbol!(tokens, Symbol::Pipe, pos),
                },
                '&' => match self.chars.peek() {
                    Some('&') => {
                        self.advance();
                        symbol!(tokens, Symbol::And, pos)
                    }
                    _ => symbol!(tokens, Symbol::Ampersand, pos),
                },
                '^' => symbol!(tokens, Symbol::Xor, pos),
                '/' => match self.chars.peek() {
                    Some('/') => {
                        self.advance();
                        let comm = self.read_eol();
                        token!(tokens, TokenKind::Comment(comm), pos);
                    }
                    _ => symbol!(tokens, Symbol::Slash, pos),
                },
                '\'' => {
                    let c = self.escape_char();
                    match self.chars.peek() {
                        Some('\'') => {
                            self.advance();
                            token!(tokens, TokenKind::CharLiteral(c), pos);
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
                            Some(_) => buf.push(self.escape_char()),
                            None => panic!("Unexpected EOF at {:?}", pos),
                        }
                    }
                    token!(tokens, TokenKind::StringLiteral(buf), pos);
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

                    token!(
                        tokens,
                        Token::keyword(buf.as_ref()).unwrap_or(
                            Token::boolean(buf.as_ref()).unwrap_or(
                                Token::r#type(buf.as_ref()).unwrap_or(TokenKind::Identifier(buf)),
                            ),
                        ),
                        pos
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
                    token!(tokens, num, pos);
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

                    token!(
                        tokens,
                        if buf.contains(".") {
                            TokenKind::FloatLiteral(f64::from_str(&buf).expect(
                                format!("{} is not a floating point literal", buf).as_ref(),
                            ))
                        } else {
                            TokenKind::IntLiteral(
                                u64::from_str_radix(&buf, 10).expect("Lexer died @digits -> u64"),
                            )
                        },
                        pos
                    );
                }
                t => panic!("Unhandled token {} pos {:?}", t, pos),
            };
        }

        tokens
    }
}
