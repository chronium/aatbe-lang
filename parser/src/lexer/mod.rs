use std::{iter::Peekable, str::Chars, str::FromStr};

pub mod token;

use token::{Position, Symbol, Token, TokenKind};

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
            Some('0') => {
                self.advance();
                '\0'
            }
            Some(c) => {
                let c = c.clone();
                self.advance();
                c
            }
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
                        self.push_symbol(Symbol::LogicalOr, pos)
                    }
                    _ => self.push_symbol(Symbol::Or, pos),
                },
                '&' => match self.chars.peek() {
                    Some('&') => {
                        self.advance();
                        self.push_symbol(Symbol::LogicalAnd, pos)
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
                    let c = match self.chars.peek() {
                        Some('\\') => {
                            self.advance();
                            self.escape_char()
                        }
                        Some(c) => {
                            let c = c.clone();
                            self.advance();
                            c
                        }
                        _ => panic!(),
                    };
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

#[cfg(test)]
mod lexer_tests {
    use super::{
        token::{Boolean, Keyword, Type},
        Lexer, Symbol, TokenKind,
    };
    #[test]
    fn end_of_file() {
        let mut lexer = Lexer::new("");
        lexer.lex();
        let mut tokens = lexer.into_iter();

        assert_eq!(tokens.next().unwrap().kind, TokenKind::EOF);
    }

    macro_rules! sep {
        ($tokens: ident) => {
            assert_eq!($tokens.next().unwrap().kind, TokenKind::SEP);
        };
    }

    #[test]
    fn symbols() {
        let mut lexer = Lexer::new(
            " @ ( ) -> { } () = + - * / & $ , : ! == != > >= < <= | || && ^ % . .. ... [ ]",
        );
        lexer.lex();
        let mut tokens = lexer.into_iter();

        let expected = vec![
            Symbol::At,
            Symbol::LParen,
            Symbol::RParen,
            Symbol::Arrow,
            Symbol::LCurly,
            Symbol::RCurly,
            Symbol::Unit,
            Symbol::Assign,
            Symbol::Plus,
            Symbol::Minus,
            Symbol::Star,
            Symbol::Slash,
            Symbol::Ampersand,
            Symbol::Dollar,
            Symbol::Comma,
            Symbol::Colon,
            Symbol::Not,
            Symbol::Equal,
            Symbol::NotEqual,
            Symbol::Greater,
            Symbol::GreaterEqual,
            Symbol::Lower,
            Symbol::LowerEqual,
            Symbol::Or,
            Symbol::LogicalOr,
            Symbol::LogicalAnd,
            Symbol::Xor,
            Symbol::Modulo,
            Symbol::Dot,
            Symbol::DoDot,
            Symbol::GoDot,
            Symbol::LBracket,
            Symbol::RBracket,
        ]
        .into_iter()
        .map(|t| Some(t));

        for t in expected {
            sep!(tokens);
            assert_eq!(tokens.next().unwrap().sym(), t);
        }
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

        sep!(tokens);
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
            TokenKind::IntLiteral(18_446_744_073_709_551_614u64),
        )
    }

    #[test]
    fn single_number_literal_zero() {
        let mut lexer = Lexer::new("0123456789");
        lexer.lex();
        let mut tokens = lexer.into_iter();

        assert_eq!(
            tokens.next().unwrap().kind,
            TokenKind::IntLiteral(0123456789),
        )
    }

    #[test]
    fn single_number_literal_hex() {
        let mut lexer = Lexer::new("0xdeadbeef");
        lexer.lex();
        let mut tokens = lexer.into_iter();

        assert_eq!(
            tokens.next().unwrap().kind,
            TokenKind::IntLiteral(0xdeadbeef),
        )
    }

    #[test]
    fn single_number_literal_hex_separator() {
        let mut lexer = Lexer::new("0xdead_beef");
        lexer.lex();
        let mut tokens = lexer.into_iter();

        assert_eq!(
            tokens.next().unwrap().kind,
            TokenKind::IntLiteral(0xdeadbeef),
        )
    }

    #[test]
    fn string_literal_no_escape() {
        let mut lexer = Lexer::new("\"Hello World\n\"");
        lexer.lex();
        let mut tokens = lexer.into_iter();

        assert_eq!(
            tokens.next().unwrap().kind,
            TokenKind::StringLiteral(String::from("Hello World\n")),
        )
    }

    #[test]
    fn keyword_identifier() {
        let mut lexer = Lexer::new(
            "fn extern var val if else use true false main record.test bool rec global ret while until",
        );
        lexer.lex();
        let mut tokens = lexer.into_iter();

        assert_eq!(tokens.next().unwrap().kw(), Some(Keyword::Fn));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().kw(), Some(Keyword::Extern));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().kw(), Some(Keyword::Var));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().kw(), Some(Keyword::Val));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().kw(), Some(Keyword::If));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().kw(), Some(Keyword::Else));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().kw(), Some(Keyword::Use));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().bl(), Some(Boolean::True));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().bl(), Some(Boolean::False));
        sep!(tokens);
        assert_eq!(
            tokens.next().unwrap().kind,
            TokenKind::Identifier(String::from("main")),
        );
        sep!(tokens);
        assert_eq!(
            tokens.next().unwrap().kind,
            TokenKind::Identifier(String::from("record.test")),
        );
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().kw(), Some(Keyword::Bool));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().kw(), Some(Keyword::Record));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().kw(), Some(Keyword::Global));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().kw(), Some(Keyword::Ret));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().kw(), Some(Keyword::While));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().kw(), Some(Keyword::Until));
    }

    #[test]
    fn type_tests() {
        let mut lexer = Lexer::new("str i8 i16 i32 i64 u8 u16 u32 u64 f32 f64");
        lexer.lex();
        let mut tokens = lexer.into_iter();

        assert_eq!(tokens.next().unwrap().ty(), Some(Type::Str));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().ty(), Some(Type::I8));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().ty(), Some(Type::I16));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().ty(), Some(Type::I32));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().ty(), Some(Type::I64));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().ty(), Some(Type::U8));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().ty(), Some(Type::U16));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().ty(), Some(Type::U32));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().ty(), Some(Type::U64));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().ty(), Some(Type::F32));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().ty(), Some(Type::F64));
    }
}
