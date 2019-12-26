use std::str::FromStr;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub position: Position,
}

pub type Position = (usize, usize);

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum TokenKind {
    EOF,
    Symbol(Symbol),
    Comment(String),
    IntLiteral(u64),
    BooleanLitral(Boolean),
    Identifier(String),
    Keyword(Keyword),
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]
pub enum Symbol {
    LParen,
    RParen,
    Arrow,
    LCurly,
    RCurly,
    At,
    Unit,
    Assign,
    Plus,
    Minus,
    Star,
    Slash,
    Ampersand,
}

impl From<Symbol> for String {
    fn from(sym: Symbol) -> String {
        match sym {
            Symbol::Plus => String::from("+"),
            Symbol::Minus => String::from("-"),
            Symbol::Star => String::from("*"),
            Symbol::Slash => String::from("/"),
            _ => panic!("Symbol to str {:?}", sym),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum Boolean {
    True,
    False,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum Keyword {
    Fn,
    Extern,
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

    pub fn boolean(boolean: &str) -> Option<TokenKind> {
        match Boolean::from_str(boolean) {
            Ok(b) => Some(TokenKind::BooleanLitral(b)),
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

    pub fn op(&self) -> Option<Symbol> {
        if let TokenKind::Symbol(sym) = self.kind {
            return match sym {
                Symbol::Plus | Symbol::Minus | Symbol::Star | Symbol::Slash => Some(sym),
                _ => None,
            };
        }

        None
    }

    pub fn bl(&self) -> Option<Boolean> {
        match self.kind {
            TokenKind::BooleanLitral(b) => Some(b),
            _ => None,
        }
    }

    pub fn int(&self) -> Option<u64> {
        match self.kind {
            TokenKind::IntLiteral(int) => Some(int),
            _ => None,
        }
    }

    pub fn ident(&self) -> Option<String> {
        match &self.kind {
            TokenKind::Identifier(id) => Some(id.clone()),
            _ => None,
        }
    }
}

impl FromStr for Keyword {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "fn" => Ok(Self::Fn),
            "extern" => Ok(Self::Extern),
            _ => Err(()),
        }
    }
}

impl FromStr for Boolean {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "true" => Ok(Self::True),
            "false" => Ok(Self::False),
            _ => Err(()),
        }
    }
}
