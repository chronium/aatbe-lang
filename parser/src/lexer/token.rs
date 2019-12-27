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
    BooleanLiteral(Boolean),
    Identifier(String),
    Keyword(Keyword),
    Type(Type),
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

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum Type {
    Str,
}

macro_rules! to_tok {
    ($name:ident, $kind:ident, $ty:ident) => {
        pub fn $name(val: &str) -> Option<TokenKind> {
            match $ty::from_str(val) {
                Ok(k) => Some(TokenKind::$kind(k)),
                Err(_) => None,
            }
        }
    };
}

macro_rules! from_tok {
    ($name:ident, $kind:ident, $res:ident) => {
        pub fn $name(&self) -> Option<$res> {
            match &self.kind {
                TokenKind::$kind(res) => Some(res.clone()),
                _ => None,
            }
        }
    }
}

impl Token {
    pub fn new(kind: TokenKind, position: Position) -> Self {
        Self { kind, position }
    }

    to_tok!(keyword, Keyword, Keyword);
    to_tok!(boolean, BooleanLiteral, Boolean);
    to_tok!(r#type, Type, Type);

    pub fn op(&self) -> Option<Symbol> {
        if let TokenKind::Symbol(sym) = self.kind {
            return match sym {
                Symbol::Plus | Symbol::Minus | Symbol::Star | Symbol::Slash => Some(sym),
                _ => None,
            };
        }

        None
    }

    from_tok!(kw, Keyword, Keyword);
    from_tok!(bl, BooleanLiteral, Boolean);
    from_tok!(sym, Symbol, Symbol);
    from_tok!(int, IntLiteral, u64);
    from_tok!(ident, Identifier, String);
    from_tok!(ty, Type, Type);
}

impl FromStr for Type {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "str" => Ok(Self::Str),
            _ => Err(()),
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
