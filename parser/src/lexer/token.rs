use std::str::FromStr;

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub position: Position,
}

pub type Position = (usize, usize);

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    EOF,
    Symbol(Symbol),
    Comment(String),
    IntLiteral(u64),
    FloatLiteral(f64),
    BooleanLiteral(Boolean),
    Identifier(String),
    Keyword(Keyword),
    Type(Type),
    StringLiteral(String),
    CharLiteral(char),
    EOL,
    SEP,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]
pub enum Symbol {
    LParen,
    RParen,
    Arrow,
    LCurly,
    RCurly,
    LBracket,
    RBracket,
    At,
    Unit,
    Assign,
    Plus,
    Minus,
    Star,
    Slash,
    Ampersand,
    Dollar,
    Comma,
    Colon,
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Lower,
    LowerEqual,
    Not,
    And,
    Pipe,
    Or,
    Xor,
    Modulo,
    Dot,
    DoDot,
    GoDot,
}

impl From<Symbol> for String {
    fn from(sym: Symbol) -> String {
        match sym {
            Symbol::Plus => String::from("+"),
            Symbol::Minus => String::from("-"),
            Symbol::Star => String::from("*"),
            Symbol::Slash => String::from("/"),
            Symbol::Not => String::from("!"),
            Symbol::Equal => String::from("=="),
            Symbol::NotEqual => String::from("!="),
            Symbol::Greater => String::from(">"),
            Symbol::GreaterEqual => String::from(">="),
            Symbol::Lower => String::from("<"),
            Symbol::LowerEqual => String::from("<="),
            Symbol::Pipe => String::from("|"),
            Symbol::Or => String::from("||"),
            Symbol::Ampersand => String::from("&"),
            Symbol::And => String::from("&&"),
            Symbol::Xor => String::from("^"),
            Symbol::Modulo => String::from("%"),
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
    Var,
    Val,
    If,
    Else,
    Use,
    Bool,
    Record,
    As,
    Then,
    Const,
    Global,
    Ret,
    While,
    Until,
    Type,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum Type {
    Char,
    Str,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
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
    };
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
                Symbol::Plus
                | Symbol::Minus
                | Symbol::Star
                | Symbol::Slash
                | Symbol::Equal
                | Symbol::NotEqual
                | Symbol::Greater
                | Symbol::GreaterEqual
                | Symbol::Lower
                | Symbol::LowerEqual
                | Symbol::Not
                | Symbol::Ampersand
                | Symbol::And
                | Symbol::Pipe
                | Symbol::Or
                | Symbol::Xor
                | Symbol::Modulo => Some(sym),
                _ => None,
            };
        }
        None
    }
    pub fn split_accessor(&self) -> Option<Vec<String>> {
        match &self.kind {
            TokenKind::Identifier(accessor) => {
                match accessor.clone().split(".").collect::<Vec<_>>() {
                    acc => Some(acc.iter().map(|val| String::from(*val)).collect()),
                }
            }
            _ => None,
        }
    }
    from_tok!(kw, Keyword, Keyword);
    from_tok!(bl, BooleanLiteral, Boolean);
    from_tok!(sym, Symbol, Symbol);
    from_tok!(int, IntLiteral, u64);
    from_tok!(float, FloatLiteral, f64);
    from_tok!(ident, Identifier, String);
    from_tok!(ty, Type, Type);
    from_tok!(st, StringLiteral, String);
    from_tok!(comm, Comment, String);
    from_tok!(ch, CharLiteral, char);
}

impl FromStr for Type {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "char" => Ok(Self::Char),
            "str" => Ok(Self::Str),
            "i8" => Ok(Self::I8),
            "i16" => Ok(Self::I16),
            "i32" => Ok(Self::I32),
            "i64" => Ok(Self::I64),
            "u8" => Ok(Self::U8),
            "u16" => Ok(Self::U16),
            "u32" => Ok(Self::U32),
            "u64" => Ok(Self::U64),
            "f32" => Ok(Self::F32),
            "f64" => Ok(Self::F64),
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
            "var" => Ok(Self::Var),
            "val" => Ok(Self::Val),
            "if" => Ok(Self::If),
            "else" => Ok(Self::Else),
            "use" => Ok(Self::Use),
            "bool" => Ok(Self::Bool),
            "rec" => Ok(Self::Record),
            "as" => Ok(Self::As),
            "then" => Ok(Self::Then),
            "const" => Ok(Self::Const),
            "global" => Ok(Self::Global),
            "ret" => Ok(Self::Ret),
            "while" => Ok(Self::While),
            "until" => Ok(Self::Until),
            "type" => Ok(Self::Type),
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
