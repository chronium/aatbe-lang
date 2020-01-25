use std::fmt;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum AST {
    File(Vec<AST>),
    Error,
    Expr(Expression),
    Import(String),
    Record(String, Vec<PrimitiveType>),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expression {
    Atom(AtomKind),
    Binary(Box<Expression>, String, Box<Expression>),
    Block(Vec<Expression>),
    Decl {
        ty: PrimitiveType,
        value: Option<Box<Expression>>,
        exterior_bind: BindType,
    },
    Assign {
        lval: LValue,
        value: Box<Expression>,
    },
    Call {
        name: String,
        args: Vec<AtomKind>,
    },
    Function {
        name: String,
        ty: PrimitiveType,
        body: Option<Box<Expression>>,
        attributes: Vec<String>,
    },
    If {
        cond_expr: Box<Expression>,
        then_expr: Box<Expression>,
        else_expr: Option<Box<Expression>>,
    },
}

#[derive(Eq, PartialEq, Clone)]
pub enum LValue {
    Ident(String),
    Accessor(Vec<String>),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum BindType {
    Mutable,
    Immutable,
    Constant,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum PrimitiveType {
    Unit,
    Str,
    Varargs,
    Bool,
    Int(IntSize),
    UInt(IntSize),
    TypeRef(String),
    Function {
        ext: bool,
        ret_ty: Box<PrimitiveType>,
        params: Vec<PrimitiveType>,
    },
    NamedType {
        name: String,
        ty: Box<PrimitiveType>,
    },
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum AtomKind {
    SymbolLiteral(String),
    Bool(Boolean),
    Integer(u64, PrimitiveType),
    StringLiteral(String),
    Expr(Box<Expression>),
    Unit,
    Parenthesized(Box<Expression>),
    Unary(String, Box<AtomKind>),
    Ident(String),
    Access(Vec<String>),
    NamedValue {
        name: String,
        val: Box<Expression>,
    },
    RecordInit {
        record: String,
        values: Vec<AtomKind>,
    },
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Boolean {
    True,
    False,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum IntSize {
    Bits8,
    Bits16,
    Bits32,
    Bits64,
}

impl fmt::Debug for LValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LValue::Ident(name) => write!(f, "{}", name),
            LValue::Accessor(parts) => write!(f, "{}", parts.join(".")),
        }
    }
}

impl From<&LValue> for String {
    fn from(lval: &LValue) -> String {
        match lval {
            LValue::Ident(name) => name.clone(),
            LValue::Accessor(parts) => parts.join("."),
        }
    }
}
