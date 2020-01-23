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
        name: String,
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
