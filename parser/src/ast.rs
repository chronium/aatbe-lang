#[derive(Debug, Eq, PartialEq, Clone)]
pub enum AST {
    File(Vec<AST>),
    Error,
    Expr(Expression),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expression {
    Atom(AtomKind),
    Binary(Box<Expression>, String, Box<Expression>),
    Block(Vec<Expression>),
    Decl {
        ty: PrimitiveType,
        value: Option<Box<Expression>>,
        ext_mut: bool,
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
pub enum PrimitiveType {
    Unit,
    Str,
    Int(IntType),
    UInt(UIntType),
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
    Integer(u64),
    StringLiteral(String),
    Expr(Box<Expression>),
    Unit,
    Parenthesized(Box<Expression>),
    Unary(String, Box<AtomKind>),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Boolean {
    True,
    False,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum IntType {
    I8,
    I16,
    I32,
    I64,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum UIntType {
    U8,
    U16,
    U32,
    U64,
}
