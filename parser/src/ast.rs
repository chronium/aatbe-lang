#[derive(Debug, Eq, PartialEq, Clone)]
pub enum AST {
    File(Vec<AST>),
    Type(PrimitiveType),
    Error,
    Expr(Expression),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expression {
    Atom(AtomKind),
    Binary(Box<Expression>, String, Box<Expression>),
    Block(Vec<Expression>),
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
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum AtomKind {
    Bool(Boolean),
    Integer(u64),
    StringLiteral(String),
    Expr(Box<Expression>),
    Unit,
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
