use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum AST {
    File(Vec<AST>),
    Error,
    Expr(Expression),
    Import(String),
    Record(String, Vec<PrimitiveType>),
    Constant {
        ty: PrimitiveType,
        value: Box<Expression>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Atom(AtomKind),
    Binary(Box<Expression>, String, Box<Expression>),
    Block(Vec<Expression>),
    Ret(Box<Expression>),
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
        args: Vec<Expression>,
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
    RecordInit {
        record: String,
        values: Vec<AtomKind>,
    },
}

#[derive(PartialEq, Clone)]
pub enum LValue {
    Ident(String),
    Accessor(Vec<String>),
    Deref(Box<LValue>),
    Index(Box<LValue>, Box<Expression>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum BindType {
    Mutable,
    Immutable,
    Constant,
}

#[derive(Debug, PartialEq, Clone)]
pub enum PrimitiveType {
    Unit,
    Str,
    Varargs,
    Bool,
    Char,
    Int(IntSize),
    UInt(IntSize),
    Float(FloatSize),
    TypeRef(String),
    Function {
        ext: bool,
        ret_ty: Box<PrimitiveType>,
        params: Vec<PrimitiveType>,
    },
    NamedType {
        name: String,
        ty: Option<Box<PrimitiveType>>,
    },
    Ref(Box<PrimitiveType>),
    Pointer(Box<PrimitiveType>),
}

impl PrimitiveType {
    pub fn inner(&self) -> &PrimitiveType {
        match self {
            PrimitiveType::NamedType {
                name: _,
                ty: Some(box ty),
            } => ty,
            PrimitiveType::Function {
                ext: _,
                ret_ty: _,
                params: _,
            } => panic!("ICE primty inner {:?}", self),
            other => other,
        }
    }

    pub fn ext(&self) -> bool {
        match self {
            PrimitiveType::Function {
                ext,
                ret_ty: _,
                params: _,
            } => ext.clone(),
            _ => panic!("ICE PrimitiveType ext {:?}", self),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum AtomKind {
    SymbolLiteral(String),
    Bool(Boolean),
    Integer(u64, PrimitiveType),
    Floating(f64, PrimitiveType),
    StringLiteral(String),
    CharLiteral(char),
    Expr(Box<Expression>),
    Unit,
    Parenthesized(Box<Expression>),
    Unary(String, Box<AtomKind>),
    Ident(String),
    Access(Vec<String>),
    Deref(Box<AtomKind>),
    Ref(Box<AtomKind>),
    Index(Box<AtomKind>, Box<Expression>),
    Cast(Box<AtomKind>, PrimitiveType),
    NamedValue { name: String, val: Box<Expression> },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Boolean {
    True,
    False,
}

#[derive(Debug, PartialEq, Clone)]
pub enum IntSize {
    Bits8,
    Bits16,
    Bits32,
    Bits64,
}

#[derive(Debug, PartialEq, Clone)]
pub enum FloatSize {
    Bits32,
    Bits64,
}

impl fmt::Debug for LValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LValue::Ident(name) => write!(f, "{}", name),
            LValue::Accessor(parts) => write!(f, "{}", parts.join(".")),
            LValue::Deref(lval) => write!(f, "*{:?}", lval),
            LValue::Index(what, _) => write!(f, "{:?}[]", what),
        }
    }
}

impl From<&LValue> for String {
    fn from(lval: &LValue) -> String {
        match lval {
            LValue::Ident(name) => name.clone(),
            LValue::Accessor(parts) => parts.join("."),
            LValue::Deref(lval) => format!("*{:?}", lval),
            LValue::Index(what, _) => format!("{:?}[]", what),
        }
    }
}
