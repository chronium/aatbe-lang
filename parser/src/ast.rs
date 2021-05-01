use std::fmt;

type ModPath = Vec<String>;

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum IdentPath {
    Local(String),
    Module(ModPath),
    Root(ModPath),
}

#[derive(Debug, PartialEq, Clone)]
pub enum AST {
    File(Vec<AST>),
    Error,
    Expr(Expression),
    Import(IdentPath),
    Record(String, Option<Vec<String>>, Vec<Type>),
    Typedef {
        name: String,
        type_names: Option<Vec<String>>,
        variants: Option<Vec<TypeKind>>,
    },
    Constant {
        ty: Type,
        export: bool,
        value: Box<Expression>,
    },
    Global {
        ty: Type,
        export: bool,
        value: Box<Expression>,
    },
    Module(String, Box<AST>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeKind {
    Newtype(Type),
    Variant(String, Option<Vec<Type>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Atom(AtomKind),
    Binary(Box<Expression>, String, Box<Expression>),
    Block(Vec<Expression>),
    Ret(Box<Expression>),
    Decl {
        ty: Type,
        value: Option<Box<Expression>>,
        exterior_bind: BindType,
    },
    Assign {
        lval: LValue,
        value: Box<Expression>,
    },
    Call {
        name: IdentPath,
        types: Vec<Type>,
        args: Vec<Expression>,
    },
    Function {
        name: String,
        ty: FunctionType,
        body: Option<Box<Expression>>,
        attributes: Vec<String>,
        type_names: Vec<String>,
        public: bool,
    },
    If {
        cond_expr: Box<Expression>,
        then_expr: Box<Expression>,
        elseif_exprs: Vec<(Expression, Expression)>,
        else_expr: Option<Box<Expression>>,
    },
    RecordInit {
        record: String,
        types: Vec<Type>,
        values: Vec<AtomKind>,
    },
    Loop {
        loop_type: LoopType,
        cond_expr: Box<Expression>,
        body: Box<Expression>,
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

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum LoopType {
    While,
    Until,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct FunctionType {
    pub ext: bool,
    pub ret_ty: Box<Type>,
    pub params: Vec<Type>,
}

impl From<FunctionType> for Type {
    fn from(func: FunctionType) -> Self {
        Type::Function(func)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Type {
    Unit,
    Str,
    Varargs,
    Bool,
    Char,
    Int(IntSize),
    UInt(IntSize),
    Float(FloatSize),
    TypeRef(String),
    GenericTypeRef(String, Vec<Type>),
    Newtype(String),
    VariantType(String),
    Variant { parent: String, variant: String },
    Function(FunctionType),
    NamedType { name: String, ty: Option<Box<Type>> },
    Ref(Box<Type>),
    Pointer(Box<Type>),
    Array { ty: Box<Type>, len: u32 },
    Slice { ty: Box<Type> },
    Symbol(String),
    Box(Box<Type>),
    Path(ModPath),
}

impl Type {
    pub fn inner(&self) -> &Type {
        match self {
            Type::NamedType {
                name: _,
                ty: Some(box ty),
            } => ty,
            Type::Function(..) => panic!("ICE primty inner {:?}", self),
            other => other,
        }
    }

    pub fn ext(&self) -> bool {
        match self {
            Type::Function(FunctionType {
                ext,
                ret_ty: _,
                params: _,
            }) => ext.clone(),
            _ => panic!("ICE Type ext {:?}", self),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum AtomKind {
    Unit,
    SymbolLiteral(String),
    Bool(Boolean),
    Integer(u64, Type),
    Floating(f64, Type),
    StringLiteral(String),
    CharLiteral(char),
    Expr(Box<Expression>),
    Parenthesized(Box<Expression>),
    Unary(String, Box<AtomKind>),
    Ident(String),
    Access(Vec<String>),
    Deref(Box<AtomKind>),
    Ref(Box<AtomKind>),
    Index(Box<AtomKind>, Box<Expression>),
    Cast(Box<AtomKind>, Type),
    Array(Vec<Expression>),
    Is(Box<AtomKind>, String),
    NamedValue { name: String, val: Box<Expression> },
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Boolean {
    True,
    False,
}

#[derive(Debug, PartialEq, Clone, PartialOrd, Eq, Hash)]
pub enum IntSize {
    Bits8,
    Bits16,
    Bits32,
    Bits64,
}

impl From<usize> for IntSize {
    fn from(size: usize) -> IntSize {
        match size {
            1 => IntSize::Bits8,
            2 => IntSize::Bits16,
            4 => IntSize::Bits32,
            8 => IntSize::Bits64,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
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
