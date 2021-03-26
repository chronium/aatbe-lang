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
    Import(String),
    Record(String, Option<Vec<String>>, Vec<PrimitiveType>),
    Typedef {
        name: String,
        type_names: Option<Vec<String>>,
        variants: Option<Vec<TypeKind>>,
    },
    Constant {
        ty: PrimitiveType,
        export: bool,
        value: Box<Expression>,
    },
    Global {
        ty: PrimitiveType,
        export: bool,
        value: Box<Expression>,
    },
    Module(String, Box<AST>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeKind {
    Newtype(PrimitiveType),
    Variant(String, Option<Vec<PrimitiveType>>),
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
        name: IdentPath,
        types: Vec<PrimitiveType>,
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
        types: Vec<PrimitiveType>,
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
    pub ret_ty: Box<PrimitiveType>,
    pub params: Vec<PrimitiveType>,
}

impl From<FunctionType> for PrimitiveType {
    fn from(func: FunctionType) -> Self {
        PrimitiveType::Function(func)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
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
    GenericTypeRef(String, Vec<PrimitiveType>),
    Newtype(String),
    VariantType(String),
    Variant {
        parent: String,
        variant: String,
    },
    Function(FunctionType),
    NamedType {
        name: String,
        ty: Option<Box<PrimitiveType>>,
    },
    Ref(Box<PrimitiveType>),
    Pointer(Box<PrimitiveType>),
    Array {
        ty: Box<PrimitiveType>,
        len: u32,
    },
    Slice {
        ty: Box<PrimitiveType>,
    },
    Symbol(String),
    Box(Box<PrimitiveType>),
    Path(ModPath),
}

impl PrimitiveType {
    pub fn inner(&self) -> &PrimitiveType {
        match self {
            PrimitiveType::NamedType {
                name: _,
                ty: Some(box ty),
            } => ty,
            PrimitiveType::Function(..) => panic!("ICE primty inner {:?}", self),
            other => other,
        }
    }

    pub fn ext(&self) -> bool {
        match self {
            PrimitiveType::Function(FunctionType {
                ext,
                ret_ty: _,
                params: _,
            }) => ext.clone(),
            _ => panic!("ICE PrimitiveType ext {:?}", self),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum AtomKind {
    Unit,
    SymbolLiteral(String),
    Bool(Boolean),
    Integer(u64, PrimitiveType),
    Floating(f64, PrimitiveType),
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
    Cast(Box<AtomKind>, PrimitiveType),
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
