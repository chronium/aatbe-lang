#![feature(box_syntax)]

#[derive(Debug)]
pub enum PrimitiveType {
    U8,
    U16,
    U32,
    U64,
    U128,
    USize,
    I8,
    I16,
    I32,
    I64,
    I128,
    ISize,
    Boolean,
    Char,
    Str,
    Any,
    NamedType {
        name: String,
        ty: Box<PrimitiveType>,
    },
    TupleType(Vec<PrimitiveType>),
    Pointer(Box<PrimitiveType>),
    Params(Box<Option<PrimitiveType>>),
    FunctionType {
        ret_type: Box<PrimitiveType>,
        param: Box<PrimitiveType>,
    },
}

#[derive(Debug)]
pub enum UnaryOp {
    Negative,
    Negate,
}

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Equals,
    NotEquals,
}

#[derive(Debug)]
pub enum VarType {
    Const,
    Mutable,
    Immutable,
}

#[derive(Debug)]
pub enum AST {
    True,
    False,
    Empty,
    IntLiteral(PrimitiveType, u64),
    CharLiteral(char),
    StringLiteral(String),
    Unary(UnaryOp, Box<AST>),
    Binary(BinaryOp, Box<AST>, Box<AST>),
    Parenthesized(Box<AST>),
    If {
        condition: Box<AST>,
        then_block: Box<AST>,
    },
    IfElse {
        if_expr: Box<AST>,
        else_expr: Box<AST>,
    },
    Block(Vec<AST>),
    Decl(VarType, PrimitiveType, String, Box<Option<AST>>),
    Assign(Box<AST>, Box<AST>),
    Index {
        lhs: Box<AST>,
        indexer: Box<AST>,
    },
    Ref(String),
    Tuple(Vec<AST>),
    AddrOf(Box<AST>),
    Function {
        name: String,
        ty: Box<PrimitiveType>,
    },
    Call {
        name: String,
        arg: Box<AST>,
    },
}

peg::parser! {
    grammar lang_parser() for str {
        use AST;

        rule _() = quiet!{[' ' | '\n' | '\t']*}

        rule ident_key()
            = ['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '_' | '-' | '0'..='9']*

        rule keywords()
            = "u8" / "u16" / "u32" / "u64" / "u128"
            / "i8" / "i16" / "i32" / "i64" / "i128"
            / "usize" / "isize"
            / "if" / "else"
            / "bool" / "char" / "str" / "any"
            / "true" / "false"
            / "val" / "var" / "const"
            / "fn"

        rule ident() -> String
            = id:$(quiet!{!keywords() ident_key()}) { id.to_string() }
            / expected!("identifier")

        rule int_primitive_type() -> PrimitiveType
            = "u8" { PrimitiveType::U8 }
            / "u16" { PrimitiveType::U16 }
            / "u32" { PrimitiveType::U32 }
            / "u64" { PrimitiveType::U64 }
            / "u128" { PrimitiveType::U128 }
            / "i8" { PrimitiveType::I8 }
            / "i16" { PrimitiveType::I16 }
            / "i32" { PrimitiveType::I32 }
            / "i64" { PrimitiveType::I64 }
            / "i128" { PrimitiveType::I128 }
            / "usize" { PrimitiveType::USize }
            / "isize" { PrimitiveType::ISize }
            / "any" { PrimitiveType::Any }

        rule ty() -> PrimitiveType
            = prim:int_primitive_type() { prim }
            / "bool" { PrimitiveType::Boolean }
            / "char" { PrimitiveType::Char }
            / "str" { PrimitiveType::Str }
            / i:ident() _ ":" _ t:ty() { PrimitiveType::NamedType { name: i, ty: box t } }
            / prim:parenthesized(<ty() ** (_ "," _)>) { PrimitiveType::TupleType(prim) }
            / "*" prim:ty() { PrimitiveType::Pointer(box prim) }
            / "..." _ t:ty()? { PrimitiveType::Params(box t) }
            / expected!("type")

        rule parenthesized<T>(x: rule<T>) -> T = "(" _ v:x() _ ")" { v }
        rule curly<T>(x: rule<T>) -> T = "{" _ v:x() _ "}" { v }
        rule boxed<T>(x: rule<T>) -> Box<T> =  v:x() { box v }
        rule double_quoted<T>(x: rule<T>) -> T = "\"" v:x() "\"" { v }
        rule single_quoted<T>(x: rule<T>) -> T = "'" v:x() "'" { v }
        rule bracketed<T>(x: rule<T>) -> T = "[" v:x() "]" { v }

        rule else() -> AST
            = "else" _ els:block() { els }

        rule cond() -> AST
            = "if" _ cond:expr() _ then:block() _ els:(else())? {
                let if_expr = AST::If { condition: box cond, then_block: box then };
                match els {
                    None => if_expr,
                    Some(else_blck) => AST::IfElse { if_expr: box if_expr, else_expr: box else_blck }
                }
            }
        rule assign() -> AST
            = "=" _ val:expr() { val }

        rule decl() -> AST
            = "var" _ id:ident() _ ":" _ ty:ty() _ val:assign()? { AST::Decl(VarType::Mutable, ty, id, box val) }
            / "val" _ id:ident() _ ":" _ ty:ty() _ val:assign()? { AST::Decl(VarType::Immutable, ty, id, box val) }
            / "const" _ id:ident() _ ":" _ ty:ty() _ val:assign()? { AST::Decl(VarType::Const, ty, id, box val) }

        rule string_literal() -> AST
            = "\"" s:$((!"\"" [_])*) "\"" { AST::StringLiteral(s.to_string()) }

        rule char_literal() -> AST
            = c:$(single_quoted(<[_]>)) { AST::CharLiteral(c.chars().nth(1).unwrap()) }
        rule int_literal() -> AST
            = n:$(['0'..='9']+) ty:int_primitive_type()? { AST::IntLiteral(ty.unwrap_or(PrimitiveType::I32), n.parse().unwrap()) }
        rule unary() -> AST
            = "-" n:atom() { AST::Unary(UnaryOp::Negative, box n) }
            / "!" n:atom() { AST::Unary(UnaryOp::Negate, box n) }
            / "&" n:atom() { AST::AddrOf(box n) }
        rule atom() -> AST
            = n:(
                unary() /
                int_literal() /
                char_literal() /
                string_literal()
                ) { n }
            / "true" { AST::True }
            / "false" { AST::False }
            / n:ident() _ e:expr() { AST::Call { name: n, arg: box e } }
            / n:ident() { AST::Ref(n) }
            / n:parenthesized(<(e:expr()? {e.unwrap_or(AST::Empty)}) ** (_ "," _)>) { AST::Tuple(n) }
            / "fn" _ n:ident() _ param:ty() _ "->" _ ret:ty() {
                AST::Function {
                    name: n,
                    ty: box PrimitiveType::FunctionType {
                        ret_type: box ret,
                        param: box param,
                    }
                }
             }
        rule expr() -> AST = precedence! {
            e:atom() { e }
            e:parenthesized(<expr()>) { AST::Parenthesized(box e) }
            e:cond() { e }
            e:decl() { e }
            --
            x:(@) _ "+" _ y:@ { AST::Binary(BinaryOp::Add, box x, box y) }
            x:(@) _ "-" _ y:@ { AST::Binary(BinaryOp::Subtract, box x, box y) }
            --
            x:(@) _ "*" _ y:@ { AST::Binary(BinaryOp::Multiply, box x, box y) }
            x:(@) _ "/" _ y:@ { AST::Binary(BinaryOp::Divide, box x, box y) }
            --
            x:(@) _ "%" _ y:@ { AST::Binary(BinaryOp::Modulo, box x, box y) }
            --
            x:(@) _ "==" _ y:@ { AST::Binary(BinaryOp::Equals, box x, box y) }
            x:(@) _ "!=" _ y:@ { AST::Binary(BinaryOp::NotEquals, box x, box y) }
            --
            x:(@) _ "=" _ y:@ { AST::Assign(box x, box y) }
            --
            x:(@) _ "[" _ y:expr() _ "]" {
                AST::Index {
                    lhs: box x,
                    indexer: box y
                }
            }
        }

        rule line() -> AST
            = _ line:expr() _ { line }

        pub rule block() -> AST
            = _ blck:curly(<line()*>) _ { AST::Block(blck) }
            / _ blck:line() _ { blck }
    }
}

fn main() {
    println!(
        "{:#?}",
        lang_parser::block(
            "
{
    fn puts str -> i32
    fn printf (*u8, ...) -> i32

    fn swap (x: any, y: any) -> (any, any)
        = (y, x)

    swap(a, b)
    printf(\"Hello %s\n\", \"World!\")
}
        "
        )
    );
}
