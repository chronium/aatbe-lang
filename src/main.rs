#![feature(box_syntax)]

#[derive(Debug)]
pub enum PrimitiveType {
    U8,
    U16,
    U32,
    U64,
    U128,
    I8,
    I16,
    I32,
    I64,
    I128,
    Boolean,
    Char,
    Str,
    TupleType(Vec<PrimitiveType>),
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
pub enum ParseTree {
    True,
    False,
    IntLiteral(PrimitiveType, u64),
    CharLiteral(char),
    StringLiteral(String),
    Unary(UnaryOp, Box<ParseTree>),
    Binary(BinaryOp, Box<ParseTree>, Box<ParseTree>),
    Parenthesized(Box<ParseTree>),
    If(Box<ParseTree>, Box<ParseTree>),
    IfElse(Box<ParseTree>, Box<ParseTree>),
    Block(Vec<ParseTree>),
    Decl(VarType, PrimitiveType, String, Box<Option<ParseTree>>),
    Assign(Box<ParseTree>, Box<ParseTree>),
    Index(Box<ParseTree>, Box<ParseTree>),
    Ref(String),
    Tuple(Vec<ParseTree>),
}

peg::parser! {
    grammar lang_parser() for str {
        use ParseTree;

        rule _() = quiet!{[' ' | '\n' | '\t']*}

        rule ident_key()
            = ['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '_' | '-' | '0'..='9']*

        rule keywords()
            = "u8" / "u16" / "u32" / "u64" / "u128"
            / "i8" / "i16" / "i32" / "i64" / "i128"
            / "if" / "else"
            / "bool" / "char" / "str"
            / "true" / "false"
            / "val" / "var" / "const"

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

        rule ty() -> PrimitiveType
            = prim:int_primitive_type() { prim }
            / "bool" { PrimitiveType::Boolean }
            / "char" { PrimitiveType::Char }
            / "str" { PrimitiveType::Str }
            / prim:parenthesized(<ty() ** (_ "," _)>) { PrimitiveType::TupleType(prim) }
            / expected!("type")

        rule parenthesized<T>(x: rule<T>) -> T = "(" _ v:x() _ ")" { v }
        rule curly<T>(x: rule<T>) -> T = "{" _ v:x() _ "}" { v }
        rule boxed<T>(x: rule<T>) -> Box<T> =  v:x() { box v }
        rule double_quoted<T>(x: rule<T>) -> T = "\"" v:x() "\"" { v }
        rule single_quoted<T>(x: rule<T>) -> T = "'" v:x() "'" { v }
        rule bracketed<T>(x: rule<T>) -> T = "[" v:x() "]" { v }

        rule else() -> ParseTree
            = "else" _ els:block() { els }

        rule cond() -> ParseTree
            = "if" _ cond:expr() _ then:block() _ els:(else())? {
                let if_expr = ParseTree::If(box cond, box then);
                match els {
                    None => if_expr,
                    Some(else_cnd) => ParseTree::IfElse(box if_expr, box else_cnd)
                }
            }

        rule decl() -> ParseTree
            = "var" _ id:ident() _ ":" _ ty:ty() _ "=" _ val:(expr())? { ParseTree::Decl(VarType::Mutable, ty, id, box val) }
            / "val" _ id:ident() _ ":" _ ty:ty() _ "=" _ val:(expr())? { ParseTree::Decl(VarType::Immutable, ty, id, box val) }
            / "const" _ id:ident() _ ":" _ ty:ty() _ "=" _ val:(expr())? { ParseTree::Decl(VarType::Const, ty, id, box val) }

        rule string_literal() -> ParseTree
            = "\"" s:$((!"\"" [_])*) "\"" { ParseTree::StringLiteral(s.to_string()) }

        rule char_literal() -> ParseTree
            = c:$(single_quoted(<[_]>)) { ParseTree::CharLiteral(c.chars().nth(1).unwrap()) }
        rule int_literal() -> ParseTree
            = n:$(['0'..='9']+) ty:int_primitive_type()? { ParseTree::IntLiteral(ty.unwrap_or(PrimitiveType::I32), n.parse().unwrap()) }
        rule unary() -> ParseTree
            = "-" n:atom() { ParseTree::Unary(UnaryOp::Negative, box n) }
            / "!" n:atom() { ParseTree::Unary(UnaryOp::Negate, box n) }
        rule atom() -> ParseTree
            = n:(
                unary() /
                int_literal() /
                char_literal() /
                string_literal()
                ) { n }
            / "true" { ParseTree::True }
            / "false" { ParseTree::False }
            / n:ident() { ParseTree::Ref(n) }
            / n:parenthesized(<expr() ** (_ "," _)>) { ParseTree::Tuple(n) }
        rule expr() -> ParseTree = precedence! {
            e:atom() { e }
            e:parenthesized(<expr()>) { ParseTree::Parenthesized(box e) }
            e:cond() { e }
            e:decl() { e }
            --
            x:(@) _ "+" _ y:@ { ParseTree::Binary(BinaryOp::Add, box x, box y) }
            x:(@) _ "-" _ y:@ { ParseTree::Binary(BinaryOp::Subtract, box x, box y) }
            --
            x:(@) _ "*" _ y:@ { ParseTree::Binary(BinaryOp::Multiply, box x, box y) }
            x:(@) _ "/" _ y:@ { ParseTree::Binary(BinaryOp::Divide, box x, box y) }
            --
            x:(@) _ "%" _ y:@ { ParseTree::Binary(BinaryOp::Modulo, box x, box y) }
            --
            x:(@) _ "==" _ y:@ { ParseTree::Binary(BinaryOp::Equals, box x, box y) }
            x:(@) _ "!=" _ y:@ { ParseTree::Binary(BinaryOp::NotEquals, box x, box y) }
            --
            x:(@) _ "=" _ y:@ { ParseTree::Assign(box x, box y) }
            --
            x:(@) _ "[" _ y:expr() _ "]" { ParseTree::Index(box x, box y) }
        }

        rule line() -> ParseTree
            = _ line:decl() _ { line }
            / _ line:expr() _ { line }

        pub rule block() -> ParseTree
            = _ blck:curly(<line()*>) _ { ParseTree::Block(blck) }
            / _ blck:line() _ { blck }
    }
}

fn main() {
    println!(
        "{:#?}",
        lang_parser::block(
            "
{
    val tuple: (i32, str, char) = (128i32, \"hello\", '!')
    const Unit: () = ()
}
        "
        )
    );
}
