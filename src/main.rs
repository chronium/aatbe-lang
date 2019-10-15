#![feature(box_syntax)]

use llvm_sys_wrapper::*;
use std::ops::Deref;

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
}

#[derive(Debug)]
pub enum UnaryOp {
    Negative,
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
pub enum AstNode {
    True,
    False,
    IntLiteral(PrimitiveType, u64),
    Unary(UnaryOp, Box<AstNode>),
    Binary(BinaryOp, Box<AstNode>, Box<AstNode>),
    Parenthesized(Box<AstNode>),
    If(Box<AstNode>, Box<AstNode>),
    IfElse(Box<AstNode>, Box<AstNode>),
    Block(Vec<Box<AstNode>>),
}

peg::parser! {
    grammar lang_parser() for str {
        use AstNode;

        rule _() = quiet!{[' ' | '\n' | '\t']*}

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

        rule parenthesized<T>(x: rule<T>) -> T = "(" _ v:x() _ ")" { v }
        rule curly<T>(x: rule<T>) -> T = "{" _ v:x() _ "}" { v }
        rule boxed<T>(x: rule<T>) -> Box<T> =  v:x() { box v }

        rule else() -> AstNode
            = "else" _ els:block() { els }

        rule cond() -> AstNode
            = "if" _ cond:expr() _ then:block() _ els:(else())? {
                let if_expr = AstNode::If(box cond, box then);
                match els {
                    None => if_expr,
                    Some(else_cnd) => AstNode::IfElse(box if_expr, box else_cnd)
                }
            }

        rule int_literal() -> AstNode
            = n:$(['0'..='9']+) ty:int_primitive_type()? { AstNode::IntLiteral(ty.unwrap_or(PrimitiveType::I32), n.parse().unwrap()) }
        rule unary() -> AstNode
            = "-" n:int_literal() { AstNode::Unary(UnaryOp::Negative, box n) }
        rule atom() -> AstNode
            = n:unary() { n }
            / n:int_literal() { n }
            / "true" { AstNode::True }
            / "false" { AstNode::False }
        rule expr() -> AstNode = precedence! {
            e:atom() { e }
            e:parenthesized(<expr()>) { AstNode::Parenthesized(box e) }
            e:cond() { e }
            --
            x:(@) _ "+" _ y:@ { AstNode::Binary(BinaryOp::Add, box x, box y) }
            x:(@) _ "-" _ y:@ { AstNode::Binary(BinaryOp::Subtract, box x, box y) }
            --
            x:(@) _ "*" _ y:@ { AstNode::Binary(BinaryOp::Multiply, box x, box y) }
            x:(@) _ "/" _ y:@ { AstNode::Binary(BinaryOp::Divide, box x, box y) }
            --
            x:(@) _ "%" _ y:@ { AstNode::Binary(BinaryOp::Modulo, box x, box y) }
            --
            x:(@) _ "==" _ y:@ { AstNode::Binary(BinaryOp::Equals, box x, box y) }
            x:(@) _ "!=" _ y:@ { AstNode::Binary(BinaryOp::NotEquals, box x, box y) }
        }

        pub rule block() -> AstNode
            = _ blck:curly(<boxed(<expr()>)*>) _ { AstNode::Block(blck) }
            / _ blck:expr() _ { blck }
    }
}

fn main() {
    println!(
        "###1: if true 1 + 2 else if false 2 * 8 else 100\n{:#?}\n\n###2: if (true) {{ 1 + 2 }} else if (false) {{ 2 * 8 }} else (100)\n{:#?}",
        lang_parser::block(
            r#"
        if true 1 + 2 else if false 2 * 8 else 100
    "#),
        lang_parser::block(
            r#"
            if (true) {
                1 + 2
            } else if (false) {
                2 * 8
            } else (100)
            "#
        )
    );
}
