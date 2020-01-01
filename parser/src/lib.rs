#![feature(box_syntax)]
#![allow(dead_code)]

mod ast;
mod lexer;
mod parser;
mod tests;

use ast::{Expression, IntType, PrimitiveType, UIntType, AST};
use lexer::{
    token,
    token::{Keyword, Symbol, Type},
};

use crate::parser::{ParseError, ParseResult, Parser};

impl Parser {
    fn parse_type(&mut self) -> ParseResult<PrimitiveType> {
        let token = self.next();
        if let Some(tok) = token {
            if let Some(Symbol::Unit) = tok.sym() {
                return Ok(PrimitiveType::Unit);
            }

            match tok.ty() {
                Some(Type::Str) => return Ok(PrimitiveType::Str),
                Some(Type::I8) => return Ok(PrimitiveType::Int(IntType::I8)),
                Some(Type::I16) => return Ok(PrimitiveType::Int(IntType::I16)),
                Some(Type::I32) => return Ok(PrimitiveType::Int(IntType::I32)),
                Some(Type::I64) => return Ok(PrimitiveType::Int(IntType::I64)),
                Some(Type::U8) => return Ok(PrimitiveType::UInt(UIntType::U8)),
                Some(Type::U16) => return Ok(PrimitiveType::UInt(UIntType::U16)),
                Some(Type::U32) => return Ok(PrimitiveType::UInt(UIntType::U32)),
                Some(Type::U64) => return Ok(PrimitiveType::UInt(UIntType::U64)),
                _ => {}
            }
        }

        Err(ParseError::ExpectedType)
    }

    fn parse_named_type(&mut self) -> ParseResult<PrimitiveType> {
        let name = ident!(required self);
        sym!(required Colon, self);
        let ty = box capture!(res parse_type, self)?;
        Ok(PrimitiveType::NamedType { name, ty })
    }

    fn parse_attribute(&mut self) -> Option<String> {
        sym!(At, self);
        ident!(self)
    }

    fn parse_function(&mut self) -> ParseResult<AST> {
        let mut attributes = Vec::new();

        loop {
            match capture!(self, parse_attribute) {
                None => break,
                Some(attr) => attributes.push(attr),
            }
        }

        let ext = kw!(bool Extern, self);
        kw!(Fn, self);
        let name = ident!(required self);

        let mut params = vec![];
        loop {
            match self.peek_symbol(Symbol::Arrow) {
                Some(true) => break,
                Some(false) => {
                    if params.len() > 0 {
                        sym!(required Comma, self);
                    }
                    let ty = capture!(res parse_named_type, self)
                        .or_else(|_| capture!(res parse_type, self))
                        .or(Err(ParseError::ExpectedType))?;
                    params.push(ty);
                }
                None => return Err(ParseError::UnexpectedEOF),
            }
        }
        sym!(required Arrow, self);

        let ret_ty = box capture!(res parse_type, self)?;

        let mut body = None;
        if sym!(bool Assign, self) {
            body = capture!(box parse_expression, err ExpectedExpression, self);
        }

        Ok(AST::Expr(Expression::Function {
            name,
            attributes,
            body,
            ty: PrimitiveType::Function {
                ext,
                ret_ty,
                params,
            },
        }))
    }
}
