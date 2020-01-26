#![feature(box_syntax)]
#![allow(dead_code)]

pub mod ast;
pub mod lexer;
pub mod parser;

mod tests;

use ast::{Expression, IntSize, PrimitiveType, AST};
use lexer::{
    token,
    token::{Keyword, Symbol, Type},
};

use crate::parser::{ParseError, ParseResult, Parser};

impl Parser {
    fn parse_type(&mut self) -> ParseResult<PrimitiveType> {
        let token = self.next();
        if let Some(tok) = token {
            match tok.sym() {
                Some(Symbol::Unit) => return Ok(PrimitiveType::Unit),
                Some(Symbol::GoDot) => return Ok(PrimitiveType::Varargs),
                Some(Symbol::Ampersand) => match capture!(res parse_type, self) {
                    Err(_) => {}
                    Ok(ty) => return Ok(PrimitiveType::Ref(box ty)),
                },
                _ => {}
            };

            match tok.kw() {
                Some(Keyword::Bool) => return Ok(PrimitiveType::Bool),
                _ => {}
            }

            match tok.ty() {
                Some(Type::Str) => return Ok(PrimitiveType::Str),
                Some(Type::I8) => return Ok(PrimitiveType::Int(IntSize::Bits8)),
                Some(Type::I16) => return Ok(PrimitiveType::Int(IntSize::Bits16)),
                Some(Type::I32) => return Ok(PrimitiveType::Int(IntSize::Bits32)),
                Some(Type::I64) => return Ok(PrimitiveType::Int(IntSize::Bits64)),
                Some(Type::U8) => return Ok(PrimitiveType::UInt(IntSize::Bits8)),
                Some(Type::U16) => return Ok(PrimitiveType::UInt(IntSize::Bits16)),
                Some(Type::U32) => return Ok(PrimitiveType::UInt(IntSize::Bits32)),
                Some(Type::U64) => return Ok(PrimitiveType::UInt(IntSize::Bits64)),
                _ => {}
            }

            match tok.ident() {
                Some(s) => return Ok(PrimitiveType::TypeRef(s)),
                None => {}
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

    fn parse_use(&mut self) -> ParseResult<AST> {
        kw!(Use, self);
        if let Some(path) = self.peek_str() {
            self.next();
            Ok(AST::Import(path))
        } else {
            Err(ParseError::ExpectedPath)
        }
    }

    fn parse_type_list(&mut self, terminator: Symbol) -> ParseResult<Vec<PrimitiveType>> {
        let mut params = vec![];
        loop {
            match self.peek_symbol(terminator) {
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
        self.next();
        Ok(params)
    }

    fn parse_record(&mut self) -> ParseResult<AST> {
        kw!(Record, self);
        let name = ident!(required self);

        let types = if sym!(bool Unit, self) {
            vec![PrimitiveType::Unit]
        } else {
            sym!(required LParen, self);
            self.parse_type_list(Symbol::RParen)
                .expect(format!("Expected type list at {}", name).as_str())
        };

        Ok(AST::Record(name, types))
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

        let params = self
            .parse_type_list(Symbol::Arrow)
            .expect(format!("Expected type list at {}", name).as_str());

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
