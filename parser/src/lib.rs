#![feature(box_syntax, box_patterns, or_patterns)]
#![allow(dead_code)]

pub mod ast;
pub mod lexer;
pub mod parser;

mod tests;

use crate::{
    ast::{Expression, FloatSize, IntSize, PrimitiveType, TypeKind, AST},
    lexer::{
        token,
        token::{Keyword, Symbol, Type},
    },
    parser::{ParseError, ParseResult, Parser},
};

use ast::FunctionType;
use std::path::PathBuf;

impl Parser {
    fn parse_type(&mut self) -> ParseResult<PrimitiveType> {
        let token = self.next();
        if let Some(tok) = token {
            match tok.sym() {
                Some(Symbol::Colon) => Some(PrimitiveType::Symbol(ident!(res self)?)),
                Some(Symbol::Unit) => Some(PrimitiveType::Unit),
                Some(Symbol::GoDot) => Some(PrimitiveType::Varargs),
                Some(Symbol::Ampersand) => capture!(res parse_type, self)
                    .ok()
                    .and_then(|ty| Some(PrimitiveType::Ref(box ty))),
                Some(Symbol::At) => capture!(res parse_type, self)
                    .ok()
                    .and_then(|ty| Some(PrimitiveType::Box(box ty))),
                Some(Symbol::LBracket) => capture!(res parse_type, self).ok().and_then(|ty| {
                    let len = if sym!(bool Comma, self) {
                        self.peek().and_then(|tok| tok.int()).map(|len| len as u32)
                    } else {
                        None
                    };
                    if len.is_some() {
                        self.next();
                    }
                    sym!(RBracket, self);
                    Some(match len {
                        None => PrimitiveType::Slice { ty: box ty },
                        Some(len) => PrimitiveType::Array { ty: box ty, len },
                    })
                }),
                _ => None,
            }
            .or_else(|| match tok.kw() {
                Some(Keyword::Bool) => Some(PrimitiveType::Bool),
                _ => None,
            })
            .or_else(|| match tok.ident() {
                Some(tyref) => {
                    if matches!(self.peek_symbol(Symbol::LBracket), Some(true)) {
                        self.next();
                        let types = self
                            .parse_type_list()
                            .expect(format!("Expected type list at {}", tyref).as_str());
                        sym!(RBracket, self);
                        Some(PrimitiveType::GenericTypeRef(tyref, types))
                    } else {
                        Some(PrimitiveType::TypeRef(tyref))
                    }
                }
                None => None,
            })
            .or_else(|| match tok.ty() {
                Some(Type::Str) => Some(PrimitiveType::Str),
                Some(Type::Char) => Some(PrimitiveType::Char),
                Some(Type::I8) => Some(PrimitiveType::Int(IntSize::Bits8)),
                Some(Type::I16) => Some(PrimitiveType::Int(IntSize::Bits16)),
                Some(Type::I32) => Some(PrimitiveType::Int(IntSize::Bits32)),
                Some(Type::I64) => Some(PrimitiveType::Int(IntSize::Bits64)),
                Some(Type::U8) => Some(PrimitiveType::UInt(IntSize::Bits8)),
                Some(Type::U16) => Some(PrimitiveType::UInt(IntSize::Bits16)),
                Some(Type::U32) => Some(PrimitiveType::UInt(IntSize::Bits32)),
                Some(Type::U64) => Some(PrimitiveType::UInt(IntSize::Bits64)),
                Some(Type::F32) => Some(PrimitiveType::Float(FloatSize::Bits32)),
                Some(Type::F64) => Some(PrimitiveType::Float(FloatSize::Bits64)),
                _ => None,
            })
        } else {
            None
        }
        .and_then(|ty| {
            if sym!(bool Star, self) {
                Some(PrimitiveType::Pointer(box ty))
            } else {
                Some(ty)
            }
        })
        .ok_or(ParseError::ExpectedType)
    }

    fn parse_named_type(&mut self) -> ParseResult<PrimitiveType> {
        let name = ident!(required self);
        if sym!(bool Comma, self)
            | sym!(bool Arrow, self)
            | sym!(bool LBracket, self)
            | sym!(bool RBracket, self)
        {
            return Err(ParseError::Continue);
        };
        let ty = if sym!(bool Colon, self) {
            Some(box capture!(res parse_type, self)?)
        } else {
            None
        };
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

            let pb = PathBuf::from(path);

            Ok(AST::Import(
                pb.to_str().expect("ICE parse_use pb.to_str").to_string(),
            ))
        } else {
            Err(ParseError::ExpectedPath)
        }
    }

    fn parse_type_list(&mut self) -> ParseResult<Vec<PrimitiveType>> {
        let mut params = vec![];

        loop {
            params.push(
                capture!(res parse_named_type, self)
                    .or_else(|_| capture!(res parse_type, self))
                    .or(Err(ParseError::ExpectedType))?,
            );

            if !sym!(bool Comma, self) {
                break;
            }
        }
        Ok(params)
    }

    fn parse_free_type_list(&mut self) -> ParseResult<Vec<PrimitiveType>> {
        let mut params = vec![];

        loop {
            params.push(capture!(res parse_type, self).or(Err(ParseError::ExpectedType))?);

            if !sym!(bool Comma, self) {
                break;
            }
        }
        Ok(params)
    }

    fn parse_type_names(&mut self) -> ParseResult<Vec<String>> {
        let mut params = vec![];

        loop {
            params.push(ident!(res self)?);

            if !sym!(bool Comma, self) {
                break;
            }
        }
        Ok(params)
    }

    fn parse_record(&mut self) -> ParseResult<AST> {
        kw!(Record, self);
        let name = ident!(required self);

        let type_names = if sym!(bool LBracket, self) {
            let type_names = self.parse_type_names()?;
            sym!(required RBracket, self);
            Some(type_names)
        } else {
            None
        };

        let fields = if sym!(bool Unit, self) {
            vec![PrimitiveType::Unit]
        } else {
            sym!(required LParen, self);
            let fields = self
                .parse_type_list()
                .expect(format!("Expected type list at {}", name).as_str());
            sym!(required RParen, self);
            fields
        };

        Ok(AST::Record(name, type_names, fields))
    }

    fn parse_const_global(&mut self) -> ParseResult<AST> {
        let export = kw!(bool Exp, self);
        let cons = kw!(bool Const, self);
        let glob = kw!(bool Global, self);

        if !cons && !glob {
            return Err(ParseError::Continue);
        }

        let ty = capture!(res parse_named_type, self)?;

        sym!(required Assign, self);
        let value = box capture!(self, parse_expression).ok_or(ParseError::ExpectedExpression)?;

        Ok(if cons {
            AST::Constant { ty, value, export }
        } else {
            AST::Global { ty, value, export }
        })
    }

    fn parse_typedef(&mut self) -> ParseResult<AST> {
        kw!(Type, self);
        let name = ident!(res self)?;

        let type_names = if sym!(bool LBracket, self) {
            let type_names = self.parse_type_names()?;
            sym!(required RBracket, self);
            Some(type_names)
        } else {
            None
        };

        let variants = if sym!(bool Assign, self) {
            let ty = capture!(res parse_type, self)?;
            let vars = self.parse_free_type_list().ok();
            Some(if sym!(bool Pipe, self) {
                let mut variants = vec![];

                if let PrimitiveType::TypeRef(name) = ty {
                    variants.push(TypeKind::Variant(name, vars));
                } else {
                    variants.push(TypeKind::Newtype(ty));
                }
                loop {
                    if let Ok(name) = ident!(res self) {
                        variants.push(TypeKind::Variant(name, self.parse_free_type_list().ok()));
                    } else {
                        variants.push(TypeKind::Newtype(capture!(res parse_type, self)?));
                    }

                    if !sym!(bool Pipe, self) {
                        break;
                    }
                }

                variants
            } else {
                if vars.is_some() {
                    if let PrimitiveType::TypeRef(name) = ty {
                        vec![TypeKind::Variant(name, vars)]
                    } else {
                        return Err(ParseError::ExpectedIdent);
                    }
                } else {
                    vec![TypeKind::Newtype(ty)]
                }
            })
        } else {
            None
        };

        Ok(AST::Typedef {
            name,
            type_names,
            variants,
        })
    }

    fn parse_function(&mut self) -> ParseResult<AST> {
        let mut attributes = vec![];

        loop {
            match capture!(self, parse_attribute) {
                None => break,
                Some(attr) => attributes.push(attr),
            }
        }

        let export = kw!(bool Exp, self);
        let ext = kw!(bool Extern, self);
        kw!(Fn, self);
        let name = ident!(required self);

        let type_names = if sym!(bool LBracket, self) {
            let type_names = self.parse_type_names()?;
            sym!(required RBracket, self);
            type_names
        } else {
            vec![]
        };

        let params = self
            .parse_type_list()
            .expect(format!("Expected type list at {}", name).as_str());

        let ret_ty = box if sym!(bool Arrow, self) {
            capture!(res parse_type, self)?
        } else {
            PrimitiveType::Unit
        };

        let mut body = None;
        if sym!(bool Assign, self) {
            body = capture!(box parse_expression, err ExpectedExpression, self);
        }

        Ok(AST::Expr(Expression::Function {
            name,
            attributes,
            body,
            type_names,
            export,
            ty: FunctionType {
                ext,
                ret_ty,
                params,
            },
        }))
    }
}
