#![feature(box_syntax, box_patterns, or_patterns)]
#![allow(dead_code)]

pub mod ast;
pub mod lexer;
pub mod parser;

mod tests;

use crate::{
    ast::{Expression, FloatSize, IntSize, Type, TypeKind, AST},
    lexer::{
        token,
        token::{Keyword, Symbol, TokenType},
    },
    parser::{ParseError, ParseResult, Parser},
};

use ast::FunctionType;
use std::path::PathBuf;

impl Parser {
    fn parse_type(&mut self) -> ParseResult<Type> {
        let token = self.next();
        if let Some(tok) = token {
            match tok.sym() {
                Some(Symbol::Colon) => Some(Type::Symbol(ident!(res self)?)),
                Some(Symbol::Unit) => Some(Type::Unit),
                Some(Symbol::GoDot) => Some(Type::Varargs),
                Some(Symbol::Ampersand) => capture!(res parse_type, self)
                    .ok()
                    .and_then(|ty| Some(Type::Ref(box ty))),
                Some(Symbol::At) => capture!(res parse_type, self)
                    .ok()
                    .and_then(|ty| Some(Type::Box(box ty))),
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
                        None => Type::Slice { ty: box ty },
                        Some(len) => Type::Array { ty: box ty, len },
                    })
                }),
                _ => None,
            }
            .or_else(|| match tok.kw() {
                Some(Keyword::Bool) => Some(Type::Bool),
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
                        Some(Type::GenericTypeRef(tyref, types))
                    } else {
                        if matches!(self.peek_symbol(Symbol::Doubly), Some(true)) {
                            let mut res = vec![tyref];
                            self.next();
                            loop {
                                match self.peek_ident() {
                                    None => break,
                                    Some(ty) => {
                                        self.next();
                                        res.push(ty);
                                        if matches!(self.peek_symbol(Symbol::Doubly), Some(true)) {
                                            self.next();
                                        }
                                    }
                                }
                            }
                            Some(Type::Path(res))
                        } else {
                            Some(Type::TypeRef(tyref))
                        }
                    }
                }
                None => None,
            })
            .or_else(|| match tok.ty() {
                Some(TokenType::Str) => Some(Type::Str),
                Some(TokenType::Char) => Some(Type::Char),
                Some(TokenType::I8) => Some(Type::Int(IntSize::Bits8)),
                Some(TokenType::I16) => Some(Type::Int(IntSize::Bits16)),
                Some(TokenType::I32) => Some(Type::Int(IntSize::Bits32)),
                Some(TokenType::I64) => Some(Type::Int(IntSize::Bits64)),
                Some(TokenType::U8) => Some(Type::UInt(IntSize::Bits8)),
                Some(TokenType::U16) => Some(Type::UInt(IntSize::Bits16)),
                Some(TokenType::U32) => Some(Type::UInt(IntSize::Bits32)),
                Some(TokenType::U64) => Some(Type::UInt(IntSize::Bits64)),
                Some(TokenType::F32) => Some(Type::Float(FloatSize::Bits32)),
                Some(TokenType::F64) => Some(Type::Float(FloatSize::Bits64)),
                _ => None,
            })
        } else {
            None
        }
        .and_then(|ty| {
            let mut ty = ty;
            while sym!(bool Star, self) {
                ty = Type::Pointer(box ty);
            }
            Some(ty)
        })
        .ok_or(ParseError::ExpectedType)
    }

    fn parse_named_type(&mut self) -> ParseResult<Type> {
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
        Ok(Type::NamedType { name, ty })
    }

    fn parse_attribute(&mut self) -> Option<String> {
        sym!(At, self);
        ident!(self)
    }

    fn parse_use(&mut self) -> ParseResult<AST> {
        kw!(Use, self);

        let path = path!(module self);

        Ok(AST::Import(path))
    }

    fn parse_type_list(&mut self) -> ParseResult<Vec<Type>> {
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

    fn parse_free_type_list(&mut self) -> ParseResult<Vec<Type>> {
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
            vec![Type::Unit]
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
        let export = kw!(bool Public, self);
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
        kw!(TokenType, self);
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

                if let Type::TypeRef(name) = ty {
                    self.variants.push(name.clone());
                    variants.push(TypeKind::Variant(name, vars));
                } else {
                    variants.push(TypeKind::Newtype(ty));
                }
                loop {
                    if let Ok(name) = ident!(res self) {
                        self.variants.push(name.clone());
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
                    if let Type::TypeRef(name) = ty {
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

        let public = kw!(bool Public, self);
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
            Type::Unit
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
            public,
            ty: FunctionType {
                ext,
                ret_ty,
                params,
            },
        }))
    }

    fn parse_module(&mut self) -> ParseResult<AST> {
        if kw!(bool Module, self) {
            let name = ident!(required self);
            sym!(required LCurly, self);
            if sym!(bool RCurly, self) {
                Ok(AST::Module(name, box AST::File(vec![])))
            } else {
                let res = Ok(AST::Module(name, box self.parse()?));
                sym!(required RCurly, self);
                res
            }
        } else {
            Err(ParseError::Continue)
        }
    }
}
