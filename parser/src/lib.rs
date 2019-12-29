#![feature(box_syntax)]
#![allow(dead_code)]

mod ast;
mod lexer;
mod parser;

use ast::{Expression, IntType, PrimitiveType, UIntType, AST};
use lexer::{
    token,
    token::{Keyword, Symbol, Type},
};

use crate::parser::{ParseError, ParseResult, Parser};

impl Parser {
    fn parse_type(&mut self) -> Option<PrimitiveType> {
        let token = self.next();
        if let Some(tok) = token {
            if let Some(Symbol::Unit) = tok.sym() {
                return Some(PrimitiveType::Unit);
            }

            match tok.ty() {
                Some(Type::Str) => return Some(PrimitiveType::Str),
                Some(Type::I8) => return Some(PrimitiveType::Int(IntType::I8)),
                Some(Type::I16) => return Some(PrimitiveType::Int(IntType::I16)),
                Some(Type::I32) => return Some(PrimitiveType::Int(IntType::I32)),
                Some(Type::I64) => return Some(PrimitiveType::Int(IntType::I64)),
                Some(Type::U8) => return Some(PrimitiveType::UInt(UIntType::U8)),
                Some(Type::U16) => return Some(PrimitiveType::UInt(UIntType::U16)),
                Some(Type::U32) => return Some(PrimitiveType::UInt(UIntType::U32)),
                Some(Type::U64) => return Some(PrimitiveType::UInt(UIntType::U64)),
                _ => {}
            }
        }

        None
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
                    let ty = capture!(expect parse_type, err ExpectedType, self);
                    params.push(ty);
                }
                None => return Err(ParseError::UnexpectedEOF),
            }
        }
        sym!(required Arrow, self);

        let ret_ty = capture!(expect parse_type, err ExpectedType, self);

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
                ret_ty: box ret_ty,
                params,
            },
        }))
    }
}

#[cfg(test)]
mod parser_tests {
    use super::{
        ast::AtomKind,
        lexer::{token::Token, Lexer},
        Expression, IntType, ParseError, Parser, PrimitiveType, AST,
    };

    fn tt(code: &'static str) -> Vec<Token> {
        let mut lexer = Lexer::new(code);
        lexer.lex();

        lexer.tt()
    }

    fn attr(attribs: Vec<&'static str>) -> Vec<String> {
        attribs.iter().map(|attr| attr.to_string()).collect()
    }

    macro_rules! parse_test {
        ($tt:expr, $name:expr) => {{
            let mut parser = Parser::new(tt($tt));
            let res = parser.parse();
            let pt = parser.pt().as_ref().expect($name);

            assert_eq!(res, Ok(()));

            pt.clone()
        }};
    }

    #[test]
    fn invalid_eof() {
        let mut parser = Parser::new(vec![]);
        let res = parser.parse();
        assert_eq!(
            res.expect_err("Expected InvalidEOF"),
            ParseError::InvalidEOF
        );
    }

    #[test]
    fn eof() {
        let mut parser = Parser::new(tt(""));
        let res = parser.parse();

        assert_eq!(res, Ok(()))
    }

    #[test]
    fn unit_function() {
        let pt = parse_test!("fn test () -> ()", "Unit Function");

        assert_eq!(
            pt,
            AST::File(vec![AST::Expr(Expression::Function {
                name: "test".to_string(),
                attributes: Vec::new(),
                body: None,
                ty: PrimitiveType::Function {
                    ext: false,
                    ret_ty: box PrimitiveType::Unit,
                    params: vec![PrimitiveType::Unit],
                }
            }),])
        );
    }

    #[test]
    fn extern_function() {
        let pt = parse_test!("extern fn test () -> ()", "Extern Function");

        assert_eq!(
            pt,
            AST::File(vec![AST::Expr(Expression::Function {
                name: "test".to_string(),
                attributes: Vec::new(),
                body: None,
                ty: PrimitiveType::Function {
                    ext: true,
                    ret_ty: box PrimitiveType::Unit,
                    params: vec![PrimitiveType::Unit],
                }
            }),])
        );
    }

    #[test]
    fn entry_function() {
        let pt = parse_test!(
            "
@entry
fn main () -> ()
",
            "Entry Function"
        );

        assert_eq!(
            pt,
            AST::File(vec![AST::Expr(Expression::Function {
                name: "main".to_string(),
                attributes: attr(vec!["entry"]),
                body: None,
                ty: PrimitiveType::Function {
                    ext: false,
                    ret_ty: box PrimitiveType::Unit,
                    params: vec![PrimitiveType::Unit],
                }
            }),])
        );
    }

    #[test]
    fn expr_function() {
        let pt = parse_test!(
            "
@entry
fn main () -> () = ()
",
            "Unit Expr Function"
        );

        assert_eq!(
            pt,
            AST::File(vec![AST::Expr(Expression::Function {
                name: "main".to_string(),
                attributes: attr(vec!["entry"]),
                body: Some(box Expression::Atom(AtomKind::Unit)),
                ty: PrimitiveType::Function {
                    ext: false,
                    ret_ty: box PrimitiveType::Unit,
                    params: vec![PrimitiveType::Unit],
                }
            }),])
        );
    }

    #[test]
    fn binary_function() {
        let pt = parse_test!(
            "
@entry
fn main () -> () = 1 * 2 + 3 * 4
",
            "Binary Function"
        );

        assert_eq!(
            pt,
            AST::File(vec![AST::Expr(Expression::Function {
                name: "main".to_string(),
                attributes: attr(vec!["entry"]),
                body: Some(box Expression::Binary(
                    box Expression::Binary(
                        box Expression::Atom(AtomKind::Integer(1)),
                        String::from("*"),
                        box Expression::Atom(AtomKind::Integer(2))
                    ),
                    String::from("+"),
                    box Expression::Binary(
                        box Expression::Atom(AtomKind::Integer(3)),
                        String::from("*"),
                        box Expression::Atom(AtomKind::Integer(4))
                    )
                )),
                ty: PrimitiveType::Function {
                    ext: false,
                    ret_ty: box PrimitiveType::Unit,
                    params: vec![PrimitiveType::Unit],
                }
            }),])
        );
    }

    #[test]
    fn empty_block_function() {
        let pt = parse_test!(
            "
@entry
fn main () -> () = {}
",
            "Empty Block Function"
        );

        assert_eq!(
            pt,
            AST::File(vec![AST::Expr(Expression::Function {
                name: "main".to_string(),
                attributes: attr(vec!["entry"]),
                body: Some(box Expression::Block(vec![])),
                ty: PrimitiveType::Function {
                    ext: false,
                    ret_ty: box PrimitiveType::Unit,
                    params: vec![PrimitiveType::Unit],
                }
            }),])
        );
    }

    #[test]
    fn block_function() {
        let pt = parse_test!(
            "
@entry
fn main () -> () = { () }
",
            "Block Function"
        );

        assert_eq!(
            pt,
            AST::File(vec![AST::Expr(Expression::Function {
                name: "main".to_string(),
                attributes: attr(vec!["entry"]),
                body: Some(box Expression::Block(vec![Expression::Atom(
                    AtomKind::Unit
                )])),
                ty: PrimitiveType::Function {
                    ext: false,
                    ret_ty: box PrimitiveType::Unit,
                    params: vec![PrimitiveType::Unit],
                }
            }),])
        );
    }

    #[test]
    fn funcall() {
        let pt = parse_test!(
            "
extern fn puts str -> i32

fn test str i32 -> ()

@entry
fn main () -> () = {
    puts \"Hello World\"
    puts \"Hallo\"
    test \"Test\" $ 1 + 2
}
",
            "Block Function"
        );

        assert_eq!(
            pt,
            AST::File(vec![
                AST::Expr(Expression::Function {
                    name: "puts".to_string(),
                    attributes: vec![],
                    body: None,
                    ty: PrimitiveType::Function {
                        ext: true,
                        ret_ty: box PrimitiveType::Int(IntType::I32),
                        params: vec![PrimitiveType::Str],
                    }
                }),
                AST::Expr(Expression::Function {
                    name: "test".to_string(),
                    attributes: vec![],
                    body: None,
                    ty: PrimitiveType::Function {
                        ext: false,
                        ret_ty: box PrimitiveType::Unit,
                        params: vec![PrimitiveType::Str, PrimitiveType::Int(IntType::I32)],
                    }
                }),
                AST::Expr(Expression::Function {
                    name: "main".to_string(),
                    attributes: attr(vec!["entry"]),
                    body: Some(box Expression::Block(vec![
                        Expression::Call {
                            name: "puts".to_string(),
                            args: vec![AtomKind::StringLiteral("Hello World".to_string())],
                        },
                        Expression::Call {
                            name: "puts".to_string(),
                            args: vec![AtomKind::StringLiteral("Hallo".to_string())],
                        },
                        Expression::Call {
                            name: "test".to_string(),
                            args: vec![
                                AtomKind::StringLiteral("Test".to_string()),
                                AtomKind::Expr(box Expression::Binary(
                                    box Expression::Atom(AtomKind::Integer(1)),
                                    String::from("+"),
                                    box Expression::Atom(AtomKind::Integer(2)),
                                ))
                            ]
                        }
                    ])),
                    ty: PrimitiveType::Function {
                        ext: false,
                        ret_ty: box PrimitiveType::Unit,
                        params: vec![PrimitiveType::Unit],
                    }
                }),
            ])
        );
    }
}
