#[cfg(test)]
mod parser_tests {
    use crate::{
        ast::{AtomKind, BindType, Boolean, IntSize, LValue},
        lexer::{token::Token, Lexer},
        Expression, ParseError, Parser, PrimitiveType, AST,
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
fn main () -> () = 1 + 2 * 3 + 4 || 1 == 2 & -foo
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
                        box Expression::Binary(
                            box Expression::Atom(AtomKind::Integer(
                                1,
                                PrimitiveType::Int(IntSize::Bits32)
                            )),
                            String::from("+"),
                            box Expression::Binary(
                                box Expression::Atom(AtomKind::Integer(
                                    2,
                                    PrimitiveType::Int(IntSize::Bits32)
                                )),
                                String::from("*"),
                                box Expression::Atom(AtomKind::Integer(
                                    3,
                                    PrimitiveType::Int(IntSize::Bits32)
                                )),
                            ),
                        ),
                        String::from("+"),
                        box Expression::Atom(AtomKind::Integer(
                            4,
                            PrimitiveType::Int(IntSize::Bits32)
                        )),
                    ),
                    String::from("||"),
                    box Expression::Binary(
                        box Expression::Binary(
                            box Expression::Atom(AtomKind::Integer(
                                1,
                                PrimitiveType::Int(IntSize::Bits32)
                            )),
                            String::from("=="),
                            box Expression::Atom(AtomKind::Integer(
                                2,
                                PrimitiveType::Int(IntSize::Bits32)
                            )),
                        ),
                        String::from("&"),
                        box Expression::Atom(AtomKind::Unary(
                            "-".to_string(),
                            box AtomKind::Ident("foo".to_string()),
                        )),
                    ),
                ),),
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

fn test s: str, i32, ... -> ()

@entry
fn main () -> () = {
    puts \"Hello World\"
    puts \"Hallo\"
    test \"Test\" $ 1 + 2
}
",
            "Function calls"
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
                        ret_ty: box PrimitiveType::Int(IntSize::Bits32),
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
                        params: vec![
                            PrimitiveType::NamedType {
                                name: "s".to_string(),
                                ty: box PrimitiveType::Str
                            },
                            PrimitiveType::Int(IntSize::Bits32),
                            PrimitiveType::Varargs,
                        ],
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
                                    box Expression::Atom(AtomKind::Integer(
                                        1,
                                        PrimitiveType::Int(IntSize::Bits32)
                                    )),
                                    String::from("+"),
                                    box Expression::Atom(AtomKind::Integer(
                                        2,
                                        PrimitiveType::Int(IntSize::Bits32)
                                    )),
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

    #[test]
    fn variable_decl_assign() {
        let pt = parse_test!(
            "
@entry
fn main () -> () = {
    var var_t: str = \"Hello World\"
    val val_t: str
    var_t = \"Aloha honua\"
}
",
            "Variable declaration and assignment"
        );

        assert_eq!(
            pt,
            AST::File(vec![AST::Expr(Expression::Function {
                name: "main".to_string(),
                attributes: attr(vec!["entry"]),
                body: Some(box Expression::Block(vec![
                    Expression::Decl {
                        ty: PrimitiveType::NamedType {
                            name: "var_t".to_string(),
                            ty: box PrimitiveType::Str,
                        },
                        value: Some(box Expression::Atom(AtomKind::StringLiteral(String::from(
                            "Hello World"
                        )))),
                        exterior_bind: BindType::Mutable,
                    },
                    Expression::Decl {
                        ty: PrimitiveType::NamedType {
                            name: "val_t".to_string(),
                            ty: box PrimitiveType::Str,
                        },
                        value: None,
                        exterior_bind: BindType::Immutable,
                    },
                    Expression::Assign {
                        lval: LValue::Ident("var_t".to_string()),
                        value: box Expression::Atom(AtomKind::StringLiteral(String::from(
                            "Aloha honua"
                        ))),
                    }
                ])),
                ty: PrimitiveType::Function {
                    ext: false,
                    ret_ty: box PrimitiveType::Unit,
                    params: vec![PrimitiveType::Unit],
                }
            }),])
        );
    }

    #[test]
    fn if_else() {
        let pt = parse_test!(
            "
@entry
fn main () -> () = {
  if 1 == 2 {
      foo \"bar\"
  } else {
      bar \"foo\"
  }

  if !(true != false) baz true
  if !true false else true
}
",
            "If Else conditions"
        );

        assert_eq!(
            pt,
            AST::File(vec![AST::Expr(Expression::Function {
                name: "main".to_string(),
                attributes: attr(vec!["entry"]),
                body: Some(box Expression::Block(vec![
                    Expression::If {
                        cond_expr: box Expression::Binary(
                            box Expression::Atom(AtomKind::Integer(
                                1,
                                PrimitiveType::Int(IntSize::Bits32)
                            )),
                            "==".to_string(),
                            box Expression::Atom(AtomKind::Integer(
                                2,
                                PrimitiveType::Int(IntSize::Bits32)
                            ))
                        ),
                        then_expr: box Expression::Block(vec![Expression::Call {
                            name: "foo".to_string(),
                            args: vec![AtomKind::StringLiteral("bar".to_string())]
                        }]),
                        else_expr: Some(box Expression::Block(vec![Expression::Call {
                            name: "bar".to_string(),
                            args: vec![AtomKind::StringLiteral("foo".to_string())]
                        }])),
                    },
                    Expression::If {
                        cond_expr: box Expression::Atom(AtomKind::Unary(
                            "!".to_string(),
                            box AtomKind::Parenthesized(box Expression::Binary(
                                box Expression::Atom(AtomKind::Bool(Boolean::True)),
                                "!=".to_string(),
                                box Expression::Atom(AtomKind::Bool(Boolean::False))
                            )),
                        )),
                        then_expr: box Expression::Call {
                            name: "baz".to_string(),
                            args: vec![AtomKind::Bool(Boolean::True)]
                        },
                        else_expr: None,
                    },
                    Expression::If {
                        cond_expr: box Expression::Atom(AtomKind::Unary(
                            "!".to_string(),
                            box AtomKind::Bool(Boolean::True)
                        )),
                        then_expr: box Expression::Atom(AtomKind::Bool(Boolean::False)),
                        else_expr: Some(box Expression::Atom(AtomKind::Bool(Boolean::True))),
                    },
                ])),
                ty: PrimitiveType::Function {
                    ext: false,
                    ret_ty: box PrimitiveType::Unit,
                    params: vec![PrimitiveType::Unit],
                }
            }),])
        );
    }

    #[test]
    fn use_path() {
        let pt = parse_test!(
            "
// test comment
use \"lib.aat\"

@entry
fn main () -> ()
",
            "Use path"
        );

        assert_eq!(
            pt,
            AST::File(vec![
                AST::Import(String::from("lib.aat")),
                AST::Expr(Expression::Function {
                    name: "main".to_string(),
                    attributes: attr(vec!["entry"]),
                    body: None,
                    ty: PrimitiveType::Function {
                        ext: false,
                        ret_ty: box PrimitiveType::Unit,
                        params: vec![PrimitiveType::Unit],
                    }
                }),
            ])
        );
    }

    #[test]
    fn record_decl() {
        let pt = parse_test!(
            "
rec Record(msg: str, time: i64)
rec Unit()
",
            "Declare record"
        );

        assert_eq!(
            pt,
            AST::File(vec![
                AST::Record(
                    "Record".to_string(),
                    vec![
                        PrimitiveType::NamedType {
                            name: "msg".to_string(),
                            ty: box PrimitiveType::Str,
                        },
                        PrimitiveType::NamedType {
                            name: "time".to_string(),
                            ty: box PrimitiveType::Int(IntSize::Bits64),
                        }
                    ]
                ),
                AST::Record("Unit".to_string(), vec![PrimitiveType::Unit],)
            ])
        );
    }

    #[test]
    fn init_record() {
        let pt = parse_test!(
            "
rec Record(msg: str, time: i32)

fn rec_test () -> () = Record { msg: \"Hello World\", time: 42, a: a.b }
",
            "Initialize record"
        );

        assert_eq!(
            pt,
            AST::File(vec![
                AST::Record(
                    "Record".to_string(),
                    vec![
                        PrimitiveType::NamedType {
                            name: "msg".to_string(),
                            ty: box PrimitiveType::Str,
                        },
                        PrimitiveType::NamedType {
                            name: "time".to_string(),
                            ty: box PrimitiveType::Int(IntSize::Bits32),
                        }
                    ]
                ),
                AST::Expr(Expression::Function {
                    name: "rec_test".to_string(),
                    attributes: vec![],
                    body: Some(box Expression::Atom(AtomKind::RecordInit {
                        record: "Record".to_string(),
                        values: vec![
                            AtomKind::NamedValue {
                                name: "msg".to_string(),
                                val: box Expression::Atom(AtomKind::StringLiteral(
                                    "Hello World".to_string()
                                ))
                            },
                            AtomKind::NamedValue {
                                name: "time".to_string(),
                                val: box Expression::Atom(AtomKind::Integer(
                                    42,
                                    PrimitiveType::Int(IntSize::Bits32)
                                ))
                            },
                            AtomKind::NamedValue {
                                name: "a".to_string(),
                                val: box Expression::Atom(AtomKind::Access(vec![
                                    "a".to_string(),
                                    "b".to_string()
                                ]))
                            }
                        ],
                    })),
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
