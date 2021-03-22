#[cfg(test)]
mod parser_tests {
    use crate::{
        ast::{AtomKind, BindType, Boolean, FunctionType, IdentPath, IntSize, LValue, TypeKind},
        lexer::{token::Token, Lexer},
        Expression, ParseError, Parser, PrimitiveType, AST,
    };

    fn tt(code: &'static str) -> Vec<Token> {
        let mut lexer = Lexer::new(code);
        lexer.lex()
    }

    fn attr(attribs: Vec<&'static str>) -> Vec<String> {
        attribs.iter().map(|attr| attr.to_string()).collect()
    }

    macro_rules! parse_test {
        ($tt:expr, $name:expr) => {{
            let mut parser = Parser::new(tt($tt));
            let res = parser.parse();

            assert!(res.is_ok());

            res.expect("")
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

        assert!(res.is_ok());
    }

    #[test]
    fn unit_function() {
        let pt = parse_test!("public fn test () -> ()", "Unit Function");

        assert_eq!(
            pt,
            AST::File(vec![AST::Expr(Expression::Function {
                name: "test".to_string(),
                attributes: vec![],
                body: None,
                type_names: vec![],
                public: true,
                ty: FunctionType {
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
                attributes: vec![],
                body: None,
                type_names: vec![],
                public: false,
                ty: FunctionType {
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
                public: false,
                type_names: vec![],
                ty: FunctionType {
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
                type_names: vec![],
                public: false,
                body: Some(box Expression::Atom(AtomKind::Unit)),
                ty: FunctionType {
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
                type_names: vec![],
                public: false,
                body: Some(box Expression::Binary(
                    box Expression::Binary(
                        box Expression::Binary(
                            box Expression::Atom(AtomKind::Integer(
                                1,
                                PrimitiveType::Int(IntSize::Bits32)
                            )),
                            "+".to_string(),
                            box Expression::Binary(
                                box Expression::Atom(AtomKind::Integer(
                                    2,
                                    PrimitiveType::Int(IntSize::Bits32)
                                )),
                                "*".to_string(),
                                box Expression::Atom(AtomKind::Integer(
                                    3,
                                    PrimitiveType::Int(IntSize::Bits32)
                                )),
                            ),
                        ),
                        "+".to_string(),
                        box Expression::Atom(AtomKind::Integer(
                            4,
                            PrimitiveType::Int(IntSize::Bits32)
                        )),
                    ),
                    "||".to_string(),
                    box Expression::Binary(
                        box Expression::Binary(
                            box Expression::Atom(AtomKind::Integer(
                                1,
                                PrimitiveType::Int(IntSize::Bits32)
                            )),
                            "==".to_string(),
                            box Expression::Atom(AtomKind::Integer(
                                2,
                                PrimitiveType::Int(IntSize::Bits32)
                            )),
                        ),
                        "&".to_string(),
                        box Expression::Atom(AtomKind::Unary(
                            "-".to_string(),
                            box AtomKind::Ident("foo".to_string()),
                        )),
                    ),
                ),),
                ty: FunctionType {
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
                type_names: vec![],
                public: false,
                ty: FunctionType {
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
                type_names: vec![],
                public: false,
                body: Some(box Expression::Block(vec![Expression::Atom(
                    AtomKind::Unit
                )])),
                ty: FunctionType {
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
    test \"Test\", 1 + 2
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
                    type_names: vec![],
                    public: false,
                    ty: FunctionType {
                        ext: true,
                        ret_ty: box PrimitiveType::Int(IntSize::Bits32),
                        params: vec![PrimitiveType::Str],
                    }
                }),
                AST::Expr(Expression::Function {
                    name: "test".to_string(),
                    attributes: vec![],
                    body: None,
                    type_names: vec![],
                    public: false,
                    ty: FunctionType {
                        ext: false,
                        ret_ty: box PrimitiveType::Unit,
                        params: vec![
                            PrimitiveType::NamedType {
                                name: "s".to_string(),
                                ty: Some(box PrimitiveType::Str)
                            },
                            PrimitiveType::Int(IntSize::Bits32),
                            PrimitiveType::Varargs,
                        ],
                    }
                }),
                AST::Expr(Expression::Function {
                    name: "main".to_string(),
                    attributes: attr(vec!["entry"]),
                    type_names: vec![],
                    public: false,
                    body: Some(box Expression::Block(vec![
                        Expression::Call {
                            name: IdentPath::Local("puts".to_string()),
                            types: vec![],
                            args: vec![Expression::Atom(AtomKind::StringLiteral(
                                "Hello World".to_string()
                            ))],
                        },
                        Expression::Call {
                            name: IdentPath::Local("puts".to_string()),
                            types: vec![],
                            args: vec![Expression::Atom(AtomKind::StringLiteral(
                                "Hallo".to_string()
                            ))],
                        },
                        Expression::Call {
                            name: IdentPath::Local("test".to_string()),
                            types: vec![],
                            args: vec![
                                Expression::Atom(AtomKind::StringLiteral("Test".to_string())),
                                Expression::Binary(
                                    box Expression::Atom(AtomKind::Integer(
                                        1,
                                        PrimitiveType::Int(IntSize::Bits32)
                                    )),
                                    "+".to_string(),
                                    box Expression::Atom(AtomKind::Integer(
                                        2,
                                        PrimitiveType::Int(IntSize::Bits32)
                                    )),
                                )
                            ]
                        }
                    ])),
                    ty: FunctionType {
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
    val infer
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
                type_names: vec![],
                public: false,
                body: Some(box Expression::Block(vec![
                    Expression::Decl {
                        ty: PrimitiveType::NamedType {
                            name: "var_t".to_string(),
                            ty: Some(box PrimitiveType::Str),
                        },
                        value: Some(box Expression::Atom(AtomKind::StringLiteral(
                            "Hello World".to_string()
                        ))),
                        exterior_bind: BindType::Mutable,
                    },
                    Expression::Decl {
                        ty: PrimitiveType::NamedType {
                            name: "val_t".to_string(),
                            ty: Some(box PrimitiveType::Str),
                        },
                        value: None,
                        exterior_bind: BindType::Immutable,
                    },
                    Expression::Decl {
                        ty: PrimitiveType::NamedType {
                            name: "infer".to_string(),
                            ty: None,
                        },
                        value: None,
                        exterior_bind: BindType::Immutable,
                    },
                    Expression::Assign {
                        lval: LValue::Ident("var_t".to_string()),
                        value: box Expression::Atom(AtomKind::StringLiteral(
                            "Aloha honua".to_string()
                        )),
                    }
                ])),
                ty: FunctionType {
                    ext: false,
                    ret_ty: box PrimitiveType::Unit,
                    params: vec![PrimitiveType::Unit],
                }
            }),])
        );
    }

    #[test]
    fn if_else_if() {
        let pt = parse_test!(
            "
@entry
fn main () = {
    if true then false
    else if false then true
    else if !false then false
    else false
}
",
            "If Else If chain"
        );

        assert_eq!(
            pt,
            AST::File(vec![AST::Expr(Expression::Function {
                name: "main".to_string(),
                attributes: attr(vec!["entry"]),
                type_names: vec![],
                public: false,
                body: Some(box Expression::Block(vec![Expression::If {
                    cond_expr: box Expression::Atom(AtomKind::Bool(Boolean::True)),
                    then_expr: box Expression::Atom(AtomKind::Bool(Boolean::False)),
                    elseif_exprs: vec![
                        (
                            Expression::Atom(AtomKind::Bool(Boolean::False)),
                            Expression::Atom(AtomKind::Bool(Boolean::True))
                        ),
                        (
                            Expression::Atom(AtomKind::Unary(
                                "!".to_string(),
                                box AtomKind::Bool(Boolean::False)
                            ),),
                            Expression::Atom(AtomKind::Bool(Boolean::False))
                        )
                    ],
                    else_expr: Some(box Expression::Atom(AtomKind::Bool(Boolean::False))),
                }])),
                ty: FunctionType {
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
                type_names: vec![],
                public: false,
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
                            )),
                        ),
                        then_expr: box Expression::Block(vec![Expression::Call {
                            name: IdentPath::Local("foo".to_string()),
                            types: vec![],
                            args: vec![Expression::Atom(AtomKind::StringLiteral(
                                "bar".to_string()
                            ))]
                        }]),
                        elseif_exprs: vec![],
                        else_expr: Some(box Expression::Block(vec![Expression::Call {
                            name: IdentPath::Local("bar".to_string()),
                            types: vec![],
                            args: vec![Expression::Atom(AtomKind::StringLiteral(
                                "foo".to_string()
                            ))]
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
                            name: IdentPath::Local("baz".to_string()),
                            types: vec![],
                            args: vec![Expression::Atom(AtomKind::Bool(Boolean::True))]
                        },
                        elseif_exprs: vec![],
                        else_expr: None,
                    },
                    Expression::If {
                        cond_expr: box Expression::Atom(AtomKind::Unary(
                            "!".to_string(),
                            box AtomKind::Bool(Boolean::True)
                        )),
                        then_expr: box Expression::Atom(AtomKind::Bool(Boolean::False)),
                        elseif_exprs: vec![],
                        else_expr: Some(box Expression::Atom(AtomKind::Bool(Boolean::True))),
                    },
                ])),
                ty: FunctionType {
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
                AST::Import("lib.aat".to_string()),
                AST::Expr(Expression::Function {
                    name: "main".to_string(),
                    attributes: attr(vec!["entry"]),
                    body: None,
                    type_names: vec![],
                    public: false,
                    ty: FunctionType {
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
rec Generic[T](value: T)
rec Unit()
",
            "Declare record"
        );

        assert_eq!(
            pt,
            AST::File(vec![
                AST::Record(
                    "Record".to_string(),
                    None,
                    vec![
                        PrimitiveType::NamedType {
                            name: "msg".to_string(),
                            ty: Some(box PrimitiveType::Str),
                        },
                        PrimitiveType::NamedType {
                            name: "time".to_string(),
                            ty: Some(box PrimitiveType::Int(IntSize::Bits64)),
                        }
                    ]
                ),
                AST::Record(
                    "Generic".to_string(),
                    Some(vec!["T".to_string()]),
                    vec![PrimitiveType::NamedType {
                        name: "value".to_string(),
                        ty: Some(box PrimitiveType::TypeRef("T".to_string())),
                    },]
                ),
                AST::Record("Unit".to_string(), None, vec![PrimitiveType::Unit],)
            ])
        );
    }

    #[test]
    fn init_record() {
        let pt = parse_test!(
            "
rec Record(msg: str, time: i32)
rec Generic[T](value: T)

fn rec_test () -> () = Record { msg: \"Hello World\", time: 42, a: a.b }
fn generic_test () = Generic[str] { value: \"Aloha\" }
",
            "Initialize record"
        );

        assert_eq!(
            pt,
            AST::File(vec![
                AST::Record(
                    "Record".to_string(),
                    None,
                    vec![
                        PrimitiveType::NamedType {
                            name: "msg".to_string(),
                            ty: Some(box PrimitiveType::Str),
                        },
                        PrimitiveType::NamedType {
                            name: "time".to_string(),
                            ty: Some(box PrimitiveType::Int(IntSize::Bits32)),
                        }
                    ]
                ),
                AST::Record(
                    "Generic".to_string(),
                    Some(vec!["T".to_string()]),
                    vec![PrimitiveType::NamedType {
                        name: "value".to_string(),
                        ty: Some(box PrimitiveType::TypeRef("T".to_string())),
                    },]
                ),
                AST::Expr(Expression::Function {
                    name: "rec_test".to_string(),
                    attributes: vec![],
                    type_names: vec![],
                    public: false,
                    body: Some(box Expression::RecordInit {
                        record: "Record".to_string(),
                        types: vec![],
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
                    }),
                    ty: FunctionType {
                        ext: false,
                        ret_ty: box PrimitiveType::Unit,
                        params: vec![PrimitiveType::Unit],
                    }
                }),
                AST::Expr(Expression::Function {
                    name: "generic_test".to_string(),
                    attributes: vec![],
                    type_names: vec![],
                    public: false,
                    body: Some(box Expression::RecordInit {
                        record: "Generic".to_string(),
                        types: vec![PrimitiveType::Str],
                        values: vec![AtomKind::NamedValue {
                            name: "value".to_string(),
                            val: box Expression::Atom(AtomKind::StringLiteral("Aloha".to_string()))
                        },],
                    }),
                    ty: FunctionType {
                        ext: false,
                        ret_ty: box PrimitiveType::Unit,
                        params: vec![PrimitiveType::Unit],
                    }
                }),
            ])
        );
    }

    #[test]
    fn generic_record_function() {
        let pt = parse_test!(
            "
rec Generic[T](value: T)

fn generic_record_test[T] Generic[T]
fn generic_test[T] value: T
",
            "Declare generic record and type"
        );

        assert_eq!(
            pt,
            AST::File(vec![
                AST::Record(
                    "Generic".to_string(),
                    Some(vec!["T".to_string()]),
                    vec![PrimitiveType::NamedType {
                        name: "value".to_string(),
                        ty: Some(box PrimitiveType::TypeRef("T".to_string())),
                    },]
                ),
                AST::Expr(Expression::Function {
                    name: "generic_record_test".to_string(),
                    attributes: vec![],
                    type_names: vec!["T".to_string()],
                    body: None,
                    public: false,
                    ty: FunctionType {
                        ext: false,
                        ret_ty: box PrimitiveType::Unit,
                        params: vec![PrimitiveType::GenericTypeRef(
                            "Generic".to_string(),
                            vec![PrimitiveType::TypeRef("T".to_string())]
                        )],
                    }
                }),
                AST::Expr(Expression::Function {
                    name: "generic_test".to_string(),
                    attributes: vec![],
                    type_names: vec!["T".to_string()],
                    body: None,
                    public: false,
                    ty: FunctionType {
                        ext: false,
                        ret_ty: box PrimitiveType::Unit,
                        params: vec![PrimitiveType::NamedType {
                            name: "value".to_string(),
                            ty: Some(box PrimitiveType::TypeRef("T".to_string()))
                        }],
                    }
                }),
            ])
        );
    }

    #[test]
    fn generic_funcall() {
        let pt = parse_test!(
            "
fn generic_test[T] value: T
fn test () = generic_test[i32] 64
",
            "Initialize generic record"
        );

        assert_eq!(
            pt,
            AST::File(vec![
                AST::Expr(Expression::Function {
                    name: "generic_test".to_string(),
                    attributes: vec![],
                    type_names: vec!["T".to_string()],
                    body: None,
                    public: false,
                    ty: FunctionType {
                        ext: false,
                        ret_ty: box PrimitiveType::Unit,
                        params: vec![PrimitiveType::NamedType {
                            name: "value".to_string(),
                            ty: Some(box PrimitiveType::TypeRef("T".to_string()))
                        }],
                    }
                }),
                AST::Expr(Expression::Function {
                    name: "test".to_string(),
                    attributes: vec![],
                    type_names: vec![],
                    public: false,
                    body: Some(box Expression::Call {
                        name: IdentPath::Local("generic_test".to_string()),
                        types: vec![PrimitiveType::Int(IntSize::Bits32)],
                        args: vec![Expression::Atom(AtomKind::Integer(
                            64,
                            PrimitiveType::Int(IntSize::Bits32)
                        ))]
                    }),
                    ty: FunctionType {
                        ext: false,
                        ret_ty: box PrimitiveType::Unit,
                        params: vec![PrimitiveType::Unit],
                    }
                }),
            ])
        );
    }

    #[test]
    fn typedefs() {
        let pt = parse_test!(
            "
type Opaque
type Generic[T]
type Newtype = u32
type Option[T] = None | Some T
type Number = u8 | u16
type Complex = u8 | u16 | Comp @str
",
            "Typedef tests"
        );

        assert_eq!(
            pt,
            AST::File(vec![
                AST::Typedef {
                    name: "Opaque".to_string(),
                    type_names: None,
                    variants: None,
                },
                AST::Typedef {
                    name: "Generic".to_string(),
                    type_names: Some(vec!["T".to_string()]),
                    variants: None,
                },
                AST::Typedef {
                    name: "Newtype".to_string(),
                    type_names: None,
                    variants: Some(vec![TypeKind::Newtype(PrimitiveType::UInt(
                        IntSize::Bits32
                    ))]),
                },
                AST::Typedef {
                    name: "Option".to_string(),
                    type_names: Some(vec!["T".to_string()]),
                    variants: Some(vec![
                        TypeKind::Variant("None".to_string(), None),
                        TypeKind::Variant(
                            "Some".to_string(),
                            Some(vec![PrimitiveType::TypeRef("T".to_string())])
                        )
                    ]),
                },
                AST::Typedef {
                    name: "Number".to_string(),
                    type_names: None,
                    variants: Some(vec![
                        TypeKind::Newtype(PrimitiveType::UInt(IntSize::Bits8)),
                        TypeKind::Newtype(PrimitiveType::UInt(IntSize::Bits16)),
                    ]),
                },
                AST::Typedef {
                    name: "Complex".to_string(),
                    type_names: None,
                    variants: Some(vec![
                        TypeKind::Newtype(PrimitiveType::UInt(IntSize::Bits8)),
                        TypeKind::Newtype(PrimitiveType::UInt(IntSize::Bits16)),
                        TypeKind::Variant(
                            "Comp".to_string(),
                            Some(vec![PrimitiveType::Box(box PrimitiveType::Str)])
                        )
                    ]),
                },
            ])
        );
    }

    #[test]
    fn empty_module() {
        let pt = parse_test!(
            "
module mod {

}",
            "Parse Empty Module"
        );

        assert_eq!(
            pt,
            AST::File(vec![AST::Module("mod".to_string(), box AST::File(vec![]))])
        );
    }

    #[test]
    fn module() {
        let pt = parse_test!(
            "
module mod {

    type Opaque

}",
            "Parse Module"
        );

        assert_eq!(
            pt,
            AST::File(vec![AST::Module(
                "mod".to_string(),
                box AST::File(vec![AST::Typedef {
                    name: "Opaque".to_string(),
                    type_names: None,
                    variants: None,
                }])
            )])
        );
    }

    #[test]
    fn module_path() {
        let pt = parse_test!(
            "
type Newtype = mod::test::next
",
            "Module Typedef tests"
        );

        assert_eq!(
            pt,
            AST::File(vec![AST::Typedef {
                name: "Newtype".to_string(),
                type_names: None,
                variants: Some(vec![TypeKind::Newtype(PrimitiveType::Path(vec![
                    "mod".to_string(),
                    "test".to_string(),
                    "next".to_string(),
                ]))]),
            },])
        );
    }

    #[test]
    fn module_call() {
        let pt = parse_test!(
            "
@entry
fn main () = {
    func ()
    mod::func ()
    ::func ()
}
",
            "Module call tests"
        );

        assert_eq!(
            pt,
            AST::File(vec![AST::Expr(Expression::Function {
                name: "main".to_string(),
                attributes: attr(vec!["entry"]),
                type_names: vec![],
                public: false,
                body: Some(box Expression::Block(vec![
                    Expression::Call {
                        name: IdentPath::Local("func".to_string()),
                        types: vec![],
                        args: vec![Expression::Atom(AtomKind::Unit)]
                    },
                    Expression::Call {
                        name: IdentPath::Module(vec!["mod".to_string(), "func".to_string()]),
                        types: vec![],
                        args: vec![Expression::Atom(AtomKind::Unit)]
                    },
                    Expression::Call {
                        name: IdentPath::Root(vec!["func".to_string()]),
                        types: vec![],
                        args: vec![Expression::Atom(AtomKind::Unit)]
                    }
                ])),
                ty: FunctionType {
                    ext: false,
                    ret_ty: box PrimitiveType::Unit,
                    params: vec![PrimitiveType::Unit],
                }
            })])
        );
    }
}
