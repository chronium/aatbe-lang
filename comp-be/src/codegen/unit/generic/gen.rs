use std::collections::HashMap;

use parser::ast::{Expression, FunctionType, IdentPath, PrimitiveType, AST};

use super::Processor;

use guard::guard;

pub fn gen(ast: &AST, proc: &Processor) -> Option<AST> {
    match ast {
        AST::Module(..) => {
            // TODO: Module
            None
        }
        AST::File(body) => Some(AST::File(
            body.iter()
                .filter_map(|ast| gen_tree(ast, proc))
                .flatten()
                .collect::<Vec<_>>(),
        )),
        _ => todo!("{:?}", ast),
    }
}

fn gen_tree(ast: &AST, proc: &Processor) -> Option<Vec<AST>> {
    match ast {
        AST::Module(..) => {
            // TODO: Module
            None
        }
        AST::Expr(expr) => gen_expr(expr, proc).and_then(|exprs| {
            Some(
                exprs
                    .iter()
                    .map(|expr| AST::Expr(expr.clone()))
                    .collect::<Vec<_>>(),
            )
        }),
        _ => todo!("{:?}", ast),
    }
}

fn gen_expr(expr: &Expression, proc: &Processor) -> Option<Vec<Expression>> {
    match expr {
        Expression::Function {
            type_names,
            name,
            ty:
                FunctionType {
                    ext,
                    ret_ty,
                    params,
                },
            body,
            attributes,
            public,
        } => {
            guard!(let Some(types) = proc
                    .function_calls
                    .get(&IdentPath::Local(name.clone())) else { return None; });

            Some(
                types
                    .iter()
                    .map(|set| {
                        let map = type_names.iter().zip(set).collect::<HashMap<_, _>>();

                        Expression::Function {
                            type_names: vec![],
                            name: name.clone(),
                            ty: FunctionType {
                                ext: *ext,
                                ret_ty: box resolve_type(ret_ty, &map),
                                params: params
                                    .iter()
                                    .map(|ty| resolve_type(ty, &map))
                                    .collect::<Vec<_>>(),
                            },
                            body: body.clone(),
                            attributes: attributes.clone(),
                            public: *public,
                        }
                    })
                    .collect(),
            )
        }
        _ => todo!("{:?}", expr),
    }
}

fn resolve_type(ty: &PrimitiveType, map: &HashMap<&String, &PrimitiveType>) -> PrimitiveType {
    match ty {
        PrimitiveType::NamedType {
            name,
            ty: Some(box ty),
        } => PrimitiveType::NamedType {
            name: name.clone(),
            ty: Some(box resolve_type(ty, map)),
        },
        PrimitiveType::TypeRef(name) => map.get(name).cloned().unwrap().clone(),
        _ => ty.clone(),
    }
}
