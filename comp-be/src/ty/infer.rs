use parser::ast::{AtomKind, Expression, IdentPath, PrimitiveType};

use guard::guard;

use crate::{
    codegen::unit::{function::find_function, CompilerContext, Query, QueryResponse},
    prefix,
};

pub fn infer_type(ctx: &CompilerContext, expr: &Expression) -> Option<(PrimitiveType, bool)> {
    match expr {
        Expression::Atom(atom) => infer_atom(ctx, &atom),
        Expression::Call {
            name,
            args: call_args,
            ..
        } => {
            let args = call_args
                .iter()
                .filter_map(|e| infer_type(ctx, e))
                .map(|ty| ty.0)
                .collect::<Vec<_>>();

            if args.len() != call_args.len() {
                return None;
            }

            let prefix = |path: &IdentPath| -> Vec<String> {
                match path {
                    IdentPath::Local(name) => prefix!(call ctx, name.clone()),
                    IdentPath::Module(name) => prefix!(call module ctx, name.clone()),
                    _ => todo!(),
                }
            };

            find_function(
                match ctx.query(Query::FunctionGroup(prefix(name).join("::"))) {
                    QueryResponse::FunctionGroup(Some(group)) => group,
                    QueryResponse::FunctionGroup(None) => todo!("no function found"),
                    _ => unreachable!(),
                },
                &args,
            )
            .map(|func| Some((func.upgrade().expect("ICE").ret_ty().clone(), false)))
            .flatten()
        }
        Expression::RecordInit { record, types, .. } if types.len() == 0 => {
            Some((PrimitiveType::TypeRef(record.clone()), true))
        }
        Expression::RecordInit { record, types, .. } => Some((
            PrimitiveType::GenericTypeRef(record.clone(), types.clone()),
            true,
        )),
        _ => unimplemented!("{:?}", expr),
    }
}

pub fn infer_atom(ctx: &CompilerContext, atom: &AtomKind) -> Option<(PrimitiveType, bool)> {
    match atom {
        AtomKind::Integer(_, sz) => Some((sz.clone(), true)),
        AtomKind::Unit => Some((PrimitiveType::Unit, true)),
        AtomKind::StringLiteral(_) => Some((PrimitiveType::Str, true)),
        AtomKind::CharLiteral(_) => Some((PrimitiveType::Char, true)),
        AtomKind::Bool(_) => Some((PrimitiveType::Bool, true)),
        AtomKind::SymbolLiteral(s) => Some((PrimitiveType::Symbol(s.clone()), true)),
        AtomKind::Array(vals) => {
            let fst = vals.first().and_then(|v| infer_type(ctx, v));
            if !vals.iter().all(|v| infer_type(ctx, v) == fst) {
                None
            } else if let Some((ty, constant)) = fst {
                Some((
                    PrimitiveType::Array {
                        ty: box ty,
                        len: vals.len() as u32,
                    },
                    constant,
                ))
            } else {
                None
            }
        }
        AtomKind::Ident(name) => {
            guard!(let QueryResponse::Slot(slot) = ctx.query(Query::Slot(name)) else { unreachable!() });
            let slot = slot?;
            Some((slot.var_ty().clone(), false).into())
        }
        AtomKind::Cast(_, ty) => Some((ty.clone(), false)),
        AtomKind::Parenthesized(box expr) => infer_type(ctx, expr),
        _ => unimplemented!("{:?}", atom),
    }
}
