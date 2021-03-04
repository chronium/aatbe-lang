use parser::ast::{AtomKind, Expression, PrimitiveType};

use crate::codegen::{unit::function::find_function, AatbeModule};

pub fn infer_type(module: &AatbeModule, expr: &Expression) -> Option<(PrimitiveType, bool)> {
    match expr {
        Expression::Atom(atom) => infer_atom(module, &atom),
        /*Expression::Call {
            name,
            args: call_args,
            ..
        } => {
            let args = call_args
                .iter()
                .filter_map(|e| infer_type(module, e))
                .map(|ty| ty.0)
                .collect::<Vec<_>>();

            if args.len() != call_args.len() {
                return None;
            }

            let group = module.get_func_group(name)?;
            let func = find_function(group, &args)?;

            return Some((func.ret_ty().clone(), false));
        }*/
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

pub fn infer_atom(module: &AatbeModule, atom: &AtomKind) -> Option<(PrimitiveType, bool)> {
    match atom {
        AtomKind::Integer(_, sz) => Some((sz.clone(), true)),
        AtomKind::Unit => Some((PrimitiveType::Unit, true)),
        AtomKind::StringLiteral(_) => Some((PrimitiveType::Str, true)),
        AtomKind::CharLiteral(_) => Some((PrimitiveType::Char, true)),
        AtomKind::Bool(_) => Some((PrimitiveType::Bool, true)),
        AtomKind::SymbolLiteral(s) => Some((PrimitiveType::Symbol(s.clone()), true)),
        AtomKind::Array(vals) => {
            let fst = vals.first().and_then(|v| infer_type(module, v));
            if !vals.iter().all(|v| infer_type(module, v) == fst) {
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
        /*AtomKind::Ident(name) => {
            let var = module.get_var(name)?;

            Some((var.var_ty().clone(), false))
        }*/
        AtomKind::Cast(_, ty) => Some((ty.clone(), false)),
        AtomKind::Parenthesized(box expr) => infer_type(module, expr),
        _ => unimplemented!("{:?}", atom),
    }
}
