use parser::ast::{AtomKind, Expression, PrimitiveType};

use crate::codegen::{unit::function::find_function, AatbeModule};

pub fn infer_type(module: &AatbeModule, expr: &Expression) -> Option<PrimitiveType> {
    match expr {
        Expression::Atom(atom) => infer_atom(module, &atom),
        Expression::Call {
            name,
            args: call_args,
            ..
        } => {
            let args = call_args
                .iter()
                .filter_map(|a| infer_type(module, a))
                .collect::<Vec<_>>();

            if args.len() != call_args.len() {
                return None;
            }

            let group = module.get_func_group(name)?;
            let func = find_function(group, &args)?;

            return Some(func.ret_ty().clone());
        }
        Expression::RecordInit { record, types, .. } if types.len() == 0 => {
            Some(PrimitiveType::TypeRef(record.clone()))
        }
        Expression::RecordInit { record, types, .. } => {
            Some(PrimitiveType::GenericTypeRef(record.clone(), types.clone()))
        }
        _ => unimplemented!("{:?}", expr),
    }
}

pub fn infer_atom(module: &AatbeModule, atom: &AtomKind) -> Option<PrimitiveType> {
    match atom {
        AtomKind::Integer(_, sz) => Some(sz.clone()),
        AtomKind::Unit => Some(PrimitiveType::Unit),
        AtomKind::StringLiteral(_) => Some(PrimitiveType::Str),
        AtomKind::CharLiteral(_) => Some(PrimitiveType::Char),
        AtomKind::SymbolLiteral(s) => Some(PrimitiveType::Symbol(s.clone())),
        AtomKind::Array(vals) => {
            let fst = vals.first().and_then(|v| infer_type(module, v));
            if !vals.iter().all(|v| infer_type(module, v) == fst) {
                None
            } else if let Some(ty) = fst {
                Some(PrimitiveType::Array {
                    ty: box ty,
                    len: vals.len() as u32,
                })
            } else {
                None
            }
        }
        _ => unimplemented!("{:?}", atom),
    }
}
