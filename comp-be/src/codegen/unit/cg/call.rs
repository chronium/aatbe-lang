use std::borrow::Borrow;

use parser::ast::{AtomKind, Expression, PrimitiveType};

use crate::codegen::{
    builder::core,
    unit::{cg::expr, function::find_function, ModuleContext, Query, QueryResponse},
    ValueTypePair,
};

use log::*;

pub fn cg(expr: &Expression, ctx: &ModuleContext) -> Option<ValueTypePair> {
    if let Expression::Call {
        name,
        types: _,
        args,
    } = expr
    {
        trace!("Call {}", name);
        let mut call_types = vec![];

        let mut error = false;
        let mut call_args = args
            .iter()
            .filter_map(|arg| match arg {
                Expression::Atom(AtomKind::SymbolLiteral(sym)) => {
                    call_types.push(PrimitiveType::Symbol(sym.clone()));
                    None
                }
                Expression::Atom(AtomKind::Unit) => {
                    call_types.push(PrimitiveType::Unit);
                    None
                }
                _ => {
                    let expr = expr::cg(arg, ctx);

                    if expr.is_none() {
                        error = true;

                        None
                    } else {
                        expr.map_or(None, |arg| match arg.prim().clone() {
                            ref ty @ PrimitiveType::VariantType(ref _name) => todo!("{:?}", ty),
                            ref ty @ PrimitiveType::Array { .. } => todo!("{:?}", ty),
                            ref ty @ PrimitiveType::Ref(box PrimitiveType::Array { .. }) => {
                                todo!("{:?}", ty)
                            }
                            ref ty @ _ => {
                                call_types.push(ty.clone());
                                Some(*arg)
                            }
                        })
                    }
                }
            })
            .collect::<Vec<_>>();

        if error {
            trace!("Got error");
            return None;
        }

        if let QueryResponse::FunctionGroup(Some(group)) = ctx.query(Query::FunctionGroup(name)) {
            if let Some(func) = find_function(group, &call_types) {
                Some(core::call(
                    ctx,
                    func.upgrade().expect("ICE").borrow(),
                    &mut call_args,
                ))
            } else {
                todo!();
            }
        } else {
            todo!();
        }
    } else {
        unreachable!()
    }
}
