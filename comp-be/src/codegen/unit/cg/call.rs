use std::borrow::Borrow;

use parser::ast::{AtomKind, Expression, IdentPath, PrimitiveType};

use crate::{
    codegen::{
        builder::base,
        unit::{cg::expr, function::find_function, CompilerContext, Query, QueryResponse},
        ValueTypePair,
    },
    fmt::AatbeFmt,
    prefix,
};

use guard::guard;
use log::*;

pub fn cg(expr: &Expression, ctx: &CompilerContext) -> Option<ValueTypePair> {
    guard!(let Expression::Call {
        name,
        types: _,
        args,
    } = expr else { unreachable!() });

    ctx.trace(format!("Call {}", AatbeFmt::fmt(expr)));
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
                let expr = expr::cg(&arg, ctx);

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

    let prefix = |path: &IdentPath| -> Vec<String> {
        match path {
            IdentPath::Local(name) => prefix!(call ctx, name.clone()),
            IdentPath::Module(name) => prefix!(call module ctx, name.clone()),
            _ => todo!(),
        }
    };

    find_function(
        match ctx.query(Query::FunctionGroup(prefix(name))) {
            QueryResponse::FunctionGroup(Some(group)) => group,
            QueryResponse::FunctionGroup(None) => todo!("no function found"),
            _ => unreachable!(),
        },
        &call_types,
    )
    .map(|func| base::call(ctx, func.upgrade().expect("ICE").borrow(), &mut call_args))
}
