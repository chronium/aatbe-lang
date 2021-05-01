use std::borrow::Borrow;

use parser::ast::{AtomKind, Expression, IdentPath, Type};

use llvm_sys_wrapper::LLVMValueRef;

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

    guard!(let Some((call_types, mut call_args)) = compute_arguments(args, ctx) else {
        trace!("Got error");
        return None
    });

    let prefix = |path: &IdentPath| -> Vec<String> {
        match path {
            IdentPath::Local(name) => prefix!(call ctx, name.clone()),
            IdentPath::Module(name) => prefix!(call module ctx, name.clone()),
            IdentPath::Root(name) => name.clone(),
        }
    };

    find_function(
        match ctx.query(Query::FunctionGroup(prefix(name).join("::"))) {
            QueryResponse::FunctionGroup(Some(group)) => group,
            QueryResponse::FunctionGroup(None) => todo!("no function found"),
            _ => unreachable!(),
        },
        &call_types,
    )
    .map(|func| base::call(ctx, func.upgrade().expect("ICE").borrow(), &mut call_args))
}

fn compute_arguments(
    args: &Vec<Expression>,
    ctx: &CompilerContext,
) -> Option<(Vec<Type>, Vec<LLVMValueRef>)> {
    let mut call_types = vec![];
    let mut error = false;

    let call_args = args
        .iter()
        .filter_map(|arg| match arg {
            Expression::Atom(AtomKind::SymbolLiteral(sym)) => {
                call_types.push(Type::Symbol(sym.clone()));
                None
            }
            Expression::Atom(AtomKind::Unit) => {
                call_types.push(Type::Unit);
                None
            }
            _ => {
                let expr = expr::cg(&arg, ctx);

                if expr.is_none() {
                    error = true;

                    None
                } else {
                    expr.map_or(None, |arg| match arg.prim().clone() {
                        ref ty @ Type::VariantType(ref _name) => todo!("{:?}", ty),
                        ref ty @ Type::Array { .. } => todo!("{:?}", ty),
                        ref ty @ Type::Ref(box Type::Array { .. }) => {
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
        None
    } else {
        Some((call_types, call_args))
    }
}
