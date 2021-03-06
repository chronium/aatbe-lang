use parser::ast::{Expression, FunctionType};

use crate::codegen::{
    unit::{
        cg::{atom, call},
        declare_and_compile_function, ModuleContext,
    },
    ValueTypePair,
};

pub fn cg(expr: &Expression, ctx: &ModuleContext) -> Option<ValueTypePair> {
    match expr {
        Expression::Call { .. } => call::cg(expr, ctx),
        Expression::Atom(atom) => atom::cg(atom, ctx),
        Expression::Function { ty, type_names, .. } if type_names.len() == 0 => match ty {
            FunctionType {
                ret_ty: _,
                params: _,
                ext: true,
            } => None,
            _ => declare_and_compile_function(ctx, expr),
        },
        Expression::Block(body) if body.len() == 0 => None,
        _ => todo!("{:?}", expr),
    }
}
