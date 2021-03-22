use parser::ast::{Expression, FunctionType};

use crate::codegen::{
    unit::{
        cg::{assign, atom, binary, call, conditional, decl},
        declare_and_compile_function, CompilerContext, Message,
    },
    ValueTypePair,
};

pub fn cg(expr: &Expression, ctx: &CompilerContext) -> Option<ValueTypePair> {
    match expr {
        Expression::Assign { .. } => assign::cg(expr, ctx),
        Expression::Decl { .. } => decl::cg(expr, ctx),
        Expression::Loop { .. } => conditional::loops::cg(expr, ctx),
        Expression::If { .. } => conditional::ifelse::cg(expr, ctx),
        Expression::Binary(..) => Some(binary::cg(expr, ctx).ok().expect("todo")),
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
        Expression::Block(body) => {
            ctx.dispatch(Message::EnterAnonymousScope);

            let ret = body
                .iter()
                .fold(None, |_, expr| Some(cg(expr, ctx)))
                .unwrap();

            ctx.dispatch(Message::ExitScope);

            ret
        }
        _ => todo!("{:?}", expr),
    }
}
