use parser::ast::Expression;

use crate::codegen::unit::{declare_function, CompilerContext, Message};

pub fn decl(expr: &Expression, ctx: &mut CompilerContext) {
    match expr {
        Expression::Function { type_names, .. } if type_names.len() == 0 => {
            declare_function(ctx, expr)
        }
        Expression::Function { name, .. } => {
            ctx.dispatch(Message::PushFunctionTemplate(name.clone(), expr.clone()));
        }
        _ => panic!("Top level {:?} unsupported", expr),
    }
}
