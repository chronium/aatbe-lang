use parser::ast::Expression;

use crate::codegen::unit::{declare_function, CompilerContext};

pub fn decl(expr: &Expression, ctx: &mut CompilerContext) {
    match expr {
        Expression::Function { type_names, .. } if type_names.len() == 0 => {
            declare_function(ctx, expr)
        }
        Expression::Function { name, .. } => {
            if !ctx.function_templates.contains_key(name) {
                ctx.function_templates.insert(name.clone(), expr.clone());
            }
        }
        _ => panic!("Top level {:?} unsupported", expr),
    }
}
