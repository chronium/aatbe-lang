mod expr;

use parser::ast::AST;

use super::ModuleContext;

pub fn decl(ast: &AST, ctx: &mut ModuleContext) {
    match ast {
        AST::Expr(expr) => expr::decl(expr, ctx),
        _ => todo!("{:?}", ast),
    }
}
