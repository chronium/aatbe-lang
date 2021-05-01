mod expr;

use parser::ast::AST;

use super::{CompilerContext, Message};

pub fn decl(ast: &AST, ctx: &mut CompilerContext) {
    match ast {
        AST::Expr(expr) => expr::decl(expr, ctx),
        AST::Module(name, ast) => {
            ctx.dispatch(Message::EnterModuleScope(name.clone()));
            super::decl(ast, ctx);
            ctx.dispatch(Message::ExitModuleScope(name.clone()));
        }
        AST::File(nodes) => nodes
            .iter()
            .fold(Some(()), |_, ast| Some(decl(ast, ctx)))
            .unwrap(),
        _ => todo!("{:?}", ast),
    }
}
