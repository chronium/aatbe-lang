use llvm_sys_wrapper::LLVMValueRef;
use parser::ast::AST;

use super::{CompilerContext, Message};

pub mod atom;
pub mod call;
pub mod expr;

pub fn cg(ast: &AST, ctx: &CompilerContext) -> Option<LLVMValueRef> {
    match ast {
        AST::Expr(expr) => expr::cg(expr, ctx).map(|e| *e),
        AST::File(nodes) => nodes.iter().fold(None, |_, n| cg(n, ctx)),
        AST::Module(name, ast) => {
            ctx.dispatch(Message::RestoreModuleScope(name.clone()));
            let ret = cg(ast, ctx);
            ctx.dispatch(Message::ExitScope);
            ret
        }
        _ => todo!("{:?}", ast),
    }
}
