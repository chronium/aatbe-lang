use llvm_sys_wrapper::LLVMValueRef;
use parser::ast::AST;

use super::ModuleContext;

pub mod atom;
pub mod call;
pub mod expr;

pub fn cg(ast: &AST, ctx: &ModuleContext) -> Option<LLVMValueRef> {
    match ast {
        AST::Expr(expr) => expr::cg(expr, ctx).map(|e| *e),
        _ => todo!("{:?}", ast),
    }
}
