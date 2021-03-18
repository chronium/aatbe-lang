mod arith;
mod cmp;

use crate::codegen::{
    unit::{cg::binary::eqne, CompilerContext},
    ValueTypePair,
};
use llvm_sys_wrapper::LLVMValueRef;
use parser::ast::IntSize;

pub fn cg(
    lhs: LLVMValueRef,
    op: &String,
    rhs: LLVMValueRef,
    ty: IntSize,
    ctx: &CompilerContext,
) -> Option<ValueTypePair> {
    match op.as_str() {
        "+" | "-" | "*" | "/" | "%" => Some(arith::cg(lhs, op, rhs, ty, ctx)),
        ">" | "<" | ">=" | "<=" => Some(cmp::cg(lhs, op, rhs, ctx)),
        "==" | "!=" => Some(eqne::cg(lhs, op, rhs, ctx)),
        _ => None,
    }
}
