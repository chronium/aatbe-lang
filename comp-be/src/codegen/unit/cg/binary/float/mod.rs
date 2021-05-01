mod arith;
mod cmp;
mod eqne;

use crate::codegen::{unit::CompilerContext, ValueTypePair};
use llvm_sys_wrapper::LLVMValueRef;
use parser::ast::FloatSize;

pub fn cg(
    lhs: LLVMValueRef,
    op: &String,
    rhs: LLVMValueRef,
    float_size: FloatSize,
    ctx: &CompilerContext,
) -> Option<ValueTypePair> {
    match op.as_str() {
        "+" | "-" | "*" | "/" | "%" => Some(arith::cg(lhs, op, rhs, float_size, ctx)),
        ">" | "<" | ">=" | "<=" => Some(cmp::cg(lhs, op, rhs, ctx)),
        "==" | "!=" => Some(eqne::cg(lhs, op, rhs, ctx)),
        _ => None,
    }
}
