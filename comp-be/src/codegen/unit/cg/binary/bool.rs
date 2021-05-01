use crate::codegen::{
    unit::{
        cg::binary::{comp, eqne},
        CompilerContext,
    },
    ValueTypePair,
};
use llvm_sys_wrapper::LLVMValueRef;

pub fn cg(
    lhs: LLVMValueRef,
    op: &String,
    rhs: LLVMValueRef,
    ctx: &CompilerContext,
) -> Option<ValueTypePair> {
    match op.as_str() {
        "==" | "!=" => Some(eqne::cg(lhs, op, rhs, ctx)),
        "&&" | "||" => Some(comp::cg(lhs, op, rhs, ctx)),
        _ => None,
    }
}
