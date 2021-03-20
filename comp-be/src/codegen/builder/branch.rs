use crate::codegen::unit::CompilerContext;
use llvm_sys_wrapper::{LLVMBasicBlockRef, LLVMValueRef};

pub fn branch(ctx: &CompilerContext, block: LLVMBasicBlockRef) -> LLVMValueRef {
    ctx.llvm_builder.build_br(block)
}

pub fn cond_branch(
    ctx: &CompilerContext,
    cond: LLVMValueRef,
    then_block: LLVMBasicBlockRef,
    else_block: LLVMBasicBlockRef,
) -> LLVMValueRef {
    ctx.llvm_builder.build_cond_br(cond, then_block, else_block)
}
