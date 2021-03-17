use crate::codegen::unit::CompilerContext;
use llvm_sys_wrapper::{LLVMBasicBlockRef, LLVMValueRef};

pub fn branch(module: &CompilerContext, block: LLVMBasicBlockRef) -> LLVMValueRef {
    module.llvm_builder.build_br(block)
}

pub fn cond_branch(
    module: &CompilerContext,
    cond: LLVMValueRef,
    then_block: LLVMBasicBlockRef,
    else_block: LLVMBasicBlockRef,
) -> LLVMValueRef {
    module
        .llvm_builder
        .build_cond_br(cond, then_block, else_block)
}
