use crate::codegen::unit::ModuleContext;
use llvm_sys_wrapper::{LLVMBasicBlockRef, LLVMValueRef};

pub fn branch(module: &ModuleContext, block: LLVMBasicBlockRef) -> LLVMValueRef {
    module.llvm_builder.build_br(block)
}

pub fn cond_branch(
    module: &ModuleContext,
    cond: LLVMValueRef,
    then_block: LLVMBasicBlockRef,
    else_block: LLVMBasicBlockRef,
) -> LLVMValueRef {
    module
        .llvm_builder
        .build_cond_br(cond, then_block, else_block)
}
