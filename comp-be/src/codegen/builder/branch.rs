use crate::codegen::AatbeModule;
use llvm_sys_wrapper::{LLVMBasicBlockRef, LLVMValueRef};

pub fn branch(module: &AatbeModule, block: LLVMBasicBlockRef) -> LLVMValueRef {
    module.llvm_builder_ref().build_br(block)
}

pub fn cond_branch(
    module: &AatbeModule,
    cond: LLVMValueRef,
    then_block: LLVMBasicBlockRef,
    else_block: LLVMBasicBlockRef,
) -> LLVMValueRef {
    module
        .llvm_builder_ref()
        .build_cond_br(cond, then_block, else_block)
}
