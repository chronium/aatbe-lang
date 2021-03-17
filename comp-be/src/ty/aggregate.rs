use crate::{
    codegen::{unit::CompilerContext, ValueTypePair},
    ty::TypeResult,
};

use llvm_sys_wrapper::LLVMValueRef;

pub trait Aggregate {
    fn gep_indexed_field(
        &self,
        ctx: &CompilerContext,
        index: u32,
        aggregate_ref: LLVMValueRef,
    ) -> TypeResult<ValueTypePair>;

    fn gep_named_field(
        &self,
        ctx: &CompilerContext,
        name: &String,
        aggregate_ref: LLVMValueRef,
    ) -> TypeResult<ValueTypePair>;

    fn gep_field(
        &self,
        ctx: &CompilerContext,
        name: &String,
        aggregate_ref: LLVMValueRef,
    ) -> TypeResult<ValueTypePair> {
        if let Ok(index) = name.parse::<u32>() {
            self.gep_indexed_field(ctx, index, aggregate_ref)
        } else {
            self.gep_named_field(ctx, name, aggregate_ref)
        }
    }
}
