use crate::{
    codegen::{AatbeModule, ValueTypePair},
    ty::TypeResult,
};

use llvm_sys_wrapper::LLVMValueRef;

pub trait Aggregate {
    fn gep_indexed_field(
        &self,
        module: &AatbeModule,
        index: u32,
        aggregate_ref: LLVMValueRef,
    ) -> TypeResult<ValueTypePair>;

    fn gep_named_field(
        &self,
        module: &AatbeModule,
        name: &String,
        aggregate_ref: LLVMValueRef,
    ) -> TypeResult<ValueTypePair>;
}
