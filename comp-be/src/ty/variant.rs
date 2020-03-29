use crate::{
    codegen::{AatbeModule, ValueTypePair},
    ty::{Aggregate, TypeError, TypeResult},
};

use llvm_sys_wrapper::{LLVMTypeRef, LLVMValueRef};
use parser::ast::PrimitiveType;

#[derive(Debug)]
pub struct VariantType {
    pub type_name: String,
    pub variant_name: String,
    pub types: Option<Vec<PrimitiveType>>,
    pub ty: LLVMTypeRef,
}

impl Aggregate for VariantType {
    fn gep_indexed_field(
        &self,
        module: &AatbeModule,
        index: u32,
        aggregate_ref: LLVMValueRef,
    ) -> TypeResult<ValueTypePair> {
        match &self.types {
            None => Err(TypeError::VariantOOB(self.variant_name.clone(), index)),
            Some(types) if types.len() < index as usize => {
                Err(TypeError::VariantOOB(self.variant_name.clone(), index))
            }
            Some(types) => Ok((
                module
                    .llvm_builder_ref()
                    .build_struct_gep(aggregate_ref, index),
                types[index as usize].clone(),
            )
                .into()),
        }
    }

    fn gep_named_field(
        &self,
        _module: &AatbeModule,
        name: &String,
        _aggregate_ref: LLVMValueRef,
    ) -> TypeResult<ValueTypePair> {
        Err(TypeError::NamedOnVariant(
            self.variant_name.clone(),
            name.clone(),
        ))
    }
}
