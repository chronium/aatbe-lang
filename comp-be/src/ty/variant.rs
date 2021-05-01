use crate::{
    codegen::{builder::base, unit::CompilerContext, ValueTypePair},
    ty::{Aggregate, LLVMTyInCtx, TypeError, TypeResult},
};

use llvm_sys_wrapper::{LLVMTypeRef, LLVMValueRef};
use parser::ast::Type;
use std::{collections::HashMap, fmt};

pub struct VariantType {
    pub type_name: String,
    pub variants: HashMap<String, Variant>,
    pub discriminant_type: Type,
    pub ty: LLVMTypeRef,
}

impl VariantType {
    pub fn push_variant(&mut self, name: &String, variant: Variant) {
        self.variants.insert(name.clone(), variant);
    }
}

impl LLVMTyInCtx for VariantType {
    fn llvm_ty_in_ctx(&self, _: &CompilerContext) -> LLVMTypeRef {
        self.ty
    }
}

#[derive(Debug)]
pub struct Variant {
    pub parent_name: String,
    pub name: String,
    pub types: Option<Vec<Type>>,
    pub ty: LLVMTypeRef,
    pub discriminant: u32,
}

impl LLVMTyInCtx for Variant {
    fn llvm_ty_in_ctx(&self, _: &CompilerContext) -> LLVMTypeRef {
        self.ty
    }
}

impl VariantType {
    pub fn get_variant(&self, name: &String) -> Option<&Variant> {
        self.variants.get(name)
    }

    pub fn has_variant(&self, name: &String) -> bool {
        self.variants.contains_key(name)
    }
}

impl Aggregate for Variant {
    fn gep_indexed_field(
        &self,
        ctx: &CompilerContext,
        index: u32,
        aggregate_ref: LLVMValueRef,
    ) -> TypeResult<ValueTypePair> {
        match &self.types {
            None => Err(TypeError::VariantOOB(self.name.clone(), index)),
            Some(types) if types.len() < index as usize => {
                Err(TypeError::VariantOOB(self.name.clone(), index))
            }
            Some(types) => Ok((
                base::struct_gep(ctx, aggregate_ref, index + 1),
                types[index as usize].clone(),
            )
                .into()),
        }
    }

    fn gep_named_field(
        &self,
        _ctx: &CompilerContext,
        name: &String,
        _aggregate_ref: LLVMValueRef,
    ) -> TypeResult<ValueTypePair> {
        Err(TypeError::NamedOnVariant(self.name.clone(), name.clone()))
    }
}

impl fmt::Debug for VariantType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "VariantType {{ type_name: {:?}, variants: {}, ty: {:?} }}",
            self.type_name,
            format!(
                "{{ {} }}",
                self.variants
                    .iter()
                    .map(|(k, v)| format!("{}: {:?}", k, v))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            self.ty
        )
    }
}
