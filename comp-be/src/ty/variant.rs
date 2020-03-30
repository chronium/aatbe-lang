use crate::{
    codegen::{AatbeModule, ValueTypePair},
    ty::{Aggregate, TypeError, TypeResult},
};

use llvm_sys_wrapper::{LLVMTypeRef, LLVMValueRef};
use parser::ast::PrimitiveType;
use std::{collections::HashMap, fmt};

pub struct VariantType {
    pub type_name: String,
    pub variants: HashMap<String, Variant>,
    pub discriminant_type: PrimitiveType,
    pub ty: LLVMTypeRef,
}

#[derive(Debug)]
pub struct Variant {
    pub parent_name: String,
    pub name: String,
    pub types: Option<Vec<PrimitiveType>>,
    pub ty: LLVMTypeRef,
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
        module: &AatbeModule,
        index: u32,
        aggregate_ref: LLVMValueRef,
    ) -> TypeResult<ValueTypePair> {
        let index = index + 1;
        match &self.types {
            None => Err(TypeError::VariantOOB(self.name.clone(), index)),
            Some(types) if types.len() < index as usize => {
                Err(TypeError::VariantOOB(self.name.clone(), index))
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
        Err(TypeError::NamedOnVariant(self.name.clone(), name.clone()))
    }
}

impl fmt::Debug for VariantType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "VariantType {{ type_name: {}, variants: {}, ty: {:?} }}",
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
