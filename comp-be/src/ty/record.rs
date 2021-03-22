use crate::{
    codegen::{builder::base, unit::CompilerContext, AatbeModule, ValueTypePair},
    ty::{Aggregate, LLVMTyInCtx, TypeError, TypeResult},
};
use parser::ast::PrimitiveType;

use llvm_sys_wrapper::{LLVMTypeRef, LLVMValueRef, Struct};
use std::collections::HashMap;
use tuple_combinator::TupleCombinator;

pub struct Record {
    name: String,
    inner: Struct,
    body: HashMap<String, u32>,
    types: HashMap<u32, PrimitiveType>,
}

impl Record {
    pub fn new(module: &AatbeModule, name: &String, types: &Vec<PrimitiveType>) -> Self {
        let mut body = HashMap::new();
        let mut types_map = HashMap::new();
        types.iter().enumerate().for_each(|(index, ty)| match ty {
            PrimitiveType::NamedType { name, ty: Some(ty) } => {
                body.insert(name.clone(), index as u32);
                types_map.insert(index as u32, *ty.clone());
            }
            _ => panic!("ICE rec new {:?}", ty),
        });

        Self {
            name: name.clone(),
            inner: Struct::new_with_name(module.llvm_context_ref().as_ref(), name.as_ref()),
            body,
            types: types_map,
        }
    }

    pub fn set_body(&self, ctx: &CompilerContext, types: &Vec<PrimitiveType>) {
        let mut types = types
            .iter()
            .map(|ty| ty.llvm_ty_in_ctx(ctx))
            .collect::<Vec<_>>();

        self.inner.set_body(&mut types, false);
    }

    pub fn get_field_index_ty(&self, name: &String) -> Option<(u32, PrimitiveType)> {
        let idx = self.body.get(name).map(|i| *i);
        (idx, self.types.get(&idx?).map(|i| i.clone())).transpose()
    }

    pub fn name(&self) -> String {
        self.name.clone()
    }
}

impl Aggregate for Record {
    fn gep_indexed_field(
        &self,
        ctx: &CompilerContext,
        index: u32,
        aggregate_ref: LLVMValueRef,
    ) -> TypeResult<ValueTypePair> {
        Ok((
            base::struct_gep(ctx, aggregate_ref, index),
            self.types
                .get(&index)
                .ok_or(TypeError::RecordIndexOOB(self.name.clone(), index))?
                .clone(),
        )
            .into())
    }

    fn gep_named_field(
        &self,
        ctx: &CompilerContext,
        name: &String,
        aggregate_ref: LLVMValueRef,
    ) -> TypeResult<ValueTypePair> {
        match self.body.get(name) {
            None => Err(TypeError::RecordNameNotFound(
                self.name.clone(),
                name.clone(),
            )),
            Some(index) => {
                let ty = self.types.get(index).cloned().unwrap();
                let gep = base::struct_gep(ctx, aggregate_ref, *index);
                Ok((gep, ty).into())
            }
        }
    }
}

pub fn store_named_field(
    ctx: &CompilerContext,
    struct_ref: LLVMValueRef,
    rec_name: &String,
    rec: &Record,
    name: &String,
    value: ValueTypePair,
) -> Result<(), PrimitiveType> {
    let index = rec
        .get_field_index_ty(name)
        .expect(format!("Cannot find field {:?} in {:?}\0", name, rec.name).as_str());

    let gep = base::struct_gep_with_name(
        ctx,
        struct_ref,
        index.0,
        format!("{}.{}\0", rec_name, name).as_str(),
    );

    if value.prim() != &index.1 {
        Err(index.1)
    } else {
        base::store(ctx, *value, gep);
        Ok(())
    }
}

impl LLVMTyInCtx for &Record {
    fn llvm_ty_in_ctx(&self, _ctx: &CompilerContext) -> LLVMTypeRef {
        self.inner.as_ref()
    }
}
