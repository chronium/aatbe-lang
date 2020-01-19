use crate::{codegen::AatbeModule, ty::LLVMTyInCtx};
use parser::ast::PrimitiveType;

use llvm_sys_wrapper::{LLVMTypeRef, LLVMValueRef, Struct};
use std::collections::HashMap;

pub struct Record {
    name: String,
    inner: Struct,
    body: HashMap<String, u32>,
}

impl Record {
    pub fn new(module: &AatbeModule, name: &String, types: &Vec<PrimitiveType>) -> Self {
        let mut body = HashMap::new();
        types.iter().enumerate().for_each(|(index, ty)| match ty {
            PrimitiveType::NamedType { name, ty: _ } => {
                body.insert(name.clone(), index as u32);
            }
            _ => {}
        });

        Self {
            name: name.clone(),
            inner: Struct::new_with_name(module.llvm_context_ref().as_ref(), name.as_ref()),
            body,
        }
    }

    pub fn set_body(&self, module: &AatbeModule, types: &Vec<PrimitiveType>) {
        let mut types = types
            .iter()
            .map(|ty| ty.llvm_ty_in_ctx(module))
            .collect::<Vec<LLVMTypeRef>>();

        self.inner.set_body(&mut types, false);
    }

    pub fn get_field_index(&self, name: &String) -> Option<&u32> {
        self.body.get(name)
    }
}

pub fn store_named_field(
    module: &AatbeModule,
    struct_ref: LLVMValueRef,
    rec_name: &String,
    rec: &Record,
    name: &String,
    value: LLVMValueRef,
) {
    let index = *rec
        .get_field_index(name)
        .expect(format!("Cannot find field {:?} in {:?}", name, rec.name).as_str());

    let gep = module.llvm_builder_ref().build_struct_gep_with_name(
        struct_ref,
        index,
        format!("{}.{}", rec_name, name).as_str(),
    );

    module.llvm_builder_ref().build_store(value, gep);
}

impl LLVMTyInCtx for &Record {
    fn llvm_ty_in_ctx(&self, _module: &AatbeModule) -> LLVMTypeRef {
        self.inner.as_ref()
    }
}
