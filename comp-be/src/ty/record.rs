use crate::{codegen::AatbeModule, ty::LLVMTyInCtx};
use parser::ast::PrimitiveType;

use llvm_sys_wrapper::{LLVMTypeRef, Struct};

pub struct Record {
    inner: Struct,
}

impl Record {
    pub fn new(module: &AatbeModule, name: &String) -> Self {
        Self {
            inner: Struct::new_with_name(module.llvm_context_ref().as_ref(), name.as_ref()),
        }
    }

    pub fn set_body(&self, module: &AatbeModule, types: &Vec<PrimitiveType>) {
        let mut types = types
            .iter()
            .map(|ty| ty.llvm_ty_in_ctx(module))
            .collect::<Vec<LLVMTypeRef>>();

        self.inner.set_body(&mut types, false);
    }
}

impl LLVMTyInCtx for &Record {
    fn llvm_ty_in_ctx(&self, _module: &AatbeModule) -> LLVMTypeRef {
        self.inner.as_ref()
    }
}
