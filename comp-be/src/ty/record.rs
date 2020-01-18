use crate::{codegen::AatbeModule, ty::LLVMTyInCtx};
use parser::ast::PrimitiveType;

use llvm_sys_wrapper::{LLVMTypeRef, Struct};

pub struct Record {
    inner: Struct,
}

impl Record {
    pub fn new(module: &AatbeModule, name: &String, types: &Vec<PrimitiveType>) -> Self {
        let inner = Struct::new_with_name(module.llvm_context_ref().as_ref(), name.as_ref());

        let mut types = types
            .iter()
            .map(|ty| ty.llvm_ty_in_ctx(module))
            .collect::<Vec<LLVMTypeRef>>();

        inner.set_body(&mut types, false);

        Self { inner }
    }
}

impl LLVMTyInCtx for &Record {
    fn llvm_ty_in_ctx(&self, _module: &AatbeModule) -> LLVMTypeRef {
        self.inner.as_ref()
    }
}
