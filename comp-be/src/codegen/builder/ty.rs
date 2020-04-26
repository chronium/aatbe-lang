use crate::{codegen::AatbeModule, ty::LLVMTyInCtx};
use llvm_sys_wrapper::LLVMTypeRef;

pub fn pointer(module: &AatbeModule, ty: &dyn LLVMTyInCtx) -> LLVMTypeRef {
    module
        .llvm_context_ref()
        .PointerType(ty.llvm_ty_in_ctx(module))
}

pub fn slice_type(module: &AatbeModule, ty: &dyn LLVMTyInCtx) -> LLVMTypeRef {
    module
        .llvm_context_ref()
        .StructType(&mut vec![slice_ptr(module, ty), i32(module)], false)
        .as_ref()
}

pub fn pointer_to(module: &AatbeModule, ty: LLVMTypeRef) -> LLVMTypeRef {
    module.llvm_context_ref().PointerType(ty)
}

pub fn array(module: &AatbeModule, ty: &dyn LLVMTyInCtx, count: u32) -> LLVMTypeRef {
    module
        .llvm_context_ref()
        .ArrayType(ty.llvm_ty_in_ctx(module), count)
}

pub fn slice(module: &AatbeModule, ty: &dyn LLVMTyInCtx) -> LLVMTypeRef {
    module
        .llvm_context_ref()
        .ArrayType(ty.llvm_ty_in_ctx(module), 0)
}

pub fn slice_ptr(module: &AatbeModule, ty: &dyn LLVMTyInCtx) -> LLVMTypeRef {
    pointer_to(module, slice(module, ty))
}

pub fn i8(module: &AatbeModule) -> LLVMTypeRef {
    module.llvm_context_ref().Int8Type()
}

pub fn i16(module: &AatbeModule) -> LLVMTypeRef {
    module.llvm_context_ref().Int16Type()
}

pub fn i32(module: &AatbeModule) -> LLVMTypeRef {
    module.llvm_context_ref().Int32Type()
}

pub fn i64(module: &AatbeModule) -> LLVMTypeRef {
    module.llvm_context_ref().Int64Type()
}
