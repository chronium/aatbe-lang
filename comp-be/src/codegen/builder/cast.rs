use crate::{
    codegen::{AatbeModule, ValueTypePair},
    ty::LLVMTyInCtx,
};
use llvm_sys_wrapper::{LLVMTypeRef, LLVMValueRef};
use parser::ast::PrimitiveType;

pub fn zext(module: &AatbeModule, val: ValueTypePair, ty: &PrimitiveType) -> ValueTypePair {
    (
        module
            .llvm_builder_ref()
            .build_zext(*val, ty.llvm_ty_in_ctx(module)),
        ty,
    )
        .into()
}

pub fn trunc(module: &AatbeModule, val: ValueTypePair, ty: &PrimitiveType) -> ValueTypePair {
    (
        module
            .llvm_builder_ref()
            .build_trunc(*val, ty.llvm_ty_in_ctx(module)),
        ty,
    )
        .into()
}

pub fn ftrunc(module: &AatbeModule, val: ValueTypePair, ty: &PrimitiveType) -> ValueTypePair {
    (
        module
            .llvm_builder_ref()
            .build_fp_trunc(*val, ty.llvm_ty_in_ctx(module)),
        ty,
    )
        .into()
}

pub fn itop(module: &AatbeModule, val: ValueTypePair, ty: &PrimitiveType) -> ValueTypePair {
    (
        module
            .llvm_builder_ref()
            .build_int_to_ptr(*val, ty.llvm_ty_in_ctx(module)),
        ty,
    )
        .into()
}

pub fn ptoi(module: &AatbeModule, val: ValueTypePair, ty: &PrimitiveType) -> ValueTypePair {
    (
        module
            .llvm_builder_ref()
            .build_ptr_to_int(*val, ty.llvm_ty_in_ctx(module)),
        ty,
    )
        .into()
}

pub fn stof(module: &AatbeModule, val: ValueTypePair, ty: &PrimitiveType) -> ValueTypePair {
    (
        module
            .llvm_builder_ref()
            .build_si_to_fp(*val, ty.llvm_ty_in_ctx(module)),
        ty,
    )
        .into()
}

pub fn utof(module: &AatbeModule, val: ValueTypePair, ty: &PrimitiveType) -> ValueTypePair {
    (
        module
            .llvm_builder_ref()
            .build_ui_to_fp(*val, ty.llvm_ty_in_ctx(module)),
        ty,
    )
        .into()
}

pub fn ftos(module: &AatbeModule, val: ValueTypePair, ty: &PrimitiveType) -> ValueTypePair {
    (
        module
            .llvm_builder_ref()
            .build_fp_to_si(*val, ty.llvm_ty_in_ctx(module)),
        ty,
    )
        .into()
}

pub fn ftou(module: &AatbeModule, val: ValueTypePair, ty: &PrimitiveType) -> ValueTypePair {
    (
        module
            .llvm_builder_ref()
            .build_fp_to_ui(*val, ty.llvm_ty_in_ctx(module)),
        ty,
    )
        .into()
}

pub fn bitcast(module: &AatbeModule, val: ValueTypePair, ty: &dyn LLVMTyInCtx) -> LLVMValueRef {
    module
        .llvm_builder_ref()
        .build_bitcast(*val, ty.llvm_ty_in_ctx(module))
}

pub fn bitcast_ty(module: &AatbeModule, val: ValueTypePair, ty: &PrimitiveType) -> ValueTypePair {
    (
        module
            .llvm_builder_ref()
            .build_bitcast(*val, ty.llvm_ty_in_ctx(module)),
        ty,
    )
        .into()
}

pub fn bitcast_to(module: &AatbeModule, val: LLVMValueRef, ty: LLVMTypeRef) -> LLVMValueRef {
    module.llvm_builder_ref().build_bitcast(val, ty)
}
