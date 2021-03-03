use crate::{
    codegen::{unit::ModuleContext, ValueTypePair},
    ty::variant::VariantType,
    ty::LLVMTyInCtx,
};
use llvm_sys_wrapper::{LLVMTypeRef, LLVMValueRef};
use parser::ast::PrimitiveType;

pub fn zext(ctx: &ModuleContext, val: ValueTypePair, ty: &PrimitiveType) -> ValueTypePair {
    (
        ctx.llvm_builder.build_zext(*val, ty.llvm_ty_in_ctx(ctx)),
        ty,
    )
        .into()
}

pub fn trunc(ctx: &ModuleContext, val: ValueTypePair, ty: &PrimitiveType) -> ValueTypePair {
    (
        ctx.llvm_builder.build_trunc(*val, ty.llvm_ty_in_ctx(ctx)),
        ty,
    )
        .into()
}

pub fn ftrunc(ctx: &ModuleContext, val: ValueTypePair, ty: &PrimitiveType) -> ValueTypePair {
    (
        ctx.llvm_builder
            .build_fp_trunc(*val, ty.llvm_ty_in_ctx(ctx)),
        ty,
    )
        .into()
}

pub fn itop(ctx: &ModuleContext, val: ValueTypePair, ty: &PrimitiveType) -> ValueTypePair {
    (
        ctx.llvm_builder
            .build_int_to_ptr(*val, ty.llvm_ty_in_ctx(ctx)),
        ty,
    )
        .into()
}

pub fn ptoi(ctx: &ModuleContext, val: ValueTypePair, ty: &PrimitiveType) -> ValueTypePair {
    (
        ctx.llvm_builder
            .build_ptr_to_int(*val, ty.llvm_ty_in_ctx(ctx)),
        ty,
    )
        .into()
}

pub fn stof(ctx: &ModuleContext, val: ValueTypePair, ty: &PrimitiveType) -> ValueTypePair {
    (
        ctx.llvm_builder
            .build_si_to_fp(*val, ty.llvm_ty_in_ctx(ctx)),
        ty,
    )
        .into()
}

pub fn utof(ctx: &ModuleContext, val: ValueTypePair, ty: &PrimitiveType) -> ValueTypePair {
    (
        ctx.llvm_builder
            .build_ui_to_fp(*val, ty.llvm_ty_in_ctx(ctx)),
        ty,
    )
        .into()
}

pub fn ftos(ctx: &ModuleContext, val: ValueTypePair, ty: &PrimitiveType) -> ValueTypePair {
    (
        ctx.llvm_builder
            .build_fp_to_si(*val, ty.llvm_ty_in_ctx(ctx)),
        ty,
    )
        .into()
}

pub fn ftou(ctx: &ModuleContext, val: ValueTypePair, ty: &PrimitiveType) -> ValueTypePair {
    (
        ctx.llvm_builder
            .build_fp_to_ui(*val, ty.llvm_ty_in_ctx(ctx)),
        ty,
    )
        .into()
}

pub fn bitcast(ctx: &ModuleContext, val: ValueTypePair, ty: &dyn LLVMTyInCtx) -> LLVMValueRef {
    ctx.llvm_builder.build_bitcast(*val, ty.llvm_ty_in_ctx(ctx))
}

pub fn bitcast_ty(ctx: &ModuleContext, val: ValueTypePair, ty: &PrimitiveType) -> ValueTypePair {
    (
        ctx.llvm_builder.build_bitcast(*val, ty.llvm_ty_in_ctx(ctx)),
        ty,
    )
        .into()
}

pub fn child_to_parent(
    ctx: &ModuleContext,
    val: ValueTypePair,
    parent: &VariantType,
) -> LLVMValueRef {
    ctx.llvm_builder.build_load(ctx.llvm_builder.build_bitcast(
        *val,
        ctx.llvm_context.PointerType(parent.llvm_ty_in_ctx(ctx)),
    ))
}

pub fn bitcast_to(ctx: &ModuleContext, val: LLVMValueRef, ty: LLVMTypeRef) -> LLVMValueRef {
    ctx.llvm_builder.build_bitcast(val, ty)
}
