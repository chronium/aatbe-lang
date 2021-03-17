use crate::{
    codegen::{unit::CompilerContext, ValueTypePair},
    ty::variant::VariantType,
    ty::LLVMTyInCtx,
};
use llvm_sys_wrapper::{LLVMTypeRef, LLVMValueRef};
use parser::ast::PrimitiveType;

pub fn zext(ctx: &CompilerContext, val: ValueTypePair, ty: &PrimitiveType) -> ValueTypePair {
    (
        ctx.llvm_builder.build_zext(*val, ty.llvm_ty_in_ctx(ctx)),
        ty,
    )
        .into()
}

pub fn trunc(ctx: &CompilerContext, val: ValueTypePair, ty: &PrimitiveType) -> ValueTypePair {
    (
        ctx.llvm_builder.build_trunc(*val, ty.llvm_ty_in_ctx(ctx)),
        ty,
    )
        .into()
}

pub fn ftrunc(ctx: &CompilerContext, val: ValueTypePair, ty: &PrimitiveType) -> ValueTypePair {
    (
        ctx.llvm_builder
            .build_fp_trunc(*val, ty.llvm_ty_in_ctx(ctx)),
        ty,
    )
        .into()
}

pub fn itop(ctx: &CompilerContext, val: ValueTypePair, ty: &PrimitiveType) -> ValueTypePair {
    (
        ctx.llvm_builder
            .build_int_to_ptr(*val, ty.llvm_ty_in_ctx(ctx)),
        ty,
    )
        .into()
}

pub fn ptoi(ctx: &CompilerContext, val: ValueTypePair, ty: &PrimitiveType) -> ValueTypePair {
    (
        ctx.llvm_builder
            .build_ptr_to_int(*val, ty.llvm_ty_in_ctx(ctx)),
        ty,
    )
        .into()
}

pub fn stof(ctx: &CompilerContext, val: ValueTypePair, ty: &PrimitiveType) -> ValueTypePair {
    (
        ctx.llvm_builder
            .build_si_to_fp(*val, ty.llvm_ty_in_ctx(ctx)),
        ty,
    )
        .into()
}

pub fn utof(ctx: &CompilerContext, val: ValueTypePair, ty: &PrimitiveType) -> ValueTypePair {
    (
        ctx.llvm_builder
            .build_ui_to_fp(*val, ty.llvm_ty_in_ctx(ctx)),
        ty,
    )
        .into()
}

pub fn ftos(ctx: &CompilerContext, val: ValueTypePair, ty: &PrimitiveType) -> ValueTypePair {
    (
        ctx.llvm_builder
            .build_fp_to_si(*val, ty.llvm_ty_in_ctx(ctx)),
        ty,
    )
        .into()
}

pub fn ftou(ctx: &CompilerContext, val: ValueTypePair, ty: &PrimitiveType) -> ValueTypePair {
    (
        ctx.llvm_builder
            .build_fp_to_ui(*val, ty.llvm_ty_in_ctx(ctx)),
        ty,
    )
        .into()
}

pub fn bitcast(ctx: &CompilerContext, val: ValueTypePair, ty: &dyn LLVMTyInCtx) -> LLVMValueRef {
    ctx.llvm_builder.build_bitcast(*val, ty.llvm_ty_in_ctx(ctx))
}

pub fn bitcast_ty(ctx: &CompilerContext, val: ValueTypePair, ty: &PrimitiveType) -> ValueTypePair {
    (
        ctx.llvm_builder.build_bitcast(*val, ty.llvm_ty_in_ctx(ctx)),
        ty,
    )
        .into()
}

pub fn child_to_parent(
    ctx: &CompilerContext,
    val: ValueTypePair,
    parent: &VariantType,
) -> LLVMValueRef {
    ctx.llvm_builder.build_load(ctx.llvm_builder.build_bitcast(
        *val,
        ctx.llvm_context.PointerType(parent.llvm_ty_in_ctx(ctx)),
    ))
}

pub fn bitcast_to(ctx: &CompilerContext, val: LLVMValueRef, ty: LLVMTypeRef) -> LLVMValueRef {
    ctx.llvm_builder.build_bitcast(val, ty)
}
