use crate::{
    codegen::{
        unit::{function::Func, CompilerContext},
        ValueTypePair,
    },
    ty::{LLVMTyInCtx, TypeKind},
};
use llvm_sys_wrapper::{LLVMBasicBlockRef, LLVMTypeRef, LLVMValueRef};
use parser::ast::{IntSize, Type};

pub fn load(ctx: &CompilerContext, pointer: LLVMValueRef) -> LLVMValueRef {
    ctx.llvm_builder.build_load(pointer)
}

pub fn load_ty(ctx: &CompilerContext, pointer: LLVMValueRef, ty: TypeKind) -> ValueTypePair {
    (ctx.llvm_builder.build_load(pointer), ty).into()
}

pub fn load_prim(ctx: &CompilerContext, pointer: LLVMValueRef, ty: Type) -> ValueTypePair {
    (ctx.llvm_builder.build_load(pointer), ty).into()
}

pub fn store(ctx: &CompilerContext, value: LLVMValueRef, pointer: LLVMValueRef) -> LLVMValueRef {
    ctx.llvm_builder.build_store(value, pointer)
}

pub fn struct_gep(ctx: &CompilerContext, pointer: LLVMValueRef, index: u32) -> LLVMValueRef {
    ctx.llvm_builder.build_struct_gep(pointer, index)
}

pub fn struct_gep_with_name(
    ctx: &CompilerContext,
    pointer: LLVMValueRef,
    index: u32,
    name: &str,
) -> LLVMValueRef {
    ctx.llvm_builder
        .build_struct_gep_with_name(pointer, index, name)
}

pub fn inbounds_gep(
    ctx: &CompilerContext,
    pointer: LLVMValueRef,
    indices: &mut Vec<LLVMValueRef>,
) -> LLVMValueRef {
    ctx.llvm_builder
        .build_inbounds_gep(pointer, indices.as_mut_slice())
}

pub fn alloca(ctx: &CompilerContext, ty: LLVMTypeRef) -> LLVMValueRef {
    ctx.llvm_builder.build_alloca(ty)
}

pub fn alloca_with_name(ctx: &CompilerContext, ty: LLVMTypeRef, name: &str) -> LLVMValueRef {
    ctx.llvm_builder.build_alloca_with_name(ty, name)
}

pub fn alloca_ty(ctx: &CompilerContext, ty: &Type) -> LLVMValueRef {
    ctx.llvm_builder.build_alloca(ty.llvm_ty_in_ctx(ctx))
}

pub fn alloca_with_name_ty(ctx: &CompilerContext, ty: &Type, name: &str) -> LLVMValueRef {
    ctx.llvm_builder
        .build_alloca_with_name(ty.llvm_ty_in_ctx(ctx), name)
}

pub fn call(
    ctx: &CompilerContext,
    func: &Func,
    call_args: &mut Vec<LLVMValueRef>,
) -> ValueTypePair {
    (
        ctx.llvm_builder
            .build_call(func.as_ref(), call_args.as_mut()),
        func.ret_ty(),
    )
        .into()
}

pub fn extract_i8(ctx: &CompilerContext, agg_val: LLVMValueRef, index: u32) -> ValueTypePair {
    (
        ctx.llvm_builder.build_extract_value(agg_val, index),
        Type::Int(IntSize::Bits8),
    )
        .into()
}

pub fn extract_i16(ctx: &CompilerContext, agg_val: LLVMValueRef, index: u32) -> ValueTypePair {
    (
        ctx.llvm_builder.build_extract_value(agg_val, index),
        Type::Int(IntSize::Bits16),
    )
        .into()
}

pub fn extract_i32(ctx: &CompilerContext, agg_val: LLVMValueRef, index: u32) -> ValueTypePair {
    (
        ctx.llvm_builder.build_extract_value(agg_val, index),
        Type::Int(IntSize::Bits32),
    )
        .into()
}

pub fn extract_i64(ctx: &CompilerContext, agg_val: LLVMValueRef, index: u32) -> ValueTypePair {
    (
        ctx.llvm_builder.build_extract_value(agg_val, index),
        Type::Int(IntSize::Bits64),
    )
        .into()
}

pub fn extract_u8(ctx: &CompilerContext, agg_val: LLVMValueRef, index: u32) -> ValueTypePair {
    (
        ctx.llvm_builder.build_extract_value(agg_val, index),
        Type::UInt(IntSize::Bits8),
    )
        .into()
}

pub fn extract_u16(ctx: &CompilerContext, agg_val: LLVMValueRef, index: u32) -> ValueTypePair {
    (
        ctx.llvm_builder.build_extract_value(agg_val, index),
        Type::UInt(IntSize::Bits16),
    )
        .into()
}

pub fn extract_u32(ctx: &CompilerContext, agg_val: LLVMValueRef, index: u32) -> ValueTypePair {
    (
        ctx.llvm_builder.build_extract_value(agg_val, index),
        Type::UInt(IntSize::Bits32),
    )
        .into()
}

pub fn extract_u64(ctx: &CompilerContext, agg_val: LLVMValueRef, index: u32) -> ValueTypePair {
    (
        ctx.llvm_builder.build_extract_value(agg_val, index),
        Type::UInt(IntSize::Bits64),
    )
        .into()
}

pub fn ret(ctx: &CompilerContext, val: ValueTypePair) -> ValueTypePair {
    (ctx.llvm_builder.build_ret(*val), val.prim()).into()
}

pub fn ret_void(ctx: &CompilerContext) {
    ctx.llvm_builder.build_ret_void();
}

pub fn pos_at_end(ctx: &CompilerContext, bb: LLVMBasicBlockRef) {
    ctx.llvm_builder.position_at_end(bb);
}
