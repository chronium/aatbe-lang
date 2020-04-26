use crate::{
    codegen::{AatbeModule, CodegenUnit, ValueTypePair},
    ty::{LLVMTyInCtx, TypeKind},
};
use llvm_sys_wrapper::{LLVMBasicBlockRef, LLVMTypeRef, LLVMValueRef};
use parser::ast::{IntSize, PrimitiveType};

pub fn load(module: &AatbeModule, pointer: LLVMValueRef) -> LLVMValueRef {
    module.llvm_builder_ref().build_load(pointer)
}

pub fn load_ty(module: &AatbeModule, pointer: LLVMValueRef, ty: TypeKind) -> ValueTypePair {
    (module.llvm_builder_ref().build_load(pointer), ty).into()
}

pub fn load_prim(module: &AatbeModule, pointer: LLVMValueRef, ty: PrimitiveType) -> ValueTypePair {
    (module.llvm_builder_ref().build_load(pointer), ty).into()
}

pub fn store(module: &AatbeModule, value: LLVMValueRef, pointer: LLVMValueRef) -> LLVMValueRef {
    module.llvm_builder_ref().build_store(value, pointer)
}

pub fn struct_gep(module: &AatbeModule, pointer: LLVMValueRef, index: u32) -> LLVMValueRef {
    module.llvm_builder_ref().build_struct_gep(pointer, index)
}

pub fn struct_gep_with_name(
    module: &AatbeModule,
    pointer: LLVMValueRef,
    index: u32,
    name: &str,
) -> LLVMValueRef {
    module
        .llvm_builder_ref()
        .build_struct_gep_with_name(pointer, index, name)
}

pub fn inbounds_gep(
    module: &AatbeModule,
    pointer: LLVMValueRef,
    indices: &mut Vec<LLVMValueRef>,
) -> LLVMValueRef {
    module
        .llvm_builder_ref()
        .build_inbounds_gep(pointer, indices.as_mut_slice())
}

pub fn alloca(module: &AatbeModule, ty: LLVMTypeRef) -> LLVMValueRef {
    module.llvm_builder_ref().build_alloca(ty)
}

pub fn alloca_with_name(module: &AatbeModule, ty: LLVMTypeRef, name: &str) -> LLVMValueRef {
    module.llvm_builder_ref().build_alloca_with_name(ty, name)
}

pub fn alloca_ty(module: &AatbeModule, ty: &PrimitiveType) -> LLVMValueRef {
    module
        .llvm_builder_ref()
        .build_alloca(ty.llvm_ty_in_ctx(module))
}

pub fn alloca_with_name_ty(module: &AatbeModule, ty: &PrimitiveType, name: &str) -> LLVMValueRef {
    module
        .llvm_builder_ref()
        .build_alloca_with_name(ty.llvm_ty_in_ctx(module), name)
}

pub fn call(
    module: &AatbeModule,
    func: &CodegenUnit,
    call_args: &mut Vec<LLVMValueRef>,
) -> ValueTypePair {
    match func {
        CodegenUnit::Function(
            func,
            PrimitiveType::Function {
                ret_ty: box ret_ty, ..
            },
        ) => (
            module
                .llvm_builder_ref()
                .build_call(func.as_ref(), call_args.as_mut()),
            ret_ty.clone(),
        )
            .into(),
        _ => unreachable!(),
    }
}

pub fn extract_i8(module: &AatbeModule, agg_val: LLVMValueRef, index: u32) -> ValueTypePair {
    (
        module
            .llvm_builder_ref()
            .build_extract_value(agg_val, index),
        PrimitiveType::Int(IntSize::Bits8),
    )
        .into()
}

pub fn extract_i16(module: &AatbeModule, agg_val: LLVMValueRef, index: u32) -> ValueTypePair {
    (
        module
            .llvm_builder_ref()
            .build_extract_value(agg_val, index),
        PrimitiveType::Int(IntSize::Bits16),
    )
        .into()
}

pub fn extract_i32(module: &AatbeModule, agg_val: LLVMValueRef, index: u32) -> ValueTypePair {
    (
        module
            .llvm_builder_ref()
            .build_extract_value(agg_val, index),
        PrimitiveType::Int(IntSize::Bits32),
    )
        .into()
}

pub fn extract_i64(module: &AatbeModule, agg_val: LLVMValueRef, index: u32) -> ValueTypePair {
    (
        module
            .llvm_builder_ref()
            .build_extract_value(agg_val, index),
        PrimitiveType::Int(IntSize::Bits64),
    )
        .into()
}

pub fn extract_u8(module: &AatbeModule, agg_val: LLVMValueRef, index: u32) -> ValueTypePair {
    (
        module
            .llvm_builder_ref()
            .build_extract_value(agg_val, index),
        PrimitiveType::UInt(IntSize::Bits8),
    )
        .into()
}

pub fn extract_u16(module: &AatbeModule, agg_val: LLVMValueRef, index: u32) -> ValueTypePair {
    (
        module
            .llvm_builder_ref()
            .build_extract_value(agg_val, index),
        PrimitiveType::UInt(IntSize::Bits16),
    )
        .into()
}

pub fn extract_u32(module: &AatbeModule, agg_val: LLVMValueRef, index: u32) -> ValueTypePair {
    (
        module
            .llvm_builder_ref()
            .build_extract_value(agg_val, index),
        PrimitiveType::UInt(IntSize::Bits32),
    )
        .into()
}

pub fn extract_u64(module: &AatbeModule, agg_val: LLVMValueRef, index: u32) -> ValueTypePair {
    (
        module
            .llvm_builder_ref()
            .build_extract_value(agg_val, index),
        PrimitiveType::UInt(IntSize::Bits64),
    )
        .into()
}

pub fn ret(module: &AatbeModule, val: ValueTypePair) -> ValueTypePair {
    (
        module.llvm_builder_ref().build_ret(*val),
        val.prim().clone(),
    )
        .into()
}

pub fn ret_void(module: &AatbeModule) {
    module.llvm_builder_ref().build_ret_void();
}

pub fn pos_at_end(module: &AatbeModule, bb: LLVMBasicBlockRef) {
    module.llvm_builder_ref().position_at_end(bb);
}
