use crate::codegen::{AatbeModule, ValueTypePair};
use llvm_sys_wrapper::{LLVMValueRef, Struct};
use parser::ast::{FloatSize, IntSize, PrimitiveType};

pub fn t(module: &AatbeModule) -> ValueTypePair {
    (module.llvm_context_ref().SInt1(1), PrimitiveType::Bool).into()
}

pub fn f(module: &AatbeModule) -> ValueTypePair {
    (module.llvm_context_ref().SInt1(1), PrimitiveType::Bool).into()
}

pub fn char(module: &AatbeModule, c: char) -> ValueTypePair {
    (
        module.llvm_context_ref().SInt8(c as u64),
        PrimitiveType::Char,
    )
        .into()
}

pub fn s8(module: &AatbeModule, value: i8) -> ValueTypePair {
    (
        module.llvm_context_ref().SInt8(value as u64),
        PrimitiveType::Int(IntSize::Bits8),
    )
        .into()
}

pub fn s16(module: &AatbeModule, value: i16) -> ValueTypePair {
    (
        module.llvm_context_ref().SInt16(value as u64),
        PrimitiveType::Int(IntSize::Bits16),
    )
        .into()
}

pub fn s32(module: &AatbeModule, value: i32) -> ValueTypePair {
    (
        module.llvm_context_ref().SInt32(value as u64),
        PrimitiveType::Int(IntSize::Bits32),
    )
        .into()
}

pub fn s64(module: &AatbeModule, value: i64) -> ValueTypePair {
    (
        module.llvm_context_ref().SInt64(value as u64),
        PrimitiveType::Int(IntSize::Bits64),
    )
        .into()
}

pub fn u8(module: &AatbeModule, value: u8) -> ValueTypePair {
    (
        module.llvm_context_ref().UInt8(value as u64),
        PrimitiveType::UInt(IntSize::Bits8),
    )
        .into()
}

pub fn u16(module: &AatbeModule, value: u16) -> ValueTypePair {
    (
        module.llvm_context_ref().UInt16(value as u64),
        PrimitiveType::UInt(IntSize::Bits16),
    )
        .into()
}

pub fn u32(module: &AatbeModule, value: u32) -> ValueTypePair {
    (
        module.llvm_context_ref().UInt32(value as u64),
        PrimitiveType::UInt(IntSize::Bits32),
    )
        .into()
}

pub fn u64(module: &AatbeModule, value: u64) -> ValueTypePair {
    (
        module.llvm_context_ref().UInt64(value),
        PrimitiveType::UInt(IntSize::Bits64),
    )
        .into()
}

pub fn slice(
    module: &AatbeModule,
    pointer: LLVMValueRef,
    ty: PrimitiveType,
    len: u32,
) -> ValueTypePair {
    (
        Struct::new_const_struct(&mut [pointer, *u32(module, len)], false),
        PrimitiveType::Slice { ty: box ty },
    )
        .into()
}

pub fn sint(module: &AatbeModule, size: IntSize, value: u64) -> ValueTypePair {
    (
        match size {
            IntSize::Bits8 => module.llvm_context_ref().SInt8(value),
            IntSize::Bits16 => module.llvm_context_ref().SInt16(value),
            IntSize::Bits32 => module.llvm_context_ref().SInt32(value),
            IntSize::Bits64 => module.llvm_context_ref().SInt64(value),
        },
        PrimitiveType::Int(size),
    )
        .into()
}

pub fn uint(module: &AatbeModule, size: IntSize, value: u64) -> ValueTypePair {
    (
        match size {
            IntSize::Bits8 => module.llvm_context_ref().UInt8(value),
            IntSize::Bits16 => module.llvm_context_ref().UInt16(value),
            IntSize::Bits32 => module.llvm_context_ref().UInt32(value),
            IntSize::Bits64 => module.llvm_context_ref().UInt64(value),
        },
        PrimitiveType::UInt(size),
    )
        .into()
}

pub fn floating(module: &AatbeModule, size: FloatSize, value: f64) -> ValueTypePair {
    (
        match size {
            FloatSize::Bits32 => module.llvm_context_ref().Float(value),
            FloatSize::Bits64 => module.llvm_context_ref().Double(value),
        },
        PrimitiveType::Float(size),
    )
        .into()
}

pub fn str(module: &AatbeModule, string: &str) -> ValueTypePair {
    (
        module.llvm_builder_ref().build_global_string_ptr(string),
        PrimitiveType::Str,
    )
        .into()
}
