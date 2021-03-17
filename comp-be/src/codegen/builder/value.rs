use crate::codegen::{unit::CompilerContext, ValueTypePair};
use llvm_sys_wrapper::{LLVMValueRef, Struct};
use parser::ast::{FloatSize, IntSize, PrimitiveType};

pub fn t(ctx: &CompilerContext) -> ValueTypePair {
    (ctx.llvm_context.SInt1(1), PrimitiveType::Bool).into()
}

pub fn f(ctx: &CompilerContext) -> ValueTypePair {
    (ctx.llvm_context.SInt1(0), PrimitiveType::Bool).into()
}

pub fn char(ctx: &CompilerContext, c: char) -> ValueTypePair {
    (ctx.llvm_context.SInt8(c as u64), PrimitiveType::Char).into()
}

pub fn s8(ctx: &CompilerContext, value: i8) -> ValueTypePair {
    (
        ctx.llvm_context.SInt8(value as u64),
        PrimitiveType::Int(IntSize::Bits8),
    )
        .into()
}

pub fn s16(ctx: &CompilerContext, value: i16) -> ValueTypePair {
    (
        ctx.llvm_context.SInt16(value as u64),
        PrimitiveType::Int(IntSize::Bits16),
    )
        .into()
}

pub fn s32(ctx: &CompilerContext, value: i32) -> ValueTypePair {
    (
        ctx.llvm_context.SInt32(value as u64),
        PrimitiveType::Int(IntSize::Bits32),
    )
        .into()
}

pub fn s64(ctx: &CompilerContext, value: i64) -> ValueTypePair {
    (
        ctx.llvm_context.SInt64(value as u64),
        PrimitiveType::Int(IntSize::Bits64),
    )
        .into()
}

pub fn u8(ctx: &CompilerContext, value: u8) -> ValueTypePair {
    (
        ctx.llvm_context.UInt8(value as u64),
        PrimitiveType::UInt(IntSize::Bits8),
    )
        .into()
}

pub fn u16(ctx: &CompilerContext, value: u16) -> ValueTypePair {
    (
        ctx.llvm_context.UInt16(value as u64),
        PrimitiveType::UInt(IntSize::Bits16),
    )
        .into()
}

pub fn u32(ctx: &CompilerContext, value: u32) -> ValueTypePair {
    (
        ctx.llvm_context.UInt32(value as u64),
        PrimitiveType::UInt(IntSize::Bits32),
    )
        .into()
}

pub fn u64(ctx: &CompilerContext, value: u64) -> ValueTypePair {
    (
        ctx.llvm_context.UInt64(value),
        PrimitiveType::UInt(IntSize::Bits64),
    )
        .into()
}

pub fn slice(
    ctx: &CompilerContext,
    pointer: LLVMValueRef,
    ty: PrimitiveType,
    len: u32,
) -> ValueTypePair {
    (
        Struct::new_const_struct(&mut [pointer, *u32(ctx, len)], false),
        PrimitiveType::Slice { ty: box ty },
    )
        .into()
}

pub fn sint(ctx: &CompilerContext, size: IntSize, value: u64) -> ValueTypePair {
    (
        match size {
            IntSize::Bits8 => ctx.llvm_context.SInt8(value),
            IntSize::Bits16 => ctx.llvm_context.SInt16(value),
            IntSize::Bits32 => ctx.llvm_context.SInt32(value),
            IntSize::Bits64 => ctx.llvm_context.SInt64(value),
        },
        PrimitiveType::Int(size),
    )
        .into()
}

pub fn uint(ctx: &CompilerContext, size: IntSize, value: u64) -> ValueTypePair {
    (
        match size {
            IntSize::Bits8 => ctx.llvm_context.UInt8(value),
            IntSize::Bits16 => ctx.llvm_context.UInt16(value),
            IntSize::Bits32 => ctx.llvm_context.UInt32(value),
            IntSize::Bits64 => ctx.llvm_context.UInt64(value),
        },
        PrimitiveType::UInt(size),
    )
        .into()
}

pub fn floating(ctx: &CompilerContext, size: FloatSize, value: f64) -> ValueTypePair {
    (
        match size {
            FloatSize::Bits32 => ctx.llvm_context.Float(value),
            FloatSize::Bits64 => ctx.llvm_context.Double(value),
        },
        PrimitiveType::Float(size),
    )
        .into()
}

pub fn str(ctx: &CompilerContext, string: &str) -> ValueTypePair {
    (
        ctx.llvm_builder.build_global_string_ptr(string),
        PrimitiveType::Str,
    )
        .into()
}
