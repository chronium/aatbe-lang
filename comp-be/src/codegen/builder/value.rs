use crate::codegen::{unit::CompilerContext, ValueTypePair};
use llvm_sys_wrapper::{LLVMValueRef, Struct};
use parser::ast::{FloatSize, IntSize, Type};

pub fn t(ctx: &CompilerContext) -> ValueTypePair {
    (ctx.llvm_context.SInt1(1), Type::Bool).into()
}

pub fn f(ctx: &CompilerContext) -> ValueTypePair {
    (ctx.llvm_context.SInt1(0), Type::Bool).into()
}

pub fn char(ctx: &CompilerContext, c: char) -> ValueTypePair {
    (ctx.llvm_context.SInt8(c as u64), Type::Char).into()
}

pub fn s8(ctx: &CompilerContext, value: i8) -> ValueTypePair {
    (
        ctx.llvm_context.SInt8(value as u64),
        Type::Int(IntSize::Bits8),
    )
        .into()
}

pub fn s16(ctx: &CompilerContext, value: i16) -> ValueTypePair {
    (
        ctx.llvm_context.SInt16(value as u64),
        Type::Int(IntSize::Bits16),
    )
        .into()
}

pub fn s32(ctx: &CompilerContext, value: i32) -> ValueTypePair {
    (
        ctx.llvm_context.SInt32(value as u64),
        Type::Int(IntSize::Bits32),
    )
        .into()
}

pub fn s64(ctx: &CompilerContext, value: i64) -> ValueTypePair {
    (
        ctx.llvm_context.SInt64(value as u64),
        Type::Int(IntSize::Bits64),
    )
        .into()
}

pub fn u8(ctx: &CompilerContext, value: u8) -> ValueTypePair {
    (
        ctx.llvm_context.UInt8(value as u64),
        Type::UInt(IntSize::Bits8),
    )
        .into()
}

pub fn u16(ctx: &CompilerContext, value: u16) -> ValueTypePair {
    (
        ctx.llvm_context.UInt16(value as u64),
        Type::UInt(IntSize::Bits16),
    )
        .into()
}

pub fn u32(ctx: &CompilerContext, value: u32) -> ValueTypePair {
    (
        ctx.llvm_context.UInt32(value as u64),
        Type::UInt(IntSize::Bits32),
    )
        .into()
}

pub fn u64(ctx: &CompilerContext, value: u64) -> ValueTypePair {
    (ctx.llvm_context.UInt64(value), Type::UInt(IntSize::Bits64)).into()
}

pub fn slice(ctx: &CompilerContext, pointer: LLVMValueRef, ty: Type, len: u32) -> ValueTypePair {
    (
        Struct::new_const_struct(&mut [pointer, *u32(ctx, len)], false),
        Type::Slice { ty: box ty },
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
        Type::Int(size),
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
        Type::UInt(size),
    )
        .into()
}

pub fn floating(ctx: &CompilerContext, size: FloatSize, value: f64) -> ValueTypePair {
    (
        match size {
            FloatSize::Bits32 => ctx.llvm_context.Float(value),
            FloatSize::Bits64 => ctx.llvm_context.Double(value),
        },
        Type::Float(size),
    )
        .into()
}

pub fn str(ctx: &CompilerContext, string: &str) -> ValueTypePair {
    (ctx.llvm_builder.build_global_string_ptr(string), Type::Str).into()
}

pub fn unit(ctx: &CompilerContext) -> ValueTypePair {
    (
        ctx.llvm_context.Null(ctx.llvm_context.VoidType()),
        Type::Unit,
    )
        .into()
}
