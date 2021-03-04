use crate::codegen::{unit::ModuleContext, ValueTypePair};
use parser::ast::{FloatSize, IntSize, PrimitiveType};

use llvm_sys_wrapper::LLVMValueRef;

pub fn codegen_float_ops(
    ctx: &ModuleContext,
    op: &String,
    lhs: LLVMValueRef,
    rhs: LLVMValueRef,
    float_size: FloatSize,
) -> ValueTypePair {
    (
        match op.as_str() {
            "+" => ctx.llvm_builder.build_fadd(lhs, rhs),
            "-" => ctx.llvm_builder.build_fsub(lhs, rhs),
            "*" => ctx.llvm_builder.build_fmul(lhs, rhs),
            "/" => ctx.llvm_builder.build_fdiv(lhs, rhs),
            "%" => ctx.llvm_builder.build_frem(lhs, rhs),
            _ => panic!("ICE codegen_float_ops unhandled op {}", op),
        },
        PrimitiveType::Float(float_size),
    )
        .into()
}

pub fn codegen_signed_ops(
    ctx: &ModuleContext,
    op: &String,
    lhs: LLVMValueRef,
    rhs: LLVMValueRef,
    int_size: IntSize,
) -> ValueTypePair {
    (
        match op.as_str() {
            "+" => ctx.llvm_builder.build_add(lhs, rhs),
            "-" => ctx.llvm_builder.build_sub(lhs, rhs),
            "*" => ctx.llvm_builder.build_mul(lhs, rhs),
            "/" => ctx.llvm_builder.build_sdiv(lhs, rhs),
            "%" => ctx.llvm_builder.build_srem(lhs, rhs),
            _ => panic!("ICE codegen_unsigned_ops unhandled op {}", op),
        },
        PrimitiveType::Int(int_size),
    )
        .into()
}

pub fn codegen_unsigned_ops(
    ctx: &ModuleContext,
    op: &String,
    lhs: LLVMValueRef,
    rhs: LLVMValueRef,
    int_size: PrimitiveType,
) -> ValueTypePair {
    (
        match op.as_str() {
            "+" => ctx.llvm_builder.build_add(lhs, rhs),
            "-" => ctx.llvm_builder.build_sub(lhs, rhs),
            "*" => ctx.llvm_builder.build_mul(lhs, rhs),
            "/" => ctx.llvm_builder.build_udiv(lhs, rhs),
            "%" => ctx.llvm_builder.build_urem(lhs, rhs),
            _ => panic!("ICE codegen_unsigned_ops unhandled op {}", op),
        },
        int_size,
    )
        .into()
}
