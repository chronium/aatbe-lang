use crate::codegen::{AatbeModule, ValueTypePair};
use parser::ast::{FloatSize, IntSize, PrimitiveType};

use llvm_sys_wrapper::LLVMValueRef;

pub fn codegen_float_ops(
    module: &AatbeModule,
    op: &String,
    lhs: LLVMValueRef,
    rhs: LLVMValueRef,
    float_size: FloatSize,
) -> ValueTypePair {
    (
        match op.as_str() {
            "+" => module.llvm_builder_ref().build_fadd(lhs, rhs),
            "-" => module.llvm_builder_ref().build_fsub(lhs, rhs),
            "*" => module.llvm_builder_ref().build_fmul(lhs, rhs),
            "/" => module.llvm_builder_ref().build_fdiv(lhs, rhs),
            "%" => module.llvm_builder_ref().build_frem(lhs, rhs),
            _ => panic!("ICE codegen_float_ops unhandled op {}", op),
        },
        PrimitiveType::Float(float_size),
    )
        .into()
}

pub fn codegen_signed_ops(
    module: &AatbeModule,
    op: &String,
    lhs: LLVMValueRef,
    rhs: LLVMValueRef,
    int_size: IntSize,
) -> ValueTypePair {
    (
        match op.as_str() {
            "+" => module.llvm_builder_ref().build_add(lhs, rhs),
            "-" => module.llvm_builder_ref().build_sub(lhs, rhs),
            "*" => module.llvm_builder_ref().build_mul(lhs, rhs),
            "/" => module.llvm_builder_ref().build_sdiv(lhs, rhs),
            "%" => module.llvm_builder_ref().build_srem(lhs, rhs),
            _ => panic!("ICE codegen_unsigned_ops unhandled op {}", op),
        },
        PrimitiveType::Int(int_size),
    )
        .into()
}

pub fn codegen_unsigned_ops(
    module: &AatbeModule,
    op: &String,
    lhs: LLVMValueRef,
    rhs: LLVMValueRef,
    int_size: IntSize,
) -> ValueTypePair {
    (
        match op.as_str() {
            "+" => module.llvm_builder_ref().build_add(lhs, rhs),
            "-" => module.llvm_builder_ref().build_sub(lhs, rhs),
            "*" => module.llvm_builder_ref().build_mul(lhs, rhs),
            "/" => module.llvm_builder_ref().build_udiv(lhs, rhs),
            "%" => module.llvm_builder_ref().build_urem(lhs, rhs),
            _ => panic!("ICE codegen_unsigned_ops unhandled op {}", op),
        },
        PrimitiveType::UInt(int_size),
    )
        .into()
}
