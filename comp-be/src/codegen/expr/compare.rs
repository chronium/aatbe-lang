use crate::codegen::{unit::CompilerContext, ValueTypePair};
use parser::ast::PrimitiveType;

use llvm_sys_wrapper::LLVMValueRef;

pub fn codegen_compare_float(
    module: &CompilerContext,
    op: &String,
    lhs: LLVMValueRef,
    rhs: LLVMValueRef,
) -> ValueTypePair {
    (
        match op.as_str() {
            "<" => module.llvm_builder.build_fcmp_ult(lhs, rhs),
            ">" => module.llvm_builder.build_fcmp_ugt(lhs, rhs),
            "<=" => module.llvm_builder.build_fcmp_ule(lhs, rhs),
            ">=" => module.llvm_builder.build_fcmp_uge(lhs, rhs),
            _ => panic!("ICE codegen_compare_float unhandled op {}", op),
        },
        PrimitiveType::Bool,
    )
        .into()
}

pub fn codegen_compare_signed(
    module: &CompilerContext,
    op: &String,
    lhs: LLVMValueRef,
    rhs: LLVMValueRef,
) -> ValueTypePair {
    (
        match op.as_str() {
            "<" => module.llvm_builder.build_icmp_slt(lhs, rhs),
            ">" => module.llvm_builder.build_icmp_sgt(lhs, rhs),
            "<=" => module.llvm_builder.build_icmp_sle(lhs, rhs),
            ">=" => module.llvm_builder.build_icmp_sge(lhs, rhs),
            _ => panic!("ICE codegen_compare_signed unhandled op {}", op),
        },
        PrimitiveType::Bool,
    )
        .into()
}

pub fn codegen_compare_unsigned(
    module: &CompilerContext,
    op: &String,
    lhs: LLVMValueRef,
    rhs: LLVMValueRef,
) -> ValueTypePair {
    (
        match op.as_str() {
            "<" => module.llvm_builder.build_icmp_ult(lhs, rhs),
            ">" => module.llvm_builder.build_icmp_ugt(lhs, rhs),
            "<=" => module.llvm_builder.build_icmp_ule(lhs, rhs),
            ">=" => module.llvm_builder.build_icmp_uge(lhs, rhs),
            _ => panic!("ICE codegen_compare_unsigned unhandled op {}", op),
        },
        PrimitiveType::Bool,
    )
        .into()
}
