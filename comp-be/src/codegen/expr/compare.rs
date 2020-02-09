use crate::{
    codegen::{AatbeModule, ValueTypePair},
    ty::TypeKind,
};
use parser::ast::PrimitiveType;

use llvm_sys_wrapper::LLVMValueRef;

pub fn codegen_compare_signed(
    module: &AatbeModule,
    op: &String,
    lhs: LLVMValueRef,
    rhs: LLVMValueRef,
) -> ValueTypePair {
    (
        match op.as_str() {
            "<" => module.llvm_builder_ref().build_icmp_slt(lhs, rhs),
            ">" => module.llvm_builder_ref().build_icmp_sgt(lhs, rhs),
            "<=" => module.llvm_builder_ref().build_icmp_sle(lhs, rhs),
            ">=" => module.llvm_builder_ref().build_icmp_sge(lhs, rhs),
            _ => panic!("ICE codegen_compare_signed unhandled op {}", op),
        },
        TypeKind::Primitive(PrimitiveType::Bool),
    )
        .into()
}

pub fn codegen_compare_unsigned(
    module: &AatbeModule,
    op: &String,
    lhs: LLVMValueRef,
    rhs: LLVMValueRef,
) -> ValueTypePair {
    (
        match op.as_str() {
            "<" => module.llvm_builder_ref().build_icmp_ult(lhs, rhs),
            ">" => module.llvm_builder_ref().build_icmp_ugt(lhs, rhs),
            "<=" => module.llvm_builder_ref().build_icmp_ule(lhs, rhs),
            ">=" => module.llvm_builder_ref().build_icmp_uge(lhs, rhs),
            _ => panic!("ICE codegen_compare_unsigned unhandled op {}", op),
        },
        TypeKind::Primitive(PrimitiveType::Bool),
    )
        .into()
}