use crate::codegen::{AatbeModule, ValueTypePair};
use llvm_sys_wrapper::LLVMValueRef;
use parser::ast::PrimitiveType;

pub fn neg(module: &AatbeModule, val: ValueTypePair) -> ValueTypePair {
    (
        module.llvm_builder_ref().build_neg(*val),
        val.prim().clone(),
    )
        .into()
}

pub fn fneg(module: &AatbeModule, val: ValueTypePair) -> ValueTypePair {
    (
        module.llvm_builder_ref().build_fneg(*val),
        val.prim().clone(),
    )
        .into()
}

pub fn not(module: &AatbeModule, val: ValueTypePair) -> ValueTypePair {
    (
        module.llvm_builder_ref().build_not(*val),
        val.prim().clone(),
    )
        .into()
}

pub fn ieq(module: &AatbeModule, lhs: LLVMValueRef, rhs: LLVMValueRef) -> ValueTypePair {
    (
        module.llvm_builder_ref().build_icmp_eq(lhs, rhs),
        PrimitiveType::Bool,
    )
        .into()
}
