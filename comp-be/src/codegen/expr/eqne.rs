use crate::{
    codegen::{AatbeModule, ValueTypePair},
    ty::TypeKind,
};
use parser::ast::PrimitiveType;

use llvm_sys_wrapper::LLVMValueRef;

pub fn codegen_eq_ne(
    module: &AatbeModule,
    op: &String,
    lhs: LLVMValueRef,
    rhs: LLVMValueRef,
) -> ValueTypePair {
    (
        match op.as_str() {
            "==" => module.llvm_builder_ref().build_icmp_eq(lhs, rhs),
            "!=" => module.llvm_builder_ref().build_icmp_ne(lhs, rhs),
            _ => panic!("ICE codegen_eq_ne unhandled op {}", op),
        },
        TypeKind::Primitive(PrimitiveType::Bool),
    )
        .into()
}

pub fn codegen_boolean(
    module: &AatbeModule,
    op: &String,
    lhs: LLVMValueRef,
    rhs: LLVMValueRef,
) -> ValueTypePair {
    (
        match op.as_str() {
            "&&" => module.llvm_builder_ref().build_and(lhs, rhs),
            "||" => module.llvm_builder_ref().build_and(lhs, rhs),
            _ => panic!("ICE codegen_eq_ne unhandled op {}", op),
        },
        TypeKind::Primitive(PrimitiveType::Bool),
    )
        .into()
}

pub fn codegen_eq_ne_float(
    module: &AatbeModule,
    op: &String,
    lhs: LLVMValueRef,
    rhs: LLVMValueRef,
) -> ValueTypePair {
    (
        match op.as_str() {
            "==" => module.llvm_builder_ref().build_fcmp_ueq(lhs, rhs),
            "!=" => module.llvm_builder_ref().build_fcmp_une(lhs, rhs),
            _ => panic!("ICE codegen_eq_ne_float unhandled op {}", op),
        },
        TypeKind::Primitive(PrimitiveType::Bool),
    )
        .into()
}
