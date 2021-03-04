use crate::codegen::{unit::ModuleContext, ValueTypePair};
use parser::ast::PrimitiveType;

use llvm_sys_wrapper::LLVMValueRef;

pub fn codegen_eq_ne(
    ctx: &ModuleContext,
    op: &String,
    lhs: LLVMValueRef,
    rhs: LLVMValueRef,
) -> ValueTypePair {
    (
        match op.as_str() {
            "==" => ctx.llvm_builder.build_icmp_eq(lhs, rhs),
            "!=" => ctx.llvm_builder.build_icmp_ne(lhs, rhs),
            _ => panic!("ICE codegen_eq_ne unhandled op {}", op),
        },
        PrimitiveType::Bool,
    )
        .into()
}

pub fn codegen_boolean(
    ctx: &ModuleContext,
    op: &String,
    lhs: LLVMValueRef,
    rhs: LLVMValueRef,
) -> ValueTypePair {
    (
        match op.as_str() {
            "&&" => ctx.llvm_builder.build_and(lhs, rhs),
            "||" => ctx.llvm_builder.build_or(lhs, rhs),
            _ => panic!("ICE codegen_eq_ne unhandled op {}", op),
        },
        PrimitiveType::Bool,
    )
        .into()
}

pub fn codegen_eq_ne_float(
    ctx: &ModuleContext,
    op: &String,
    lhs: LLVMValueRef,
    rhs: LLVMValueRef,
) -> ValueTypePair {
    (
        match op.as_str() {
            "==" => ctx.llvm_builder.build_fcmp_ueq(lhs, rhs),
            "!=" => ctx.llvm_builder.build_fcmp_une(lhs, rhs),
            _ => panic!("ICE codegen_eq_ne_float unhandled op {}", op),
        },
        PrimitiveType::Bool,
    )
        .into()
}
