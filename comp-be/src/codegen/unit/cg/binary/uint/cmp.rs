use crate::{codegen::unit::CompilerContext, codegen::ValueTypePair};
use llvm_sys_wrapper::LLVMValueRef;
use parser::ast::Type;

pub fn cg(
    lhs: LLVMValueRef,
    op: &String,
    rhs: LLVMValueRef,
    ctx: &CompilerContext,
) -> ValueTypePair {
    (
        match op.as_str() {
            "<" => ctx.llvm_builder.build_icmp_ult(lhs, rhs),
            ">" => ctx.llvm_builder.build_icmp_ugt(lhs, rhs),
            "<=" => ctx.llvm_builder.build_icmp_ule(lhs, rhs),
            ">=" => ctx.llvm_builder.build_icmp_uge(lhs, rhs),
            _ => panic!("ICE codegen_compare_unsigned unhandled op {}", op),
        },
        Type::Bool,
    )
        .into()
}
