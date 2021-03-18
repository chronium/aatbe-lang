use crate::{codegen::unit::CompilerContext, codegen::ValueTypePair};
use llvm_sys_wrapper::LLVMValueRef;
use parser::ast::PrimitiveType;

pub fn cg(
    lhs: LLVMValueRef,
    op: &String,
    rhs: LLVMValueRef,
    ctx: &CompilerContext,
) -> ValueTypePair {
    (
        match op.as_str() {
            "<" => ctx.llvm_builder.build_icmp_slt(lhs, rhs),
            ">" => ctx.llvm_builder.build_icmp_sgt(lhs, rhs),
            "<=" => ctx.llvm_builder.build_icmp_sle(lhs, rhs),
            ">=" => ctx.llvm_builder.build_icmp_sge(lhs, rhs),
            _ => panic!("ICE codegen_compare_signed unhandled op {}", op),
        },
        PrimitiveType::Bool,
    )
        .into()
}
