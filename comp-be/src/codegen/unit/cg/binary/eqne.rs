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
            "==" => ctx.llvm_builder.build_icmp_eq(lhs, rhs),
            "!=" => ctx.llvm_builder.build_icmp_ne(lhs, rhs),
            _ => panic!("ICE codegen_eq_ne unhandled op {}", op),
        },
        PrimitiveType::Bool,
    )
        .into()
}
