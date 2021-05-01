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
            "==" => ctx.llvm_builder.build_fcmp_ueq(lhs, rhs),
            "!=" => ctx.llvm_builder.build_fcmp_une(lhs, rhs),
            _ => panic!("ICE codegen_eq_ne_float unhandled op {}", op),
        },
        Type::Bool,
    )
        .into()
}
