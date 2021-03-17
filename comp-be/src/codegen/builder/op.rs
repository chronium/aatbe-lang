use crate::codegen::{unit::CompilerContext, ValueTypePair};
use llvm_sys_wrapper::LLVMValueRef;
use parser::ast::PrimitiveType;

pub fn neg(ctx: &CompilerContext, val: ValueTypePair) -> ValueTypePair {
    (ctx.llvm_builder.build_neg(*val), val.prim()).into()
}

pub fn fneg(ctx: &CompilerContext, val: ValueTypePair) -> ValueTypePair {
    (ctx.llvm_builder.build_fneg(*val), val.prim()).into()
}

pub fn not(ctx: &CompilerContext, val: ValueTypePair) -> ValueTypePair {
    (ctx.llvm_builder.build_not(*val), val.prim()).into()
}

pub fn ieq(ctx: &CompilerContext, lhs: LLVMValueRef, rhs: LLVMValueRef) -> ValueTypePair {
    (
        ctx.llvm_builder.build_icmp_eq(lhs, rhs),
        PrimitiveType::Bool,
    )
        .into()
}
