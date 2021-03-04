use crate::codegen::{unit::ModuleContext, ValueTypePair};
use llvm_sys_wrapper::LLVMValueRef;
use parser::ast::PrimitiveType;

pub fn neg(ctx: &ModuleContext, val: ValueTypePair) -> ValueTypePair {
    (ctx.llvm_builder.build_neg(*val), val.prim()).into()
}

pub fn fneg(ctx: &ModuleContext, val: ValueTypePair) -> ValueTypePair {
    (ctx.llvm_builder.build_fneg(*val), val.prim()).into()
}

pub fn not(ctx: &ModuleContext, val: ValueTypePair) -> ValueTypePair {
    (ctx.llvm_builder.build_not(*val), val.prim()).into()
}

pub fn ieq(ctx: &ModuleContext, lhs: LLVMValueRef, rhs: LLVMValueRef) -> ValueTypePair {
    (
        ctx.llvm_builder.build_icmp_eq(lhs, rhs),
        PrimitiveType::Bool,
    )
        .into()
}
