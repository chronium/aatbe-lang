use crate::{codegen::unit::CompilerContext, codegen::ValueTypePair};
use llvm_sys_wrapper::LLVMValueRef;
use parser::ast::{IntSize, Type};

pub fn cg(
    lhs: LLVMValueRef,
    op: &String,
    rhs: LLVMValueRef,
    int_size: IntSize,
    ctx: &CompilerContext,
) -> ValueTypePair {
    (
        match op.as_str() {
            "+" => ctx.llvm_builder.build_add(lhs, rhs),
            "-" => ctx.llvm_builder.build_sub(lhs, rhs),
            "*" => ctx.llvm_builder.build_mul(lhs, rhs),
            "/" => ctx.llvm_builder.build_sdiv(lhs, rhs),
            "%" => ctx.llvm_builder.build_srem(lhs, rhs),
            _ => panic!("ICE codegen_unsigned_ops unhandled op {}", op),
        },
        Type::Int(int_size),
    )
        .into()
}
