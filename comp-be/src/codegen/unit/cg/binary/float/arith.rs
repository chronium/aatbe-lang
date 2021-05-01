use crate::{codegen::unit::CompilerContext, codegen::ValueTypePair};
use llvm_sys_wrapper::LLVMValueRef;
use parser::ast::{FloatSize, Type};

pub fn cg(
    lhs: LLVMValueRef,
    op: &String,
    rhs: LLVMValueRef,
    float_size: FloatSize,
    ctx: &CompilerContext,
) -> ValueTypePair {
    (
        match op.as_str() {
            "+" => ctx.llvm_builder.build_fadd(lhs, rhs),
            "-" => ctx.llvm_builder.build_fsub(lhs, rhs),
            "*" => ctx.llvm_builder.build_fmul(lhs, rhs),
            "/" => ctx.llvm_builder.build_fdiv(lhs, rhs),
            "%" => ctx.llvm_builder.build_frem(lhs, rhs),
            _ => panic!("ICE codegen_float_ops unhandled op {}", op),
        },
        Type::Float(float_size),
    )
        .into()
}
