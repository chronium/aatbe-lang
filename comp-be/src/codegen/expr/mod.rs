mod eqne;
use eqne::codegen_eq_ne;

mod compare;
use compare::{codegen_compare_signed, codegen_compare_unsigned};

use crate::{
    codegen::{AatbeModule, CompileError, GenRes, ValueTypePair},
    fmt::AatbeFmt,
};
use parser::ast::{Expression, PrimitiveType};

use llvm_sys_wrapper::LLVMValueRef;

fn dispatch_bool(
    module: &AatbeModule,
    op: &String,
    lhs: LLVMValueRef,
    rhs: LLVMValueRef,
) -> Option<ValueTypePair> {
    match op.as_str() {
        "==" | "!=" => Some(codegen_eq_ne(module, op, lhs, rhs)),
        _ => None,
    }
}

fn dispatch_signed(
    module: &AatbeModule,
    op: &String,
    lhs: LLVMValueRef,
    rhs: LLVMValueRef,
) -> Option<ValueTypePair> {
    match op.as_str() {
        ">" | "<" | ">=" | "<=" => Some(codegen_compare_signed(module, op, lhs, rhs)),
        "==" | "!=" => Some(codegen_eq_ne(module, op, lhs, rhs)),
        _ => None,
    }
}

fn dispatch_unsigned(
    module: &AatbeModule,
    op: &String,
    lhs: LLVMValueRef,
    rhs: LLVMValueRef,
) -> Option<ValueTypePair> {
    match op.as_str() {
        ">" | "<" | ">=" | "<=" => Some(codegen_compare_unsigned(module, op, lhs, rhs)),
        "==" | "!=" => Some(codegen_eq_ne(module, op, lhs, rhs)),
        _ => None,
    }
}

pub fn codegen_binary(
    module: &mut AatbeModule,
    op: &String,
    lhs_expr: &Expression,
    rhs_expr: &Expression,
) -> GenRes {
    let lhs = module
        .codegen_expr(lhs_expr)
        .expect("ICE dispatch_binary codegen_expr lhs");
    let rhs = module
        .codegen_expr(rhs_expr)
        .expect("ICE dispatch_binary codegen_expr rhs");

    match (lhs.prim(), rhs.prim()) {
        (PrimitiveType::Bool, PrimitiveType::Bool) => {
            match dispatch_bool(module, op, lhs.val(), rhs.val()) {
                Some(res) => Ok(res),
                None => Err(CompileError::OpMismatch {
                    op: op.clone(),
                    types: (lhs.prim().fmt(), rhs.prim().fmt()),
                    values: (lhs_expr.fmt(), rhs_expr.fmt()),
                }),
            }
        }
        (PrimitiveType::UInt(_), PrimitiveType::UInt(_)) => {
            match dispatch_unsigned(module, op, lhs.val(), rhs.val()) {
                Some(res) => Ok(res),
                None => Err(CompileError::OpMismatch {
                    op: op.clone(),
                    types: (lhs.prim().fmt(), rhs.prim().fmt()),
                    values: (lhs_expr.fmt(), rhs_expr.fmt()),
                }),
            }
        }
        (PrimitiveType::Int(_), PrimitiveType::Int(_)) => {
            match dispatch_signed(module, op, lhs.val(), rhs.val()) {
                Some(res) => Ok(res),
                None => Err(CompileError::OpMismatch {
                    op: op.clone(),
                    types: (lhs.prim().fmt(), rhs.prim().fmt()),
                    values: (lhs_expr.fmt(), rhs_expr.fmt()),
                }),
            }
        }
        _ => Err(CompileError::BinaryMismatch {
            op: op.clone(),
            types: (lhs.prim().fmt(), rhs.prim().fmt()),
            values: (lhs_expr.fmt(), rhs_expr.fmt()),
        }),
    }
}
