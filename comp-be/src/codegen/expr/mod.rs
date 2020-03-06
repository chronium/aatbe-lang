mod eqne;
use eqne::{codegen_boolean, codegen_eq_ne, codegen_eq_ne_float};

mod compare;
use compare::{codegen_compare_float, codegen_compare_signed, codegen_compare_unsigned};

mod math;
use math::{codegen_float_ops, codegen_signed_ops, codegen_unsigned_ops};

pub mod const_expr;

use crate::{
    codegen::{AatbeModule, CompileError, GenRes, ValueTypePair},
    fmt::AatbeFmt,
};
use parser::ast::{Expression, FloatSize, IntSize, PrimitiveType};

use llvm_sys_wrapper::LLVMValueRef;

fn dispatch_bool(
    module: &AatbeModule,
    op: &String,
    lhs: LLVMValueRef,
    rhs: LLVMValueRef,
) -> Option<ValueTypePair> {
    match op.as_str() {
        "==" | "!=" => Some(codegen_eq_ne(module, op, lhs, rhs)),
        "&&" | "||" => Some(codegen_boolean(module, op, lhs, rhs)),
        _ => None,
    }
}

fn dispatch_signed(
    module: &AatbeModule,
    op: &String,
    lhs: LLVMValueRef,
    rhs: LLVMValueRef,
    int_size: IntSize,
) -> Option<ValueTypePair> {
    match op.as_str() {
        "+" | "-" | "*" | "/" | "%" => Some(codegen_signed_ops(module, op, lhs, rhs, int_size)),
        ">" | "<" | ">=" | "<=" => Some(codegen_compare_signed(module, op, lhs, rhs)),
        "==" | "!=" => Some(codegen_eq_ne(module, op, lhs, rhs)),
        _ => None,
    }
}

fn dispatch_float(
    module: &AatbeModule,
    op: &String,
    lhs: LLVMValueRef,
    rhs: LLVMValueRef,
    float_size: FloatSize,
) -> Option<ValueTypePair> {
    match op.as_str() {
        "+" | "-" | "*" | "/" | "%" => Some(codegen_float_ops(module, op, lhs, rhs, float_size)),
        ">" | "<" | ">=" | "<=" => Some(codegen_compare_float(module, op, lhs, rhs)),
        "==" | "!=" => Some(codegen_eq_ne_float(module, op, lhs, rhs)),
        _ => None,
    }
}

fn dispatch_unsigned(
    module: &AatbeModule,
    op: &String,
    lhs: LLVMValueRef,
    rhs: LLVMValueRef,
    int_size: IntSize,
) -> Option<ValueTypePair> {
    match op.as_str() {
        "+" | "-" | "*" | "/" | "%" => Some(codegen_unsigned_ops(module, op, lhs, rhs, int_size)),
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
        .expect(format!("ICE dispatch_binary codegen_expr lhs {:?}", lhs_expr).as_ref());
    let rhs = module
        .codegen_expr(rhs_expr)
        .expect(format!("ICE dispatch_binary codegen_expr rhs {:?}", rhs_expr).as_ref());

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
        (PrimitiveType::UInt(lsz), PrimitiveType::UInt(rsz)) if lsz == rsz => {
            match dispatch_unsigned(module, op, lhs.val(), rhs.val(), lsz.clone()) {
                Some(res) => Ok(res),
                None => Err(CompileError::OpMismatch {
                    op: op.clone(),
                    types: (lhs.prim().fmt(), rhs.prim().fmt()),
                    values: (lhs_expr.fmt(), rhs_expr.fmt()),
                }),
            }
        }
        (PrimitiveType::Int(lsz), PrimitiveType::Int(rsz)) if lsz == rsz => {
            match dispatch_signed(module, op, lhs.val(), rhs.val(), lsz.clone()) {
                Some(res) => Ok(res),
                None => Err(CompileError::OpMismatch {
                    op: op.clone(),
                    types: (lhs.prim().fmt(), rhs.prim().fmt()),
                    values: (lhs_expr.fmt(), rhs_expr.fmt()),
                }),
            }
        }
        (PrimitiveType::Float(lsz), PrimitiveType::Float(rsz)) if lsz == rsz => {
            match dispatch_float(module, op, lhs.val(), rhs.val(), lsz.clone()) {
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
