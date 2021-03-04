mod eqne;
use eqne::{codegen_boolean, codegen_eq_ne, codegen_eq_ne_float};

mod compare;
use compare::{codegen_compare_float, codegen_compare_signed, codegen_compare_unsigned};

mod math;
use math::{codegen_float_ops, codegen_signed_ops, codegen_unsigned_ops};

pub mod const_expr;

use crate::{
    codegen::{unit::ModuleContext, CompileError, GenRes, ValueTypePair},
    fmt::AatbeFmt,
};
use parser::ast::{Expression, FloatSize, IntSize, PrimitiveType};

use llvm_sys_wrapper::LLVMValueRef;

fn dispatch_bool(
    ctx: &ModuleContext,
    op: &String,
    lhs: LLVMValueRef,
    rhs: LLVMValueRef,
) -> Option<ValueTypePair> {
    match op.as_str() {
        "==" | "!=" => Some(codegen_eq_ne(ctx, op, lhs, rhs)),
        "&&" | "||" => Some(codegen_boolean(ctx, op, lhs, rhs)),
        _ => None,
    }
}

fn dispatch_signed(
    ctx: &ModuleContext,
    op: &String,
    lhs: LLVMValueRef,
    rhs: LLVMValueRef,
    int_size: IntSize,
) -> Option<ValueTypePair> {
    match op.as_str() {
        "+" | "-" | "*" | "/" | "%" => Some(codegen_signed_ops(ctx, op, lhs, rhs, int_size)),
        ">" | "<" | ">=" | "<=" => Some(codegen_compare_signed(ctx, op, lhs, rhs)),
        "==" | "!=" => Some(codegen_eq_ne(ctx, op, lhs, rhs)),
        _ => None,
    }
}

fn dispatch_float(
    ctx: &ModuleContext,
    op: &String,
    lhs: LLVMValueRef,
    rhs: LLVMValueRef,
    float_size: FloatSize,
) -> Option<ValueTypePair> {
    match op.as_str() {
        "+" | "-" | "*" | "/" | "%" => Some(codegen_float_ops(ctx, op, lhs, rhs, float_size)),
        ">" | "<" | ">=" | "<=" => Some(codegen_compare_float(ctx, op, lhs, rhs)),
        "==" | "!=" => Some(codegen_eq_ne_float(ctx, op, lhs, rhs)),
        _ => None,
    }
}

fn dispatch_unsigned(
    ctx: &ModuleContext,
    op: &String,
    lhs: LLVMValueRef,
    rhs: LLVMValueRef,
    int_size: PrimitiveType,
) -> Option<ValueTypePair> {
    match op.as_str() {
        "+" | "-" | "*" | "/" | "%" => Some(codegen_unsigned_ops(ctx, op, lhs, rhs, int_size)),
        ">" | "<" | ">=" | "<=" => Some(codegen_compare_unsigned(ctx, op, lhs, rhs)),
        "==" | "!=" => Some(codegen_eq_ne(ctx, op, lhs, rhs)),
        _ => None,
    }
}

pub fn codegen_binary(
    module: &mut ModuleContext,
    op: &String,
    lhs_expr: &Expression,
    rhs_expr: &Expression,
) -> GenRes {
    todo!()
    /*let lhs = module.codegen_expr(lhs_expr).ok_or(CompileError::Handled)?;
    let rhs = module.codegen_expr(rhs_expr).ok_or(CompileError::Handled)?;

    match (lhs.prim(), rhs.prim()) {
        (PrimitiveType::Bool, PrimitiveType::Bool) => match dispatch_bool(ctx, op, *lhs, *rhs) {
            Some(res) => Ok(res),
            None => Err(CompileError::OpMismatch {
                op: op.clone(),
                types: (lhs.prim().fmt(), rhs.prim().fmt()),
                values: (lhs_expr.fmt(), rhs_expr.fmt()),
            }),
        },
        (PrimitiveType::Char, PrimitiveType::Char) => {
            match dispatch_unsigned(ctx, op, *lhs, *rhs, PrimitiveType::Char) {
                Some(res) => Ok(res),
                None => Err(CompileError::OpMismatch {
                    op: op.clone(),
                    types: (lhs.prim().fmt(), rhs.prim().fmt()),
                    values: (lhs_expr.fmt(), rhs_expr.fmt()),
                }),
            }
        }
        (PrimitiveType::UInt(lsz), PrimitiveType::UInt(rsz)) if lsz == rsz => {
            match dispatch_unsigned(ctx, op, *lhs, *rhs, PrimitiveType::UInt(lsz.clone())) {
                Some(res) => Ok(res),
                None => Err(CompileError::OpMismatch {
                    op: op.clone(),
                    types: (lhs.prim().fmt(), rhs.prim().fmt()),
                    values: (lhs_expr.fmt(), rhs_expr.fmt()),
                }),
            }
        }
        (PrimitiveType::Int(lsz), PrimitiveType::Int(rsz)) if lsz == rsz => {
            match dispatch_signed(ctx, op, *lhs, *rhs, lsz.clone()) {
                Some(res) => Ok(res),
                None => Err(CompileError::OpMismatch {
                    op: op.clone(),
                    types: (lhs.prim().fmt(), rhs.prim().fmt()),
                    values: (lhs_expr.fmt(), rhs_expr.fmt()),
                }),
            }
        }
        (PrimitiveType::Float(lsz), PrimitiveType::Float(rsz)) if lsz == rsz => {
            match dispatch_float(ctx, op, *lhs, *rhs, lsz.clone()) {
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
    }*/
}
