mod bool;
mod comp;
mod eqne;
mod float;
mod int;
mod uint;

use parser::ast::{Expression, IntSize, PrimitiveType};

use crate::{
    codegen::{
        unit::{cg::expr, CompilerContext},
        CompileError, GenRes,
    },
    fmt::AatbeFmt,
};

pub fn cg(expr: &Expression, ctx: &CompilerContext) -> GenRes {
    if let Expression::Binary(box lh, op, box rh) = expr {
        let lhs = expr::cg(lh, ctx).ok_or(CompileError::Handled)?;
        let rhs = expr::cg(rh, ctx).ok_or(CompileError::Handled)?;

        match (lhs.prim(), rhs.prim()) {
            (PrimitiveType::Bool, PrimitiveType::Bool) => {
                ctx.trace(format!("Binary {} {} {}", lh.fmt(), op, rh.fmt()));
                match bool::cg(*lhs, op, *rhs, ctx) {
                    Some(res) => Ok(res),
                    None => Err(CompileError::OpMismatch {
                        op: op.clone(),
                        types: (lhs.prim().fmt(), rhs.prim().fmt()),
                        values: (lh.fmt(), rh.fmt()),
                    }),
                }
            }
            (PrimitiveType::Char, PrimitiveType::Char) => {
                ctx.trace(format!("Binary {} {} {}", lh.fmt(), op, rh.fmt()));
                match uint::cg(*lhs, op, *rhs, IntSize::Bits8, ctx) {
                    Some(res) => Ok(res),
                    None => Err(CompileError::OpMismatch {
                        op: op.clone(),
                        types: (lhs.prim().fmt(), rhs.prim().fmt()),
                        values: (lh.fmt(), rh.fmt()),
                    }),
                }
            }
            (PrimitiveType::UInt(lsz), PrimitiveType::UInt(rsz)) if lsz == rsz => {
                ctx.trace(format!("Binary {} {} {}", lh.fmt(), op, rh.fmt()));
                match uint::cg(*lhs, op, *rhs, lsz.clone(), ctx) {
                    Some(res) => Ok(res),
                    None => Err(CompileError::OpMismatch {
                        op: op.clone(),
                        types: (lhs.prim().fmt(), rhs.prim().fmt()),
                        values: (lh.fmt(), rh.fmt()),
                    }),
                }
            }
            (PrimitiveType::Int(lsz), PrimitiveType::Int(rsz)) if lsz == rsz => {
                ctx.trace(format!("Binary {} {} {}", lh.fmt(), op, rh.fmt()));
                match int::cg(*lhs, op, *rhs, lsz.clone(), ctx) {
                    Some(res) => Ok(res),
                    None => Err(CompileError::OpMismatch {
                        op: op.clone(),
                        types: (lhs.prim().fmt(), rhs.prim().fmt()),
                        values: (lh.fmt(), rh.fmt()),
                    }),
                }
            }
            (PrimitiveType::Float(lsz), PrimitiveType::Float(rsz)) if lsz == rsz => {
                ctx.trace(format!("Binary {} {} {}", lh.fmt(), op, rh.fmt()));
                match float::cg(*lhs, op, *rhs, lsz.clone(), ctx) {
                    Some(res) => Ok(res),
                    None => Err(CompileError::OpMismatch {
                        op: op.clone(),
                        types: (lhs.prim().fmt(), rhs.prim().fmt()),
                        values: (lh.fmt(), rh.fmt()),
                    }),
                }
            }
            _ => Err(CompileError::BinaryMismatch {
                op: op.clone(),
                types: (lhs.prim().fmt(), rhs.prim().fmt()),
                values: (lh.fmt(), rh.fmt()),
            }),
        }
    } else {
        panic!("ICE")
    }
}
