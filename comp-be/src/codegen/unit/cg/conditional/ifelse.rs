use crate::{
    codegen::{
        builder::{base, branch},
        unit::{cg::expr, CompilerContext, Message},
        ValueTypePair,
    },
    fmt::AatbeFmt,
    ty::LLVMTyInCtx,
};
use llvm_sys_wrapper::{LLVMBasicBlockRef, Phi};
use parser::ast::{Expression, PrimitiveType};

use guard::guard;

pub fn cg(expr: &Expression, ctx: &CompilerContext) -> Option<ValueTypePair> {
    guard!(let Expression::If {
        cond_expr,
        then_expr,
        elseif_exprs,
        else_expr
    } = expr else { unreachable!() });

    let then_bb = ctx.basic_block("then");
    let elseif_bbs = elseif_exprs
        .iter()
        .map(|e| {
            (
                (
                    ctx.basic_block("elseif_cond"),
                    ctx.basic_block("elseif_body"),
                ),
                e,
            )
        })
        .collect::<Vec<_>>();
    let else_bb = else_expr.as_ref().map(|_| ctx.basic_block("else"));
    let end_bb = ctx.basic_block("if_end");

    ctx.dispatch(Message::EnterIfScope(cond_expr.fmt()));
    let cond = expr::cg(cond_expr, ctx)?;

    if *cond.prim().inner() != PrimitiveType::Bool {
        /*
        self.add_error(CompileError::ExpectedType {
            expected_ty: PrimitiveType::Bool.fmt(),
            found_ty: cond.prim().fmt(),
            value: cond_expr.fmt(),
        }); */
        todo!("NOT A BOOL");
    }

    let next_bb = |i: usize| -> LLVMBasicBlockRef {
        elseif_bbs
            .get(i)
            .map(|e| e.0 .0)
            .unwrap_or(else_bb.unwrap_or(end_bb))
    };

    branch::cond_branch(ctx, *cond, then_bb, next_bb(0));

    base::pos_at_end(ctx, then_bb);
    let then_val = expr::cg(then_expr, ctx)?;

    ctx.dispatch(Message::ExitScope);

    let elseif_vals = elseif_bbs
        .iter()
        .enumerate()
        .map(|(i, ((bb_cond, bb_body), (cond, expr)))| {
            ctx.dispatch(Message::EnterElseIfScope(cond.fmt()));
            base::pos_at_end(ctx, *bb_cond);

            let cond = expr::cg(cond, ctx)?;

            if *cond.prim().inner() != PrimitiveType::Bool {
                /*
                self.add_error(CompileError::ExpectedType {
                    expected_ty: PrimitiveType::Bool.fmt(),
                    found_ty: cond.prim().fmt(),
                    value: cond_expr.fmt(),
                }); */
                todo!("NOT A BOOL");
            }

            branch::cond_branch(ctx, *cond, *bb_body, next_bb(i + 1));

            base::pos_at_end(ctx, *bb_body);
            let val = expr::cg(expr, ctx);
            branch::branch(ctx, end_bb);
            ctx.dispatch(Message::ExitScope);

            val
        })
        .collect::<Vec<_>>();

    base::pos_at_end(ctx, then_bb);

    let else_val = else_bb
        .map(|bb| {
            ctx.dispatch(Message::EnterElseScope);
            branch::branch(ctx, end_bb);
            base::pos_at_end(ctx, bb);

            let res = else_expr.as_ref().and_then(|expr| expr::cg(&expr, ctx));
            ctx.dispatch(Message::ExitScope);
            res
        })
        .flatten();

    branch::branch(ctx, end_bb);

    base::pos_at_end(ctx, end_bb);

    let ty = then_val.prim().inner().clone();
    let mut values = vec![*then_val];
    let mut blocks = vec![then_bb];

    // Else If type failed to compile
    if elseif_vals.len() != 0 && elseif_vals.iter().any(|v| v.is_none()) {
        // TODO: Error
        return None;
    } else {
        let velseif = elseif_vals
            .iter()
            .map(|e| e.as_ref().unwrap())
            .collect::<Vec<_>>();

        // Else If types do not match if type
        if !velseif
            .iter()
            .fold(true, |a, b| a && b.prim().inner() == &ty)
        {
            // TODO: Error
            return None;
        } else {
            for (i, val) in velseif.iter().enumerate() {
                values.push(***val);
                blocks.push(elseif_bbs[i].0 .1);
            }
        }
    }

    // Else type does not match if type
    if else_val
        .as_ref()
        .map(|v| v.prim().inner().clone())
        .unwrap_or(ty.clone())
        != ty
    {
        // TODO: Error
        return None;
    } else if let Some(velse) = else_val {
        values.push(*velse);
        blocks.push(else_bb.unwrap());
    }

    if ty == PrimitiveType::Unit {
        return None;
    }

    match values.len() {
        0 => None,
        1 => Some(then_val),
        _ => {
            let phi = Phi::new(ctx.llvm_builder.as_ref(), ty.llvm_ty_in_ctx(ctx), "");

            phi.add_incomings(&mut values, &mut blocks);

            Some((phi.as_ref(), ty).into())
        }
    }
}
