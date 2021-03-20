use crate::codegen::{
    builder::{branch, core},
    unit::{cg::expr, CompilerContext, Message},
    ValueTypePair,
};
use llvm_sys_wrapper::LLVMBasicBlockRef;
use parser::ast::{Expression, PrimitiveType};

pub fn cg(expr: &Expression, ctx: &CompilerContext) -> Option<ValueTypePair> {
    if let Expression::If {
        cond_expr,
        then_expr,
        elseif_exprs,
        else_expr,
        is_expr,
    } = expr
    {
        if *is_expr {
            todo!("If ret");
        }

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
        let end_bb = ctx.basic_block("end");

        ctx.dispatch(Message::EnterAnonymousScope);
        ctx.trace(format!("If {:?}", cond_expr));
        let cond = expr::cg(cond_expr, ctx)?;

        if *cond.prim().inner() != PrimitiveType::Bool {
            todo!("NOT A BOOL");
        }

        let next_bb = |i: usize| -> LLVMBasicBlockRef {
            elseif_bbs
                .get(i)
                .map(|e| e.0 .0)
                .unwrap_or(else_bb.unwrap_or(end_bb))
        };

        branch::cond_branch(ctx, *cond, then_bb, next_bb(0));

        let elseif_vals = elseif_bbs
            .iter()
            .enumerate()
            .map(|(i, ((bb_cond, bb_body), (cond, expr)))| {
                ctx.trace(format!("Else If {:?}", cond));
                core::pos_at_end(ctx, *bb_cond);

                let cond = expr::cg(cond, ctx)?;
                branch::cond_branch(ctx, *cond, *bb_body, next_bb(i + 1));

                core::pos_at_end(ctx, *bb_body);
                let val = expr::cg(expr, ctx);
                branch::branch(ctx, end_bb);

                val
            })
            .collect::<Vec<_>>();

        core::pos_at_end(ctx, then_bb);
        let then_val = expr::cg(then_expr, ctx);

        let else_val = else_bb
            .map(|bb| {
                ctx.trace("Else".to_string());
                branch::branch(ctx, end_bb);
                core::pos_at_end(ctx, bb);

                else_expr.as_ref().and_then(|expr| expr::cg(&expr, ctx))
            })
            .flatten();

        ctx.dispatch(Message::ExitScope);

        branch::branch(ctx, end_bb);

        core::pos_at_end(ctx, end_bb);
        None
    } else {
        panic!("ICE")
    }
}
