use parser::ast::{Expression, LoopType, PrimitiveType};

use guard::guard;

use crate::codegen::{
    builder::{base, branch},
    unit::{cg::expr, CompilerContext},
    ValueTypePair,
};

pub fn cg(expr: &Expression, ctx: &CompilerContext) -> Option<ValueTypePair> {
    guard!(let Expression::Loop {
        loop_type,
        cond_expr,
        body
    } = expr else { unreachable!() });

    let cond_bb = ctx.basic_block("");
    let body_bb = ctx.basic_block("");
    let end_bb = ctx.basic_block("");

    branch::branch(ctx, cond_bb);
    base::pos_at_end(ctx, cond_bb);
    let cond = expr::cg(cond_expr, ctx)?;

    if *cond.prim().inner() != PrimitiveType::Bool {
        // TODO: Error
        /*self.add_error(CompileError::ExpectedType {
            expected_ty: PrimitiveType::Bool.fmt(),
            found_ty: cond.prim().fmt(),
            value: cond_expr.fmt(),
        });*/
    };

    match loop_type {
        LoopType::While => branch::cond_branch(ctx, *cond, body_bb, end_bb),
        LoopType::Until => branch::cond_branch(ctx, *cond, end_bb, body_bb),
    };

    base::pos_at_end(ctx, body_bb);
    expr::cg(body, ctx);

    branch::branch(ctx, cond_bb);
    base::pos_at_end(ctx, end_bb);

    None
}
