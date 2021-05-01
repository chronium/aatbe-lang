use guard::guard;
use parser::ast::Expression;

use crate::codegen::{
    unit::{store_value, CompilerContext},
    ValueTypePair,
};

pub fn cg(expr: &Expression, ctx: &CompilerContext) -> Option<ValueTypePair> {
    guard!(let Expression::Assign { lval, value } = expr else { unreachable!() });

    match value {
        box Expression::RecordInit {
            record,
            types,
            values,
        } => todo!(),
        _ => store_value(ctx, lval, value),
    }
}
