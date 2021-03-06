use parser::ast::{AtomKind, Boolean};

use crate::codegen::{
    builder::value, expr::const_expr::const_atom, unit::ModuleContext, ValueTypePair,
};

pub fn cg(atom: &AtomKind, ctx: &ModuleContext) -> Option<ValueTypePair> {
    match atom {
        AtomKind::Bool(Boolean::True) => Some(value::t(ctx)),
        AtomKind::Bool(Boolean::False) => Some(value::f(ctx)),
        atom @ (AtomKind::StringLiteral(_) | AtomKind::CharLiteral(_)) => const_atom(ctx, atom),
        _ => todo!("{:?}", atom),
    }
}
