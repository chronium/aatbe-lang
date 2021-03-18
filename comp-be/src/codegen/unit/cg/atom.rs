use parser::ast::{AtomKind, Boolean};

use crate::{
    codegen::{
        builder::value,
        expr::const_atom,
        unit::{
            cg::{consts, expr},
            CompilerContext,
        },
        ValueTypePair,
    },
    fmt::AatbeFmt,
};

pub fn cg(atom: &AtomKind, ctx: &CompilerContext) -> Option<ValueTypePair> {
    ctx.trace(format!("Atom {}", atom.fmt()));
    match atom {
        AtomKind::Parenthesized(expr) => expr::cg(expr, ctx),
        AtomKind::Unit => None,
        AtomKind::Bool(Boolean::True) => Some(value::t(ctx)),
        AtomKind::Bool(Boolean::False) => Some(value::f(ctx)),
        atom
        @
        (AtomKind::Integer(..)
        | AtomKind::Floating(..)
        | AtomKind::Unary(_, box AtomKind::Integer(..))) => consts::numeric::cg(atom, ctx),
        atom @ (AtomKind::StringLiteral(..) | AtomKind::CharLiteral(..)) => const_atom(ctx, atom),
        _ => todo!("{:?}", atom),
    }
}
