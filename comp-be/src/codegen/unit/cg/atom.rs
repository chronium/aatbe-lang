use parser::ast::{AtomKind, Boolean, PrimitiveType};

use guard::guard;

use crate::{
    codegen::{
        builder::value,
        expr::const_atom,
        unit::{
            cg::{consts, expr},
            CompilerContext, Query, QueryResponse,
        },
        ValueTypePair,
    },
    fmt::AatbeFmt,
};

pub fn cg(atom: &AtomKind, ctx: &CompilerContext) -> Option<ValueTypePair> {
    ctx.trace(format!("Atom {}", atom.fmt()));
    match atom {
        AtomKind::Parenthesized(expr) => expr::cg(expr, ctx),
        AtomKind::Unit => Some(value::unit(ctx)),
        AtomKind::Bool(Boolean::True) => Some(value::t(ctx)),
        AtomKind::Bool(Boolean::False) => Some(value::f(ctx)),
        atom
        @
        (AtomKind::Integer(..)
        | AtomKind::Floating(..)
        | AtomKind::Unary(_, box AtomKind::Integer(..))) => consts::numeric::cg(atom, ctx),
        atom @ (AtomKind::StringLiteral(..) | AtomKind::CharLiteral(..)) => const_atom(ctx, atom),
        AtomKind::Ident(name) => {
            guard!(let QueryResponse::Slot(slot) = ctx.query(Query::Slot(name)) else { unreachable!(); });
            let slot = slot?;
            match slot.var_ty().clone() {
                ty @ PrimitiveType::Newtype(_) | ty @ PrimitiveType::VariantType(_) => {
                    let val: ValueTypePair = slot.into();
                    Some((*val, ty).into())
                }
                ty => Some((slot.load_var(ctx.llvm_builder), ty).into()),
            }
        }
        _ => todo!("{:?}", atom),
    }
}
