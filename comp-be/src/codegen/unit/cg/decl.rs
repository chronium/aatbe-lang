use parser::ast::{Expression, PrimitiveType};

use guard::guard;

use crate::codegen::{
    unit::{alloc_variable, CompilerContext, Query, QueryResponse},
    ValueTypePair,
};

pub fn cg(expr: &Expression, ctx: &CompilerContext) -> Option<ValueTypePair> {
    match expr {
        Expression::Decl {
            ty: PrimitiveType::NamedType { name, ty: Some(ty) },
            value: _,
            exterior_bind: _,
        } => {
            alloc_variable(expr, ctx);

            guard!(let QueryResponse::Slot(slot) = ctx.query(Query::Slot(name)) else { unreachable!() });
            let slot = slot?;
            Some((slot.into(), *ty.clone()).into())
        }
        Expression::Decl {
            ty: PrimitiveType::NamedType { name, ty: None },
            value,
            exterior_bind: _,
        } => {
            if value.is_none() {
                todo!("ERROR")
            }

            alloc_variable(expr, ctx).and_then(|ty| {
                guard!(let QueryResponse::Slot(slot) = ctx.query(Query::Slot(name)) else { unreachable!() });
                let slot = slot?;
                Some((slot.into(), ty).into())
            })
        }
        _ => unreachable!(),
    }
}
