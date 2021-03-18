use parser::ast::{AtomKind, PrimitiveType};

use crate::codegen::{builder::value, unit::CompilerContext, ValueTypePair};

pub fn cg(atom: &AtomKind, ctx: &CompilerContext) -> Option<ValueTypePair> {
    match atom {
        AtomKind::Integer(val, PrimitiveType::Int(size)) => {
            Some(value::sint(ctx, size.clone(), *val))
        }
        AtomKind::Integer(val, PrimitiveType::UInt(size)) => {
            Some(value::uint(ctx, size.clone(), *val))
        }
        AtomKind::Floating(val, PrimitiveType::Float(size)) => {
            Some(value::floating(ctx, size.clone(), *val))
        }
        AtomKind::Unary(op, box AtomKind::Integer(val, PrimitiveType::Int(size)))
            if op == &String::from("-") =>
        {
            Some(value::sint(ctx, size.clone(), (-(*val as i64)) as u64))
        }
        AtomKind::Unary(op, box AtomKind::Integer(val, PrimitiveType::UInt(size)))
            if op == &String::from("-") =>
        {
            Some(value::uint(ctx, size.clone(), (-(*val as i64)) as u64))
        }
        _ => panic!("ICE Atom {:?}", atom),
    }
}
