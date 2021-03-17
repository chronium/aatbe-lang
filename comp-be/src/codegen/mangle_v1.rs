use parser::ast::{AtomKind, Expression, FloatSize, FunctionType, IntSize, PrimitiveType};

use super::unit::CompilerContext;
use crate::prefix;

pub trait NameMangler {
    fn mangle(&self, ctx: &CompilerContext) -> String;
}

impl NameMangler for Expression {
    fn mangle(&self, ctx: &CompilerContext) -> String {
        match self {
            Expression::Function {
                name,
                ty,
                body: _,
                attributes,
                type_names,
                export: _,
            } => match ty {
                FunctionType {
                    ext: false,
                    ret_ty: _,
                    params: _,
                } => {
                    if !attributes.contains(&String::from("entry")) {
                        format!(
                            "{}{}{}",
                            prefix!(ctx, name.clone()).join("__"),
                            if type_names.len() > 0 {
                                format!("G{}", type_names.len())
                            } else {
                                String::default()
                            },
                            ty.mangle(ctx),
                        )
                    } else {
                        name.clone()
                    }
                }
                FunctionType {
                    ext: true,
                    ret_ty: _,
                    params: _,
                } => name.clone(),
            },
            _ => panic!("Cannot name mangle {:?}", self),
        }
    }
}

impl NameMangler for AtomKind {
    fn mangle(&self, ctx: &CompilerContext) -> String {
        match self {
            AtomKind::StringLiteral(lit) => format!("{:?}", lit),
            AtomKind::Ident(id) => id.clone(),
            AtomKind::SymbolLiteral(sym) => format!(":{}", sym),
            AtomKind::Floating(val, ty) => format!("{:?}{}", val, ty.mangle(ctx)),
            AtomKind::Integer(val, ty) => format!("{:?}{}", val, ty.mangle(ctx)),
            AtomKind::Access(arr) => arr.join("."),
            AtomKind::Parenthesized(val) => format!("{}", val.mangle(ctx)),
            AtomKind::Ref(val) => format!("&{}", val.mangle(ctx)),
            _ => panic!("ICE mangle {:?}", self),
        }
    }
}

impl NameMangler for FunctionType {
    fn mangle(&self, ctx: &CompilerContext) -> String {
        let params_mangled = self
            .params
            .iter()
            .map(|p| p.mangle(ctx))
            .filter(|m| !m.is_empty())
            .collect::<Vec<_>>()
            .join(".");
        if !params_mangled.is_empty() {
            format!("A{}", params_mangled)
        } else {
            String::new()
        }
    }
}

impl NameMangler for PrimitiveType {
    fn mangle(&self, ctx: &CompilerContext) -> String {
        match self {
            PrimitiveType::TypeRef(ty) => ty.clone(),
            PrimitiveType::Function(ty) => ty.mangle(ctx),
            // TODO: Handle Unit
            PrimitiveType::Unit => String::new(),
            PrimitiveType::NamedType {
                name: _,
                ty: Some(ty),
            } => ty.mangle(ctx),
            PrimitiveType::Str => String::from("s"),
            PrimitiveType::Int(size) => format!("i{}", size.mangle(ctx)),
            PrimitiveType::UInt(size) => format!("u{}", size.mangle(ctx)),
            PrimitiveType::Float(size) => format!("f{}", size.mangle(ctx)),
            PrimitiveType::Bool => String::from("b"),
            PrimitiveType::Char => String::from("c"),
            PrimitiveType::Ref(r) => format!("R{}", r.mangle(ctx)),
            PrimitiveType::Pointer(p) => format!("P{}", p.mangle(ctx)),
            PrimitiveType::Array { ty, len: _ } => format!("A{}", ty.mangle(ctx)),
            PrimitiveType::Slice { ty } => format!("S{}", ty.mangle(ctx)),
            PrimitiveType::Symbol(name) => format!("N{}", name),
            /*PrimitiveType::VariantType(name) => format!(
                "{}",
                module
                    .typectx_ref()
                    .get_variant(name)
                    .expect(format!("Cannot find variant {}", name).as_str())
                    .parent_name
            ),*/
            PrimitiveType::Variant { parent, .. } => parent.clone(),
            _ => panic!("Cannot name mangle {:?}", self),
        }
    }
}

impl NameMangler for IntSize {
    fn mangle(&self, _ctx: &CompilerContext) -> String {
        match self {
            IntSize::Bits8 => String::from("8"),
            IntSize::Bits16 => String::from("16"),
            IntSize::Bits32 => String::from("32"),
            IntSize::Bits64 => String::from("64"),
        }
    }
}

impl NameMangler for FloatSize {
    fn mangle(&self, _ctx: &CompilerContext) -> String {
        match self {
            FloatSize::Bits32 => String::from("32"),
            FloatSize::Bits64 => String::from("64"),
        }
    }
}
