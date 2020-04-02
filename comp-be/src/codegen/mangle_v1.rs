use crate::codegen::AatbeModule;

use parser::ast::{AtomKind, Expression, FloatSize, IntSize, PrimitiveType};

pub trait NameMangler {
    fn mangle(&self, module: &AatbeModule) -> String;
}

impl NameMangler for Expression {
    fn mangle(&self, module: &AatbeModule) -> String {
        match self {
            Expression::Function {
                name,
                ty,
                body: _,
                attributes,
                type_names,
            } => match ty {
                PrimitiveType::Function {
                    ext: false,
                    ret_ty: _,
                    params: _,
                } => {
                    if !attributes.contains(&String::from("entry")) {
                        format!(
                            "{}{}{}",
                            name,
                            if type_names.len() > 0 {
                                format!("G{}", type_names.len())
                            } else {
                                String::default()
                            },
                            ty.mangle(module),
                        )
                    } else {
                        name.clone()
                    }
                }
                PrimitiveType::Function {
                    ext: true,
                    ret_ty: _,
                    params: _,
                } => name.clone(),
                _ => panic!("ICE non function type mangle"),
            },
            _ => panic!("Cannot name mangle {:?}", self),
        }
    }
}

impl NameMangler for AtomKind {
    fn mangle(&self, module: &AatbeModule) -> String {
        match self {
            AtomKind::StringLiteral(lit) => format!("{:?}", lit),
            AtomKind::Ident(id) => id.clone(),
            AtomKind::SymbolLiteral(sym) => format!(":{}", sym),
            AtomKind::Floating(val, ty) => format!("{:?}{}", val, ty.mangle(module)),
            AtomKind::Integer(val, ty) => format!("{:?}{}", val, ty.mangle(module)),
            AtomKind::Access(arr) => arr.join("."),
            AtomKind::Parenthesized(val) => format!("{}", val.mangle(module)),
            AtomKind::Ref(val) => format!("&{}", val.mangle(module)),
            _ => panic!("ICE mangle {:?}", self),
        }
    }
}

impl NameMangler for PrimitiveType {
    fn mangle(&self, module: &AatbeModule) -> String {
        match self {
            PrimitiveType::TypeRef(ty) => ty.clone(),
            PrimitiveType::Function {
                ext: _,
                ret_ty: _,
                params,
            } => {
                let params_mangled = params
                    .iter()
                    .map(|p| p.mangle(module))
                    .filter(|m| !m.is_empty())
                    .collect::<Vec<_>>()
                    .join(".");
                if !params_mangled.is_empty() {
                    format!("A{}", params_mangled)
                } else {
                    String::new()
                }
            }
            // TODO: Handle Unit
            PrimitiveType::Unit => String::new(),
            PrimitiveType::NamedType {
                name: _,
                ty: Some(ty),
            } => ty.mangle(module),
            PrimitiveType::Str => String::from("s"),
            PrimitiveType::Int(size) => format!("i{}", size.mangle(module)),
            PrimitiveType::UInt(size) => format!("u{}", size.mangle(module)),
            PrimitiveType::Float(size) => format!("f{}", size.mangle(module)),
            PrimitiveType::Bool => String::from("b"),
            PrimitiveType::Char => String::from("c"),
            PrimitiveType::Ref(r) => format!("R{}", r.mangle(module)),
            PrimitiveType::Array { ty, len: _ } => format!("A{}", ty.mangle(module)),
            PrimitiveType::Slice { ty } => format!("S{}", ty.mangle(module)),
            PrimitiveType::Symbol(name) => format!("N{}", name),
            PrimitiveType::VariantType(name) => format!(
                "{}",
                module
                    .typectx_ref()
                    .get_variant(name)
                    .expect(format!("Cannot find variant {}", name).as_str())
                    .parent_name
            ),
            PrimitiveType::Variant { parent, .. } => parent.clone(),
            _ => panic!("Cannot name mangle {:?}", self),
        }
    }
}

impl NameMangler for IntSize {
    fn mangle(&self, _module: &AatbeModule) -> String {
        match self {
            IntSize::Bits8 => String::from("8"),
            IntSize::Bits16 => String::from("16"),
            IntSize::Bits32 => String::from("32"),
            IntSize::Bits64 => String::from("64"),
        }
    }
}

impl NameMangler for FloatSize {
    fn mangle(&self, _module: &AatbeModule) -> String {
        match self {
            FloatSize::Bits32 => String::from("32"),
            FloatSize::Bits64 => String::from("64"),
        }
    }
}
