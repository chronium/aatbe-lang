use parser::ast::{AtomKind, Expression, FloatSize, IntSize, PrimitiveType};

pub trait NameMangler {
    fn mangle(&self) -> String;
}

impl NameMangler for Expression {
    fn mangle(&self) -> String {
        match self {
            Expression::Function {
                name,
                ty,
                body: _,
                attributes,
            } => match ty {
                PrimitiveType::Function {
                    ext: false,
                    ret_ty: _,
                    params: _,
                } => {
                    if !attributes.contains(&String::from("entry")) {
                        format!("{}{}", name, ty.mangle())
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
            Expression::Atom(atom) => atom.mangle(),
            Expression::Call { name, args } => format!(
                "{} {}",
                name,
                args.iter()
                    .map(|arg| arg.mangle())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            _ => panic!("Cannot name mangle {:?}", self),
        }
    }
}

impl NameMangler for AtomKind {
    fn mangle(&self) -> String {
        match self {
            AtomKind::StringLiteral(lit) => format!("{:?}", lit),
            AtomKind::Ident(id) => id.clone(),
            AtomKind::SymbolLiteral(sym) => format!(":{}", sym),
            AtomKind::Floating(val, ty) => format!("{:?}{}", val, ty.mangle()),
            AtomKind::Integer(val, ty) => format!("{:?}{}", val, ty.mangle()),
            AtomKind::Access(arr) => arr.join("."),
            AtomKind::Parenthesized(val) => format!("({})", val.mangle()),
            _ => panic!("ICE mangle {:?}", self),
        }
    }
}

impl NameMangler for PrimitiveType {
    fn mangle(&self) -> String {
        match self {
            PrimitiveType::TypeRef(ty) => ty.clone(),
            PrimitiveType::Function {
                ext: _,
                ret_ty: _,
                params,
            } => {
                let params_mangled = params
                    .iter()
                    .map(|p| p.mangle())
                    .filter(|m| !m.is_empty())
                    .collect::<Vec<String>>()
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
            } => ty.mangle(),
            PrimitiveType::Str => String::from("s"),
            PrimitiveType::Int(size) => format!("i{}", size.mangle()),
            PrimitiveType::UInt(size) => format!("u{}", size.mangle()),
            PrimitiveType::Float(size) => format!("f{}", size.mangle()),
            PrimitiveType::Bool => String::from("b"),
            PrimitiveType::Char => String::from("c"),
            PrimitiveType::Ref(r) => format!("R{}", r.mangle()),
            PrimitiveType::Array { ty, len: Some(_) } => format!("A{}", ty.mangle()),
            _ => panic!("Cannot name mangle {:?}", self),
        }
    }
}

impl NameMangler for IntSize {
    fn mangle(&self) -> String {
        match self {
            IntSize::Bits8 => String::from("8"),
            IntSize::Bits16 => String::from("16"),
            IntSize::Bits32 => String::from("32"),
            IntSize::Bits64 => String::from("64"),
        }
    }
}

impl NameMangler for FloatSize {
    fn mangle(&self) -> String {
        match self {
            FloatSize::Bits32 => String::from("32"),
            FloatSize::Bits64 => String::from("64"),
        }
    }
}
