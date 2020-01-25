use parser::ast::{Expression, IntSize, PrimitiveType};

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
                        format!("{}__{}", name, ty.mangle())
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
                format!("{}", params_mangled)
            }
            /* Leave out until type inference
            PrimitiveType::Unit => String::from("U"),
            PrimitiveType::NamedType { name: _, ty } => ty.mangle(),
            PrimitiveType::Str => String::from("str"),
            PrimitiveType::Int(size) => format!("i{}", size.mangle()),
            PrimitiveType::UInt(size) => format!("u{}", size.mangle()),
            PrimitiveType::Function {
                ext: _,
                ret_ty,
                params,
            } => {
                let params_mangled = params
                    .iter()
                    .map(|p| p.mangle())
                    .collect::<Vec<String>>()
                    .join(".");
                format!("{}_{}", params_mangled, ret_ty.mangle())
            }*/
            //_ => panic!("Cannot name mangle {:?}", self),
            _ => String::new(),
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
