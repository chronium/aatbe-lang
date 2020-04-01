use parser::ast::{AtomKind, Boolean, Expression, FloatSize, IntSize, LValue, PrimitiveType};

pub trait AatbeFmt {
    fn fmt(self) -> String;
}

impl AatbeFmt for PrimitiveType {
    fn fmt(self) -> String {
        (&self).fmt().clone()
    }
}

impl AatbeFmt for &PrimitiveType {
    fn fmt(self) -> String {
        match self {
            PrimitiveType::Str => String::from("str"),
            PrimitiveType::Bool => String::from("bool"),
            PrimitiveType::Int(bits) => match bits {
                IntSize::Bits8 => String::from("i8"),
                IntSize::Bits16 => String::from("i16"),
                IntSize::Bits32 => String::from("i32"),
                IntSize::Bits64 => String::from("i64"),
            },
            PrimitiveType::UInt(bits) => match bits {
                IntSize::Bits8 => String::from("u8"),
                IntSize::Bits16 => String::from("u16"),
                IntSize::Bits32 => String::from("u32"),
                IntSize::Bits64 => String::from("u64"),
            },
            PrimitiveType::Float(bits) => match bits {
                FloatSize::Bits32 => String::from("f32"),
                FloatSize::Bits64 => String::from("f64"),
            },
            PrimitiveType::Varargs => String::from("..."),
            PrimitiveType::NamedType {
                name: _,
                ty: Some(ty),
            } => ty.clone().fmt(),
            PrimitiveType::Pointer(ty) => format!("{}*", ty.clone().fmt()),
            PrimitiveType::Char => String::from("char"),
            PrimitiveType::TypeRef(ty) => ty.clone(),
            PrimitiveType::Array { ty, len } => {
                format!("{}[{}]", ty.clone().fmt(), len.to_string())
            }
            PrimitiveType::Unit => String::from("()"),
            PrimitiveType::Ref(ty) => format!("&{}", ty.clone().fmt()),
            PrimitiveType::Function { ret_ty, .. } => ret_ty.clone().fmt(),
            PrimitiveType::Slice { ty } => format!("{}[]", ty.clone().fmt()),
            PrimitiveType::GenericTypeRef(name, types) => format!(
                "{}[{}]",
                name,
                types
                    .iter()
                    .map(|ty| ty.fmt())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            PrimitiveType::Newtype(name) => name.clone(),
            PrimitiveType::VariantType(name) => name.clone(),
            _ => panic!("ICE fmt {:?}", self),
        }
    }
}

impl AatbeFmt for &AtomKind {
    fn fmt(self) -> String {
        match self {
            AtomKind::StringLiteral(lit) => format!("\"{}\"", lit),
            AtomKind::CharLiteral(lit) => format!("{}", lit),
            AtomKind::Integer(val, ty) => format!("{}{}", val, ty.fmt()),
            AtomKind::Floating(val, ty) => format!("{}{}", val, ty.fmt()),
            AtomKind::Bool(Boolean::True) => String::from("true"),
            AtomKind::Bool(Boolean::False) => String::from("false"),
            AtomKind::Ident(id) => format!("{}", id),
            AtomKind::Unary(op, id) => format!("{}{}", op, id.fmt()),
            AtomKind::Parenthesized(expr) => format!("({})", expr.fmt()),
            AtomKind::Cast(val, ty) => format!("{} as {}", val.fmt(), ty.fmt()),
            AtomKind::NamedValue { name, val } => format!("{}: {}", name, val.fmt()),
            AtomKind::Access(list) => list.join("."),
            AtomKind::Array(vals) => format!(
                "[{}]",
                vals.iter()
                    .map(|val| val.fmt())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            AtomKind::Index(lval, index) => format!("{}[{}]", lval.fmt(), index.fmt()),
            AtomKind::Ref(val) => format!("&{}", val.fmt()),
            AtomKind::Deref(val) => format!("*{}", val.fmt()),
            _ => panic!("ICE fmt {:?}", self),
        }
    }
}

impl AatbeFmt for &Expression {
    fn fmt(self) -> String {
        match self {
            Expression::Binary(lhs, op, rhs) => format!("{} {} {}", lhs.fmt(), op, rhs.fmt()),
            Expression::Atom(atom) => atom.fmt(),
            Expression::RecordInit {
                record,
                types,
                values,
            } => format!(
                "{}{} {{ {} }}",
                record,
                if types.len() > 0 {
                    format!(
                        "[{}]",
                        types
                            .iter()
                            .map(|val| val.fmt())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                } else {
                    String::default()
                },
                values
                    .iter()
                    .map(|val| val.fmt())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Expression::Call { name, types, args } => format!(
                "{}{} {}",
                name,
                if types.len() > 0 {
                    format!(
                        "[{}]",
                        types
                            .iter()
                            .map(|val| val.fmt())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                } else {
                    String::default()
                },
                args.iter()
                    .map(|val| val.fmt())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Expression::Function {
                name,
                ty:
                    PrimitiveType::Function {
                        ext,
                        ret_ty: box ret_ty,
                        params,
                    },
                ..
            } => format!(
                "{}fn {} {} -> {}",
                if *ext { "ext " } else { "" },
                name,
                params
                    .iter()
                    .map(|val| val.fmt())
                    .collect::<Vec<_>>()
                    .join(", "),
                ret_ty.fmt(),
            ),
            _ => panic!("ICE fmt {:?}", self),
        }
    }
}

impl AatbeFmt for &LValue {
    fn fmt(self) -> String {
        match self {
            LValue::Ident(ident) => ident.clone(),
            LValue::Accessor(access) => access.join("."),
            LValue::Deref(lval) => format!("*{}", lval.fmt()),
            LValue::Index(lval, expr) => format!("{}[{}]", lval.fmt(), expr.fmt()),
        }
    }
}
