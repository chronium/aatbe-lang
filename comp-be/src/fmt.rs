use parser::ast::{AtomKind, Boolean, Expression, IntSize, LValue, PrimitiveType};

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
            PrimitiveType::Varargs => String::from("..."),
            PrimitiveType::NamedType { name: _, ty } => ty.clone().fmt(),
            PrimitiveType::Pointer(ty) => format!("{}*", ty.clone().fmt()),
            PrimitiveType::Char => String::from("char"),
            PrimitiveType::TypeRef(ty) => ty.clone(),
            _ => panic!("ICE fmt {:?}", self),
        }
    }
}

impl AatbeFmt for &AtomKind {
    fn fmt(self) -> String {
        match self {
            AtomKind::StringLiteral(lit) => format!("{:?}", lit),
            AtomKind::Integer(val, ty) => format!("{}{}", val, ty.fmt()),
            AtomKind::Bool(Boolean::True) => String::from("true"),
            AtomKind::Bool(Boolean::False) => String::from("false"),
            AtomKind::Ident(id) => format!("{}", id),
            AtomKind::Unary(op, id) => format!("{}{}", op, id.fmt()),
            AtomKind::Parenthesized(expr) => format!("({})", expr.fmt()),
            AtomKind::NamedValue { name, val } => format!("{}: {}", name, val.fmt()),
            _ => panic!("ICE fmt {:?}", self),
        }
    }
}

impl AatbeFmt for &Expression {
    fn fmt(self) -> String {
        match self {
            Expression::Binary(lhs, op, rhs) => format!("{} {} {}", lhs.fmt(), op, rhs.fmt()),
            Expression::Atom(atom) => atom.fmt(),
            Expression::RecordInit { record, values } => format!(
                "{} {{ {} }}",
                record,
                values
                    .iter()
                    .map(|val| val.fmt())
                    .collect::<Vec<String>>()
                    .join(", ")
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
