use parser::ast::{
    AtomKind, Boolean, Expression, FloatSize, FunctionType, IdentPath, IntSize, LValue,
    Type,
};

pub trait AatbeFmt {
    fn fmt(self) -> String;
}

impl AatbeFmt for &IdentPath {
    fn fmt(self) -> String {
        match self {
            IdentPath::Local(path) => path.clone(),
            IdentPath::Module(path) => path.join("::"),
            IdentPath::Root(path) => format!("::{}", path.join("::")),
        }
    }
}

impl AatbeFmt for Type {
    fn fmt(self) -> String {
        (&self).fmt()
    }
}

impl AatbeFmt for FunctionType {
    fn fmt(self) -> String {
        (&self).fmt()
    }
}

impl AatbeFmt for &FunctionType {
    fn fmt(self) -> String {
        let params = if self.params.len() == 1 && self.params[0] == Type::Unit {
            String::from("()")
        } else {
            format!(
                "{}",
                self.params
                    .iter()
                    .map(|p| p.fmt())
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        };

        let ret_ty = format!(" -> {}", (&*self.ret_ty).fmt());

        format!("{}{}", params, ret_ty)
    }
}

impl AatbeFmt for &Type {
    fn fmt(self) -> String {
        match self {
            Type::Str => String::from("str"),
            Type::Bool => String::from("bool"),
            Type::Int(bits) => match bits {
                IntSize::Bits8 => String::from("i8"),
                IntSize::Bits16 => String::from("i16"),
                IntSize::Bits32 => String::from("i32"),
                IntSize::Bits64 => String::from("i64"),
            },
            Type::UInt(bits) => match bits {
                IntSize::Bits8 => String::from("u8"),
                IntSize::Bits16 => String::from("u16"),
                IntSize::Bits32 => String::from("u32"),
                IntSize::Bits64 => String::from("u64"),
            },
            Type::Float(bits) => match bits {
                FloatSize::Bits32 => String::from("f32"),
                FloatSize::Bits64 => String::from("f64"),
            },
            Type::Varargs => String::from("..."),
            Type::NamedType {
                name: _,
                ty: Some(ty),
            } => ty.clone().fmt(),
            Type::Pointer(ty) => format!("{}*", ty.clone().fmt()),
            Type::Char => String::from("char"),
            Type::TypeRef(ty) => ty.clone(),
            Type::Array { ty, len } => {
                format!("{}[{}]", ty.clone().fmt(), len.to_string())
            }
            Type::Unit => String::from("()"),
            Type::Ref(ty) => format!("&{}", ty.clone().fmt()),
            Type::Function(func) => func.fmt(),
            Type::Slice { ty } => format!("{}[]", ty.clone().fmt()),
            Type::GenericTypeRef(name, types) => format!(
                "{}[{}]",
                name,
                types
                    .iter()
                    .map(|ty| ty.fmt())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Type::Newtype(name) => name.clone(),
            Type::VariantType(name) => name.clone(),
            _ => panic!("ICE fmt {:?}", self),
        }
    }
}

impl AatbeFmt for &AtomKind {
    fn fmt(self) -> String {
        match self {
            AtomKind::StringLiteral(lit) => format!("{:?}", lit),
            AtomKind::CharLiteral(lit) => format!("{:?}", lit),
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
            AtomKind::SymbolLiteral(sym) => format!(":{}", sym),
            AtomKind::Unit => format!("()"),
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
                name.fmt(),
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
                public,
                ty: ty @ FunctionType { ext, .. },
                ..
            } => format!(
                "{}{}fn {}{}",
                if *public { "public " } else { "" },
                if *ext { "ext " } else { "" },
                name,
                ty.fmt(),
            ),
            Expression::Assign {
                lval,
                value: box value,
            } => format!("{} = {}", lval.fmt(), value.fmt()),
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
