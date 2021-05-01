use parser::ast::{IntSize, Type, TypeKind};

pub trait AatbeSizeOf {
    fn size_of(&self) -> usize;
    fn smallest(&self) -> usize;
}

impl AatbeSizeOf for Type {
    fn size_of(&self) -> usize {
        match self {
            Type::UInt(IntSize::Bits8) => 1,
            Type::UInt(IntSize::Bits16) => 2,
            Type::UInt(IntSize::Bits32) => 4,
            Type::UInt(IntSize::Bits64) => 8,
            Type::Int(IntSize::Bits8) => 1,
            Type::Int(IntSize::Bits16) => 2,
            Type::Int(IntSize::Bits32) => 4,
            Type::Int(IntSize::Bits64) => 8,
            Type::Str => 8, // TODO: Platform specific pointer size
            Type::Pointer(_) => 8, // FIXME: Platform specific pointer size
            Type::Box(_) => 8, // FIXME: Platform specific pointer size
            Type::Bool => 1,
            Type::Char => 1,
            _ => unimplemented!("{:?}", self),
        }
    }

    fn smallest(&self) -> usize {
        unimplemented!()
    }
}

impl AatbeSizeOf for TypeKind {
    fn size_of(&self) -> usize {
        match self {
            TypeKind::Newtype(ty) => ty.size_of(),
            TypeKind::Variant(_, Some(types)) => {
                types.iter().map(|ty| ty.size_of()).sum::<usize>() + 1
            }
            TypeKind::Variant(_, None) => 1,
        }
    }

    fn smallest(&self) -> usize {
        match self {
            TypeKind::Newtype(ty) => ty.size_of(),
            TypeKind::Variant(_, Some(types)) => types.iter().map(|ty| ty.size_of()).min().unwrap(),
            TypeKind::Variant(_, None) => 1,
        }
    }
}
