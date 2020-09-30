use parser::ast::{IntSize, PrimitiveType, TypeKind};

pub trait AatbeSizeOf {
    fn size_of(&self) -> usize;
    fn smallest(&self) -> usize;
}

impl AatbeSizeOf for PrimitiveType {
    fn size_of(&self) -> usize {
        match self {
            PrimitiveType::UInt(IntSize::Bits8) => 1,
            PrimitiveType::UInt(IntSize::Bits16) => 2,
            PrimitiveType::UInt(IntSize::Bits32) => 4,
            PrimitiveType::UInt(IntSize::Bits64) => 8,
            PrimitiveType::Int(IntSize::Bits8) => 1,
            PrimitiveType::Int(IntSize::Bits16) => 2,
            PrimitiveType::Int(IntSize::Bits32) => 4,
            PrimitiveType::Int(IntSize::Bits64) => 8,
            PrimitiveType::Str => 8, // TODO: Platform specific pointer size
            PrimitiveType::Pointer(_) => 8, // FIXME: Platform specific pointer size
            PrimitiveType::Box(_) => 8, // FIXME: Platform specific pointer size
            PrimitiveType::Bool => 1,
            PrimitiveType::Char => 1,
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
