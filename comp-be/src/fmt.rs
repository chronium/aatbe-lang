use parser::ast::{IntSize, PrimitiveType};

pub trait AatbeFmt {
    fn fmt(self) -> String;
}

impl AatbeFmt for &PrimitiveType {
    fn fmt(self) -> String {
        match self {
            PrimitiveType::Int(bits) => match bits {
                IntSize::Bits8 => String::from("i8"),
                IntSize::Bits16 => String::from("i16"),
                IntSize::Bits32 => String::from("i32"),
                IntSize::Bits64 => String::from("i64"),
            },
            _ => panic!("ICE fmt {:?}", self),
        }
    }
}
