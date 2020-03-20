pub mod atom;
pub mod call;
pub mod conditional;
pub mod expr;
pub mod mangle_v1;
pub mod module;
pub mod scope;
pub mod unit;

pub use expr::*;
pub use module::AatbeModule;
pub use scope::Scope;
pub use unit::CodegenUnit;

pub type GenRes = Result<ValueTypePair, CompileError>;

pub enum CompileError {
    NoGenericFunction {
        function: String,
    },
    NoGenericRecord {
        rec: String,
    },
    ExpectedReturn {
        function: String,
        ty: String,
    },
    ArrayTypesNotUniform {
        values: String,
    },
    ExpectedType {
        expected_ty: String,
        found_ty: String,
        value: String,
    },
    MismatchedArguments {
        function: String,
        expected_ty: String,
        found_ty: String,
    },
    UnknownFunction {
        name: String,
        values: String,
    },
    UnaryMismatch {
        op: String,
        expected_ty: String,
        found_ty: String,
        value: String,
    },
    BinaryMismatch {
        op: String,
        types: (String, String),
        values: (String, String),
    },
    OpMismatch {
        op: String,
        types: (String, String),
        values: (String, String),
    },
    AssignMismatch {
        expected_ty: String,
        found_ty: String,
        value: String,
        var: String,
    },
    StoreMismatch {
        expected_ty: String,
        found_ty: String,
        value: String,
        lval: String,
    },
    NotIndexable {
        ty: String,
        lval: String,
    },
    ExpectedValue {
        name: String,
    },
    Handled,
}

use crate::ty::TypeKind;
use llvm_sys_wrapper::LLVMValueRef;
use parser::ast::PrimitiveType;

pub struct ValueTypePair(LLVMValueRef, TypeKind);

impl From<(LLVMValueRef, TypeKind)> for ValueTypePair {
    fn from((val, ty): (LLVMValueRef, TypeKind)) -> ValueTypePair {
        ValueTypePair(val, ty)
    }
}

impl From<(LLVMValueRef, PrimitiveType)> for ValueTypePair {
    fn from((val, ty): (LLVMValueRef, PrimitiveType)) -> ValueTypePair {
        ValueTypePair(val, TypeKind::Primitive(ty))
    }
}

impl From<ValueTypePair> for (LLVMValueRef, TypeKind) {
    fn from(vtp: ValueTypePair) -> (LLVMValueRef, TypeKind) {
        (vtp.0, vtp.1)
    }
}

impl ValueTypePair {
    pub fn prim(&self) -> &PrimitiveType {
        match self {
            ValueTypePair(
                _,
                TypeKind::Primitive(PrimitiveType::NamedType {
                    name: _,
                    ty: Some(ty),
                }),
            ) => ty,
            ValueTypePair(_, TypeKind::Primitive(prim)) => prim,
            _ => panic!("ICE prim {:?}"),
        }
    }

    pub fn ty(&self) -> TypeKind {
        TypeKind::Primitive(self.prim().clone())
    }

    pub fn indexable(&self, module: &AatbeModule) -> Option<ValueTypePair> {
        match &self {
            ValueTypePair(val, TypeKind::Primitive(prim)) => match prim {
                prim @ (PrimitiveType::Str | PrimitiveType::Array { .. }) => {
                    Some((*val, prim.clone()).into())
                }
                PrimitiveType::Slice { .. } => Some(
                    (
                        module.llvm_builder_ref().build_extract_value(*val, 0),
                        prim.clone(),
                    )
                        .into(),
                ),
                PrimitiveType::Pointer(box ty) => Some((*val, ty.clone()).into()),
                _ => None,
            },
            _ => None,
        }
    }
}

use std::ops::Deref;

impl Deref for ValueTypePair {
    type Target = LLVMValueRef;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
