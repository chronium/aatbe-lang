use llvm_sys_wrapper::{Builder, LLVMValueRef};

use crate::codegen::{AatbeModule, ValueTypePair};
use parser::ast::{BindType, PrimitiveType};

pub mod function;
pub use function::{
    codegen_function, declare_and_compile_function, declare_function, inject_function_in_scope,
};

pub mod variable;
pub use variable::{alloc_variable, init_record, store_value};

pub mod module;
pub use module::{FunctionVisibility, Message, ModuleContext, ModuleUnit, Query, QueryResponse};

pub mod decl;
pub use decl::decl;

pub mod cg;
pub use cg::cg;

#[derive(Debug, Clone)]
pub enum Mutability {
    Immutable,
    Mutable,
    Constant,
    Global,
}

impl From<&BindType> for Mutability {
    fn from(ty: &BindType) -> Self {
        match ty {
            BindType::Constant => Mutability::Constant,
            BindType::Immutable => Mutability::Immutable,
            BindType::Mutable => Mutability::Mutable,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Slot {
    FunctionArgument(LLVMValueRef, PrimitiveType),
    Variable {
        mutable: Mutability,
        name: String,
        ty: PrimitiveType,
        value: LLVMValueRef,
    },
}

impl Into<ValueTypePair> for &Slot {
    fn into(self) -> ValueTypePair {
        match self {
            Slot::FunctionArgument(arg, ty) => (*arg, ty).into(),
            Slot::Variable {
                mutable: _,
                name: _,
                ty,
                value,
            } => (*value, ty).into(),
        }
    }
}

impl Into<LLVMValueRef> for &Slot {
    fn into(self) -> LLVMValueRef {
        match self {
            Slot::FunctionArgument(arg, _) => *arg,
            Slot::Variable { value, .. } => *value,
        }
    }
}

impl Into<LLVMValueRef> for Slot {
    fn into(self) -> LLVMValueRef {
        match self {
            Slot::FunctionArgument(arg, _) => arg,
            Slot::Variable { value, .. } => value,
        }
    }
}

impl Slot {
    pub fn get_aggregate_name(&self) -> Option<String> {
        match self {
            Slot::Variable {
                ty:
                    PrimitiveType::TypeRef(rec)
                    | PrimitiveType::NamedType {
                        name: _,
                        ty: Some(box PrimitiveType::TypeRef(rec)),
                    },
                ..
            } => Some(rec.clone()),
            Slot::FunctionArgument(
                _arg,
                PrimitiveType::TypeRef(rec)
                | PrimitiveType::Pointer(box PrimitiveType::TypeRef(rec)),
            ) => Some(rec.clone()),
            Slot::Variable {
                ty:
                    PrimitiveType::VariantType(name)
                    | PrimitiveType::NamedType {
                        name: _,
                        ty: Some(box PrimitiveType::VariantType(name)),
                    },
                ..
            } => Some(name.clone()),
            _ => None,
        }
    }

    pub fn get_index(&self, module: &AatbeModule, name: &String) -> Option<(u32, PrimitiveType)> {
        match self {
            Slot::Variable {
                ty:
                    PrimitiveType::NamedType {
                        name: _,
                        ty: Some(box PrimitiveType::TypeRef(record)),
                    },
                ..
            } => match module.typectx_ref().get_record(record) {
                Ok(rec) => rec.get_field_index_ty(name),
                _ => panic!("Cannot get index from {:?}", self),
            },
            _ => panic!("Cannot get index from {:?}", self),
        }
    }

    pub fn load_var(&self, builder: &Builder) -> LLVMValueRef {
        match self {
            Slot::Variable {
                ty: PrimitiveType::Array { .. },
                ..
            }
            | Slot::Variable {
                ty:
                    PrimitiveType::NamedType {
                        ty: Some(box PrimitiveType::Array { .. }),
                        ..
                    },
                ..
            } => self.into(),
            Slot::Variable {
                ty: PrimitiveType::Slice { .. },
                ..
            }
            | Slot::Variable {
                ty:
                    PrimitiveType::NamedType {
                        ty: Some(box PrimitiveType::Slice { .. }),
                        ..
                    },
                ..
            } => {
                builder.build_extract_value(self.into(), 0);
                unimplemented!();
            }
            Slot::Variable { mutable, .. } => match mutable {
                Mutability::Constant => self.into(),
                _ => builder.build_load(self.into()),
            },
            Slot::FunctionArgument(_, PrimitiveType::Slice { .. }) => {
                builder.build_load(self.into())
            }
            Slot::FunctionArgument(_, _) => self.into(),
        }
    }

    pub fn var_ty(&self) -> &PrimitiveType {
        match self {
            Slot::Variable { ty, .. } => match ty {
                PrimitiveType::NamedType {
                    name: _,
                    ty: Some(ty),
                } => ty,
                ty => ty,
            },
            Slot::FunctionArgument(_, ty) => ty,
        }
    }

    pub fn get_mutability(&self) -> &Mutability {
        match self {
            Slot::Variable { mutable, .. } => mutable,
            Slot::FunctionArgument(_, PrimitiveType::Array { .. }) => &Mutability::Mutable,
            Slot::FunctionArgument(_, PrimitiveType::Slice { .. }) => &Mutability::Mutable,
            _ => panic!("ICE get_mutability: Not a variable {:?}", self),
        }
    }
}
