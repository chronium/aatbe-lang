use llvm_sys_wrapper::{Builder, Function, LLVMBasicBlockRef, LLVMValueRef};

use crate::codegen::{AatbeModule, ValueTypePair};
use parser::ast::{BindType, PrimitiveType};

pub mod function;
pub use function::{
    codegen_function, declare_and_compile_function, declare_function, inject_function_in_scope,
};

pub mod variable;
use std::fmt;
pub use variable::{alloc_variable, init_record, store_value};

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

#[derive(Clone)]
pub enum CodegenUnit {
    Function(Function, PrimitiveType),
    FunctionArgument(LLVMValueRef, PrimitiveType),
    Variable {
        mutable: Mutability,
        name: String,
        ty: PrimitiveType,
        value: LLVMValueRef,
    },
}

impl fmt::Debug for CodegenUnit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CodegenUnit::Function(_, prim) => write!(f, "Function({:?})", prim),
            CodegenUnit::FunctionArgument(_, prim) => write!(f, "FunctionArgument({:?})", prim),
            CodegenUnit::Variable {
                mutable,
                name,
                ty,
                value,
            } => write!(
                f,
                "FunctionArgument {{ mutable: {:?}, name: {:?}, ty: {:?}, value: {:?} }}",
                mutable, name, ty, value
            ),
        }
    }
}

impl Into<ValueTypePair> for &CodegenUnit {
    fn into(self) -> ValueTypePair {
        match self {
            CodegenUnit::Function(func, ty) => (func.as_ref(), ty.clone()).into(),
            CodegenUnit::FunctionArgument(arg, ty) => (*arg, ty.clone()).into(),
            CodegenUnit::Variable {
                mutable: _,
                name: _,
                ty,
                value,
            } => (*value, ty.clone()).into(),
        }
    }
}

impl Into<LLVMValueRef> for &CodegenUnit {
    fn into(self) -> LLVMValueRef {
        match self {
            CodegenUnit::Function(func, _) => func.as_ref(),
            CodegenUnit::FunctionArgument(arg, _) => *arg,
            CodegenUnit::Variable { value, .. } => *value,
        }
    }
}

impl Into<LLVMValueRef> for CodegenUnit {
    fn into(self) -> LLVMValueRef {
        match self {
            CodegenUnit::Function(func, _) => func.as_ref(),
            CodegenUnit::FunctionArgument(arg, _) => arg,
            CodegenUnit::Variable { value, .. } => value,
        }
    }
}

impl CodegenUnit {
    pub fn get_aggregate_name(&self) -> Option<String> {
        match self {
            CodegenUnit::Variable {
                ty:
                    PrimitiveType::TypeRef(rec)
                    | PrimitiveType::NamedType {
                        name: _,
                        ty: Some(box PrimitiveType::TypeRef(rec)),
                    },
                ..
            } => Some(rec.clone()),
            CodegenUnit::FunctionArgument(
                _arg,
                PrimitiveType::TypeRef(rec)
                | PrimitiveType::Pointer(box PrimitiveType::TypeRef(rec)),
            ) => Some(rec.clone()),
            CodegenUnit::Variable {
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

    pub fn ret_ty(&self) -> PrimitiveType {
        match self {
            CodegenUnit::Function(
                _,
                PrimitiveType::Function {
                    ext: _,
                    ret_ty: box ret_ty,
                    params: _,
                },
            ) => ret_ty.clone(),
            _ => panic!("ICE ret_ty {:?}", self),
        }
    }

    pub fn param_types(&self) -> Vec<PrimitiveType> {
        match self {
            CodegenUnit::Function(_, PrimitiveType::Function { params, .. }) => params
                .iter()
                .filter_map(|p| match p {
                    //TODO: Handle Unit
                    PrimitiveType::Unit => None,
                    PrimitiveType::NamedType {
                        name: _,
                        ty: Some(box ty),
                    } => Some(ty.clone()),
                    p => Some(p.clone()),
                })
                .collect::<Vec<_>>()
                .clone(),
            _ => panic!("ICE param_types {:?}", self),
        }
    }

    pub fn get_index(&self, module: &AatbeModule, name: &String) -> Option<(u32, PrimitiveType)> {
        match self {
            CodegenUnit::Variable {
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

    pub fn bb(&self, name: String) -> LLVMBasicBlockRef {
        match self {
            CodegenUnit::Function(func, _) => func.append_basic_block(name.as_ref()),
            _ => panic!("Cannot append basic block on {:?}", self),
        }
    }

    fn get_param(&self, index: u32) -> LLVMValueRef {
        match self {
            CodegenUnit::Function(func, _) => func.get_param(index),
            _ => panic!("Cannot get parameter from {:?}", self),
        }
    }

    pub fn load_var(&self, builder: &Builder) -> LLVMValueRef {
        match self {
            CodegenUnit::Variable {
                ty: PrimitiveType::Array { .. },
                ..
            }
            | CodegenUnit::Variable {
                ty:
                    PrimitiveType::NamedType {
                        ty: Some(box PrimitiveType::Array { .. }),
                        ..
                    },
                ..
            } => self.into(),
            CodegenUnit::Variable {
                ty: PrimitiveType::Slice { .. },
                ..
            }
            | CodegenUnit::Variable {
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
            CodegenUnit::Variable { mutable, .. } => match mutable {
                Mutability::Constant => self.into(),
                _ => builder.build_load(self.into()),
            },
            CodegenUnit::FunctionArgument(_, PrimitiveType::Slice { .. }) => {
                builder.build_load(self.into())
            }
            CodegenUnit::FunctionArgument(_, _) => self.into(),
            _ => panic!("Cannot load non-variable"),
        }
    }

    pub fn var_ty(&self) -> &PrimitiveType {
        match self {
            CodegenUnit::Variable { ty, .. } => match ty {
                PrimitiveType::NamedType {
                    name: _,
                    ty: Some(ty),
                } => ty,
                ty => ty,
            },
            CodegenUnit::FunctionArgument(_, ty) => ty,
            _ => panic!("ICE var_ty {:?}", self),
        }
    }

    pub fn get_mutability(&self) -> &Mutability {
        match self {
            CodegenUnit::Variable { mutable, .. } => mutable,
            CodegenUnit::FunctionArgument(_, PrimitiveType::Array { .. }) => &Mutability::Mutable,
            CodegenUnit::FunctionArgument(_, PrimitiveType::Slice { .. }) => &Mutability::Mutable,
            _ => panic!("ICE get_mutability: Not a variable {:?}", self),
        }
    }
}
