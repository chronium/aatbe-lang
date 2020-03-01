use llvm_sys_wrapper::{Builder, Function, LLVMBasicBlockRef, LLVMValueRef};

use crate::codegen::{AatbeModule, ValueTypePair};
use parser::ast::{BindType, PrimitiveType};

pub mod function;
pub use function::{codegen_function, declare_function, inject_function_in_scope};

pub mod variable;
pub use variable::{alloc_variable, init_record, store_value};

#[derive(Debug)]
pub enum Mutability {
    Immutable,
    Mutable,
    Constant,
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

#[derive(Debug)]
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
            CodegenUnit::Variable {
                mutable: _,
                name: _,
                ty: _,
                value,
            } => *value,
        }
    }
}

impl CodegenUnit {
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
            CodegenUnit::Function(
                _,
                PrimitiveType::Function {
                    ext: _,
                    ret_ty: _,
                    params,
                },
            ) => params
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
                .collect::<Vec<PrimitiveType>>()
                .clone(),
            _ => panic!("ICE param_types {:?}", self),
        }
    }

    pub fn get_index(&self, module: &AatbeModule, name: &String) -> Option<(u32, PrimitiveType)> {
        match self {
            CodegenUnit::Variable {
                mutable: _,
                name: _,
                ty:
                    PrimitiveType::NamedType {
                        name: _,
                        ty: Some(box PrimitiveType::TypeRef(record)),
                    },
                value: _,
            } => match module.typectx_ref().get_record(record) {
                Ok(rec) => rec.get_field_index_ty(name),
                _ => panic!("Cannot get index from {:?}", self),
            },
            _ => panic!("Cannot get index from {:?}", self),
        }
    }

    pub fn append_basic_block(&self, name: String) -> LLVMBasicBlockRef {
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
                mutable: _,
                name: _,
                ty: PrimitiveType::Array { ty: _, len: _ },
                value: _,
            } => self.into(),
            CodegenUnit::Variable {
                mutable,
                name: _,
                ty: _,
                value: _,
            } => match mutable {
                Mutability::Mutable | Mutability::Immutable => builder.build_load(self.into()),
                Mutability::Constant => self.into(),
            },
            CodegenUnit::FunctionArgument(_, _) => self.into(),
            _ => panic!("Cannot load non-variable"),
        }
    }

    pub fn var_ty(&self) -> &PrimitiveType {
        match self {
            CodegenUnit::Variable {
                mutable: _,
                name: _,
                ty,
                value: _,
            } => match ty {
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
            CodegenUnit::Variable {
                mutable,
                name: _,
                ty: _,
                value: _,
            } => mutable,
            _ => panic!("ICE get_mutability: Not a variable {:?}", self),
        }
    }
}
