use crate::{codegen::AatbeModule, ty::record::Record};
use parser::ast::{IntSize, PrimitiveType};

use llvm_sys_wrapper::{LLVMFunctionType, LLVMTypeRef};

use std::collections::HashMap;

pub mod record;

#[derive(Debug)]
pub enum TypeError {
    NotRecord(String),
}

pub type TypeResult<T> = Result<T, TypeError>;

pub enum TypeKind {
    RecordType(Record),
    Primitive(PrimitiveType),
}

impl LLVMTyInCtx for TypeKind {
    fn llvm_ty_in_ctx(&self, module: &AatbeModule) -> LLVMTypeRef {
        match self {
            TypeKind::RecordType(record) => record.llvm_ty_in_ctx(module),
            TypeKind::Primitive(primitive) => primitive.llvm_ty_in_ctx(module),
        }
    }
}

impl TypeKind {
    fn record(&self) -> Option<&Record> {
        match self {
            TypeKind::RecordType(record) => Some(record),
            TypeKind::Primitive(_) => None,
        }
    }

    pub fn prim(primitive: PrimitiveType) -> TypeKind {
        TypeKind::Primitive(primitive)
    }
}

pub struct TypeContext {
    types: HashMap<String, TypeKind>,
}

impl TypeContext {
    pub fn new() -> Self {
        Self {
            types: HashMap::new(),
        }
    }

    pub fn push_type(&mut self, name: &String, ty: TypeKind) {
        if self.types.contains_key(name) {
            panic!("Type {} already in context", name);
        } else {
            self.types.insert(name.clone(), ty);
        }
    }

    pub fn get_type(&self, name: &String) -> Option<&TypeKind> {
        self.types.get(name)
    }

    pub fn get_record(&self, name: &String) -> TypeResult<&Record> {
        self.types
            .get(name)
            .map(|ty| ty.record())
            .flatten()
            .ok_or(TypeError::NotRecord(name.clone()))
    }
}

pub trait LLVMTyInCtx {
    fn llvm_ty_in_ctx(&self, module: &AatbeModule) -> LLVMTypeRef;
}

impl LLVMTyInCtx for PrimitiveType {
    fn llvm_ty_in_ctx(&self, module: &AatbeModule) -> LLVMTypeRef {
        let ctx = module.llvm_context_ref();

        match self {
            PrimitiveType::Unit => ctx.VoidType(),
            PrimitiveType::Bool => ctx.Int1Type(),
            PrimitiveType::Int(IntSize::Bits8) | PrimitiveType::UInt(IntSize::Bits8) => {
                ctx.Int8Type()
            }
            PrimitiveType::Int(IntSize::Bits16) | PrimitiveType::UInt(IntSize::Bits16) => {
                ctx.Int16Type()
            }
            PrimitiveType::Int(IntSize::Bits32) | PrimitiveType::UInt(IntSize::Bits32) => {
                ctx.Int32Type()
            }
            PrimitiveType::Int(IntSize::Bits64) | PrimitiveType::UInt(IntSize::Bits64) => {
                ctx.Int64Type()
            }
            PrimitiveType::Str => ctx.CharPointerType(),
            PrimitiveType::Char => ctx.Int8Type(),
            PrimitiveType::NamedType { name: _, ty } => ty.llvm_ty_in_ctx(module),
            PrimitiveType::TypeRef(name) => module
                .typectx_ref()
                .get_type(name)
                .expect(format!("Type {} is not declared", name).as_str())
                .llvm_ty_in_ctx(module),
            PrimitiveType::Pointer(ty) => match ty {
                box PrimitiveType::Char => ctx.CharPointerType(),
                tr @ box PrimitiveType::TypeRef(_) => ctx.PointerType(tr.llvm_ty_in_ctx(module)),
                _ => panic!("llvm_ty_in_ctx {:?}", ty),
            },
            PrimitiveType::Function {
                ext: _,
                ret_ty,
                params,
            } => {
                let ret = ret_ty.llvm_ty_in_ctx(module);
                let mut varargs = false;
                let mut param_types = params
                    .iter()
                    .filter_map(|t| match t {
                        PrimitiveType::TypeRef(_name) => None,
                        PrimitiveType::Unit => None,
                        PrimitiveType::Varargs => {
                            varargs = true;
                            None
                        }
                        _ => Some(t.llvm_ty_in_ctx(module)),
                    })
                    .collect::<Vec<LLVMTypeRef>>();

                unsafe {
                    LLVMFunctionType(
                        ret,
                        param_types.as_mut_ptr(),
                        param_types.len() as u32,
                        varargs as i32,
                    )
                }
            }
            _ => panic!("ICE: llvm_ty_in_ctx {:?}", self),
        }
    }
}
