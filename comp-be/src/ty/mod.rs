use crate::{
    codegen::{AatbeModule, ValueTypePair},
    fmt::AatbeFmt,
    ty::{record::Record, variant::VariantType},
};
use parser::ast::{FloatSize, IntSize, PrimitiveType};

use llvm_sys_wrapper::{LLVMFunctionType, LLVMTypeRef, LLVMValueRef};

use std::{collections::HashMap, fmt};

pub mod aggregate;
pub mod record;
pub mod variant;

pub use aggregate::Aggregate;

#[derive(Debug)]
pub enum TypeError {
    NotFound(String),
    NotFoundPrim(PrimitiveType),
    VariantOOB(String, u32),
    RecordIndexOOB(String, u32),
    NamedOnVariant(String, String),
    RecordNameNotFound(String, String),
}

pub type TypeResult<T> = Result<T, TypeError>;

pub enum TypeKind {
    RecordType(Record),
    Primitive(PrimitiveType),
    Typedef(TypedefKind),
}

#[derive(Debug)]
pub enum TypedefKind {
    Opaque(LLVMTypeRef),
    Newtype(LLVMTypeRef, PrimitiveType),
    Variant(VariantType),
}

impl fmt::Debug for TypeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeKind::Primitive(prim) => write!(f, "Primitive({})", AatbeFmt::fmt(prim)),
            TypeKind::RecordType(rec) => write!(f, "RecordType({})", rec.name()),
            TypeKind::Typedef(tk) => write!(f, "Typedef({:?})", tk),
        }
    }
}

impl LLVMTyInCtx for TypeKind {
    fn llvm_ty_in_ctx(&self, module: &AatbeModule) -> LLVMTypeRef {
        match self {
            TypeKind::RecordType(record) => record.llvm_ty_in_ctx(module),
            TypeKind::Primitive(primitive) => primitive.llvm_ty_in_ctx(module),
            TypeKind::Typedef(TypedefKind::Opaque(ty)) => *ty,
            TypeKind::Typedef(TypedefKind::Newtype(ty, _)) => *ty,
            TypeKind::Typedef(TypedefKind::Variant { .. }) => unimplemented!(),
        }
    }
}

impl TypeKind {
    fn record(&self) -> Option<&Record> {
        match self {
            TypeKind::RecordType(record) => Some(record),
            _ => None,
        }
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

    pub fn get_aggregate(&self, name: &String) -> Option<&dyn Aggregate> {
        match self.types.get(name)? {
            TypeKind::RecordType(record) => Some(record),
            TypeKind::Typedef(TypedefKind::Variant(variant)) => Some(variant),
            _ => None,
        }
    }

    pub fn get_aggregate_from_prim(&self, prim: &PrimitiveType) -> Option<&dyn Aggregate> {
        match prim {
            PrimitiveType::TypeRef(name) => match self.types.get(name)? {
                TypeKind::RecordType(record) => Some(record),
                TypeKind::Typedef(TypedefKind::Variant(variant)) => Some(variant),
                _ => None,
            },
            _ => unreachable!(),
        }
    }

    pub fn get_type_for_variant(&self, name: &String) -> Option<&TypeKind> {
        self.types
            .values()
            .filter(|ty| match ty {
                TypeKind::Typedef(TypedefKind::Variant(VariantType { variant_name, .. }))
                    if variant_name == name =>
                {
                    true
                }
                _ => false,
            })
            .nth(0)
    }

    pub fn get_record(&self, name: &String) -> TypeResult<&Record> {
        self.types
            .get(name)
            .and_then(|ty| ty.record())
            .ok_or(TypeError::NotFound(name.clone()))
    }

    pub fn get_field_named(
        &self,
        module: &AatbeModule,
        reference: LLVMValueRef,
        ty: &dyn Aggregate,
        fields: Vec<String>,
    ) -> TypeResult<ValueTypePair> {
        match fields.as_slice() {
            [field] => ty.gep_named_field(module, field, reference),
            [field, subfields @ ..] => {
                let field = ty.gep_named_field(module, field, reference)?;
                let prim = field.prim();
                let ty = self
                    .get_aggregate_from_prim(&prim.clone())
                    .ok_or(TypeError::NotFoundPrim(prim.clone()))?;

                self.get_field_named(module, *field, ty, subfields.to_vec())
            }
            [] => unreachable!(),
        }
    }
}

pub trait LLVMTyInCtx {
    fn llvm_ty_in_ctx(&self, module: &AatbeModule) -> LLVMTypeRef;
}

impl LLVMTyInCtx for PrimitiveType {
    fn llvm_ty_in_ctx(&self, module: &AatbeModule) -> LLVMTypeRef {
        let ctx = module.llvm_context_ref();

        match self {
            PrimitiveType::Slice { ty } => ctx
                .StructType(
                    &mut vec![
                        ctx.PointerType(ctx.ArrayType(ty.llvm_ty_in_ctx(module), 0)),
                        ctx.Int32Type(),
                    ],
                    false,
                )
                .as_ref(),
            PrimitiveType::Array { ty, len } => ctx.ArrayType(ty.llvm_ty_in_ctx(module), *len),
            PrimitiveType::Ref(r) => ctx.PointerType(r.llvm_ty_in_ctx(module)),
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
            PrimitiveType::Float(FloatSize::Bits32) => ctx.FloatType(),
            PrimitiveType::Float(FloatSize::Bits64) => ctx.DoubleType(),
            PrimitiveType::Str => ctx.CharPointerType(),
            PrimitiveType::Char => ctx.Int8Type(),
            PrimitiveType::NamedType {
                name: _,
                ty: Some(ty),
            } => ty.llvm_ty_in_ctx(module),
            PrimitiveType::TypeRef(name) => module
                .typectx_ref()
                .get_type(name)
                .expect(format!("Type {} is not declared", name).as_str())
                .llvm_ty_in_ctx(module),
            PrimitiveType::GenericTypeRef(name, types) => {
                let rec = format!(
                    "{}[{}]",
                    name,
                    types
                        .iter()
                        .map(|ty| ty.fmt())
                        .collect::<Vec<_>>()
                        .join(", ")
                );
                module
                    .typectx_ref()
                    .get_type(&rec)
                    .expect(format!("Type {} is not declared", rec).as_str())
                    .llvm_ty_in_ctx(module)
            }
            PrimitiveType::Pointer(ty) => match ty {
                box PrimitiveType::Char => ctx.CharPointerType(),
                tr @ box PrimitiveType::TypeRef(_) => ctx.PointerType(tr.llvm_ty_in_ctx(module)),
                tr @ box PrimitiveType::UInt(_) => ctx.PointerType(tr.llvm_ty_in_ctx(module)),
                tr @ box PrimitiveType::Int(_) => ctx.PointerType(tr.llvm_ty_in_ctx(module)),
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
                        PrimitiveType::TypeRef(name) => {
                            Some(module.typectx_ref().get_type(name)?.llvm_ty_in_ctx(module))
                        }
                        PrimitiveType::Symbol(_) => None,
                        PrimitiveType::Unit => None,
                        PrimitiveType::Varargs => {
                            varargs = true;
                            None
                        }
                        _ => Some(t.llvm_ty_in_ctx(module)),
                    })
                    .collect::<Vec<_>>();

                unsafe {
                    LLVMFunctionType(
                        ret,
                        param_types.as_mut_ptr(),
                        param_types.len() as u32,
                        varargs as i32,
                    )
                }
            }
            PrimitiveType::Newtype(name) => module
                .typectx_ref()
                .get_type(&name)
                .map(|ty| match ty {
                    TypeKind::Typedef(TypedefKind::Newtype(_, ty)) => ty.llvm_ty_in_ctx(module),
                    _ => unreachable!(),
                })
                .expect(format!("Cannot find type for {:?}", self).as_ref()),
            _ => panic!("ICE: llvm_ty_in_ctx {:?}", self),
        }
    }
}
