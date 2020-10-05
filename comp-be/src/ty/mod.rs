use crate::{
    codegen::{AatbeModule, ValueTypePair},
    fmt::AatbeFmt,
    ty::variant::{Variant, VariantType},
};
use parser::ast::{FloatSize, FunctionType, IntSize, PrimitiveType};

use llvm_sys_wrapper::{LLVMFunctionType, LLVMTypeRef, LLVMValueRef};

use std::{collections::HashMap, fmt};

pub mod aggregate;
pub mod infer;
pub mod record;
pub mod size;
pub mod variant;

pub use aggregate::Aggregate;
pub use record::Record;

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

pub enum TypedefKind {
    Opaque(LLVMTypeRef),
    Newtype(LLVMTypeRef, PrimitiveType),
    VariantType(VariantType),
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

impl fmt::Debug for TypedefKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypedefKind::Opaque(ty) => write!(f, "Opaque({:?})", ty),
            TypedefKind::Newtype(ty, prim) => {
                write!(f, "RecordType({:?}, {})", ty, AatbeFmt::fmt(prim))
            }
            TypedefKind::VariantType(ty) => write!(f, "VariantType({:?})", ty),
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
            TypeKind::Typedef(TypedefKind::VariantType(VariantType { ty, .. })) => *ty,
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

    fn typedef(&self) -> Option<&TypedefKind> {
        match self {
            TypeKind::Typedef(ty) => Some(ty),
            _ => None,
        }
    }

    fn variant_mut(&mut self) -> Option<&mut VariantType> {
        match self {
            TypeKind::Typedef(TypedefKind::VariantType(ty)) => Some(ty),
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

    pub fn llvm_ty_in_ctx(&self, name: &String) -> Option<&dyn LLVMTyInCtx> {
        self.get_type(name)
            .map(|t| t as &dyn LLVMTyInCtx)
            .or_else(|| self.get_variant(name).map(|t| t as &dyn LLVMTyInCtx))
    }

    pub fn get_aggregate(&self, name: &String) -> Option<&dyn Aggregate> {
        match self.types.get(name)? {
            TypeKind::RecordType(record) => Some(record),
            //TypeKind::Typedef(TypedefKind::Variant(variant)) => Some(variant),
            _ => None,
        }
    }

    pub fn get_aggregate_from_prim(&self, prim: &PrimitiveType) -> Option<&dyn Aggregate> {
        match prim {
            PrimitiveType::TypeRef(name) => match self.types.get(name) {
                Some(TypeKind::RecordType(record)) => Some(record as &dyn Aggregate),
                _ => None,
            },
            PrimitiveType::VariantType(name) => match self.get_variant(name) {
                Some(variant) => Some(variant as &dyn Aggregate),
                _ => None,
            },
            PrimitiveType::Variant { variant, .. } => match self.get_variant(variant) {
                Some(variant) => Some(variant as &dyn Aggregate),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn get_variant(&self, name: &String) -> Option<&Variant> {
        self.types
            .values()
            .filter_map(|ty| match ty {
                TypeKind::Typedef(TypedefKind::VariantType(variant)) => variant.get_variant(name),
                _ => None,
            })
            .nth(0)
    }

    pub fn push_variant(
        &mut self,
        parent_name: &String,
        variant_name: &String,
        variant: Variant,
    ) -> Result<(), TypeError> {
        self.types
            .get_mut(parent_name)
            .and_then(|ty| ty.variant_mut())
            .ok_or(TypeError::NotFound(parent_name.clone()))
            .map(|var| var.push_variant(variant_name, variant))
    }

    pub fn get_parent_for_variant(&self, name: &String) -> Option<&VariantType> {
        self.types
            .values()
            .filter_map(|ty| match ty {
                TypeKind::Typedef(TypedefKind::VariantType(variant))
                    if variant.has_variant(name) =>
                {
                    Some(variant)
                }
                _ => None,
            })
            .nth(0)
    }

    pub fn get_record(&self, name: &String) -> TypeResult<&Record> {
        self.types
            .get(name)
            .and_then(|ty| ty.record())
            .ok_or(TypeError::NotFound(name.clone()))
    }

    pub fn get_typedef(&self, name: &String) -> TypeResult<&TypedefKind> {
        self.types
            .get(name)
            .and_then(|ty| ty.typedef())
            .ok_or(TypeError::NotFound(name.clone()))
    }

    pub fn gep_fields(
        &self,
        module: &AatbeModule,
        ty: &dyn Aggregate,
        fields: Vec<String>,
        reference: LLVMValueRef,
    ) -> TypeResult<ValueTypePair> {
        match fields.as_slice() {
            [field] => ty.gep_field(module, field, reference),
            [field, subfields @ ..] => {
                let field = ty.gep_field(module, field, reference)?;
                let prim = field.prim();
                let ty = self
                    .get_aggregate_from_prim(&prim.clone())
                    .ok_or(TypeError::NotFoundPrim(prim.clone()))?;

                self.gep_fields(module, ty, subfields.to_vec(), *field)
            }
            [] => unreachable!(),
        }
    }
}

pub trait LLVMTyInCtx {
    fn llvm_ty_in_ctx(&self, module: &AatbeModule) -> LLVMTypeRef;
}

impl LLVMTyInCtx for FunctionType {
    fn llvm_ty_in_ctx(&self, module: &AatbeModule) -> LLVMTypeRef {
        let ret = self.ret_ty.llvm_ty_in_ctx(module);
        let mut varargs = false;
        let ext = self.ext;
        let mut param_types = self
            .params
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
                PrimitiveType::NamedType {
                    ty: Some(box PrimitiveType::Slice { ty: box ty }),
                    ..
                }
                | PrimitiveType::Slice { ty: box ty }
                    if ext =>
                {
                    Some(
                        module
                            .llvm_context_ref()
                            .PointerType(ty.llvm_ty_in_ctx(module)),
                    )
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
                .llvm_ty_in_ctx(name)
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
                box PrimitiveType::Str => ctx.PointerType(ctx.Int8PointerType()),
                box PrimitiveType::Pointer(box ty) => ctx.PointerType(ty.llvm_ty_in_ctx(module)),
                _ => panic!("llvm_ty_in_ctx {:?}", ty),
            },
            PrimitiveType::Function(ty) => ty.llvm_ty_in_ctx(module),
            PrimitiveType::Newtype(name) => module
                .typectx_ref()
                .get_type(&name)
                .map(|ty| match ty {
                    TypeKind::Typedef(TypedefKind::Newtype(_, ty)) => ty.llvm_ty_in_ctx(module),
                    _ => unreachable!(),
                })
                .expect(format!("Cannot find type for {:?}", self).as_ref()),
            PrimitiveType::Variant { variant, .. } => {
                module.typectx_ref().get_variant(variant).expect("ICE").ty
            }
            PrimitiveType::VariantType(variant) => {
                module
                    .typectx_ref()
                    .get_parent_for_variant(variant)
                    .expect("ICE")
                    .ty
            }
            PrimitiveType::Box(box val) => module
                .llvm_context_ref()
                .PointerType(val.llvm_ty_in_ctx(module)),
            _ => panic!("ICE: llvm_ty_in_ctx {:?}", self),
        }
    }
}
