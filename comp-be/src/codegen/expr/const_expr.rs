use crate::{
    codegen::{
        unit::{CodegenUnit, Mutability},
        AatbeModule, CompileError, ValueTypePair,
    },
    fmt::AatbeFmt,
    ty::LLVMTyInCtx,
};
use parser::ast::{AtomKind, Expression, FloatSize, IntSize, PrimitiveType, AST};

pub fn const_atom(module: &AatbeModule, atom: &AtomKind) -> Option<ValueTypePair> {
    match atom {
        AtomKind::StringLiteral(string) => Some(
            (
                module
                    .llvm_builder_ref()
                    .build_global_string_ptr(string.as_str()),
                PrimitiveType::Str,
            )
                .into(),
        ),
        AtomKind::CharLiteral(ch) => Some(
            (
                module.llvm_context_ref().SInt8(*ch as u64),
                PrimitiveType::Char,
            )
                .into(),
        ),
        AtomKind::Integer(val, prim @ PrimitiveType::Int(IntSize::Bits8)) => {
            Some((module.llvm_context_ref().SInt8(*val), prim.clone()).into())
        }
        AtomKind::Integer(val, prim @ PrimitiveType::Int(IntSize::Bits16)) => {
            Some((module.llvm_context_ref().SInt16(*val), prim.clone()).into())
        }
        AtomKind::Integer(val, prim @ PrimitiveType::Int(IntSize::Bits32)) => {
            Some((module.llvm_context_ref().SInt32(*val), prim.clone()).into())
        }
        AtomKind::Integer(val, prim @ PrimitiveType::Int(IntSize::Bits64)) => {
            Some((module.llvm_context_ref().SInt64(*val), prim.clone()).into())
        }
        AtomKind::Integer(val, prim @ PrimitiveType::UInt(IntSize::Bits8)) => {
            Some((module.llvm_context_ref().UInt8(*val), prim.clone()).into())
        }
        AtomKind::Integer(val, prim @ PrimitiveType::UInt(IntSize::Bits16)) => {
            Some((module.llvm_context_ref().UInt8(*val), prim.clone()).into())
        }
        AtomKind::Integer(val, prim @ PrimitiveType::UInt(IntSize::Bits32)) => {
            Some((module.llvm_context_ref().UInt8(*val), prim.clone()).into())
        }
        AtomKind::Integer(val, prim @ PrimitiveType::UInt(IntSize::Bits64)) => {
            Some((module.llvm_context_ref().UInt8(*val), prim.clone()).into())
        }
        AtomKind::Floating(val, prim @ PrimitiveType::Float(FloatSize::Bits32)) => {
            Some((module.llvm_context_ref().Float(*val), prim.clone()).into())
        }
        AtomKind::Floating(val, prim @ PrimitiveType::Float(FloatSize::Bits64)) => {
            Some((module.llvm_context_ref().Double(*val), prim.clone()).into())
        }
        _ => panic!("ICE fold_atom {:?}", atom),
    }
}

fn fold_expression(module: &AatbeModule, expr: &Expression) -> Option<ValueTypePair> {
    match expr {
        Expression::Atom(atom) => const_atom(module, atom),
        _ => panic!("ICE fold_expression {:?}", expr),
    }
}

pub fn fold_constant(module: &mut AatbeModule, ast: &AST) -> Option<CodegenUnit> {
    match ast {
        AST::Global {
            ty:
                PrimitiveType::NamedType {
                    name,
                    ty: Some(box ty),
                },
            value,
        } => fold_expression(module, value)
            .and_then(|val| {
                if val.prim() != ty.inner() {
                    module.add_error(CompileError::AssignMismatch {
                        expected_ty: ty.fmt(),
                        found_ty: val.prim().fmt(),
                        value: value.fmt(),
                        var: name.clone(),
                    });

                    None
                } else {
                    Some(val)
                }
            })
            .map(|val| {
                let val_ref = module
                    .llvm_module_ref()
                    .add_global(ty.llvm_ty_in_ctx(module), name.as_ref());
                module.llvm_module_ref().set_initializer(val_ref, *val);
                CodegenUnit::Variable {
                    mutable: Mutability::Global,
                    name: name.clone(),
                    ty: val.prim().clone(),
                    value: val_ref,
                }
            }),
        AST::Constant {
            ty:
                PrimitiveType::NamedType {
                    name,
                    ty: Some(box ty),
                },
            value,
        } => fold_expression(module, value)
            .and_then(|val| {
                if val.prim() != ty.inner() {
                    module.add_error(CompileError::AssignMismatch {
                        expected_ty: ty.fmt(),
                        found_ty: val.prim().fmt(),
                        value: value.fmt(),
                        var: name.clone(),
                    });

                    None
                } else {
                    Some(val)
                }
            })
            .map(|val| CodegenUnit::Variable {
                mutable: Mutability::Constant,
                name: name.clone(),
                ty: val.prim().clone(),
                value: *val,
            }),
        _ => unreachable!(),
    }
}
