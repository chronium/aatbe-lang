use crate::{
    codegen::{
        builder::value,
        unit::{CodegenUnit, Mutability},
        AatbeModule, CompileError, ValueTypePair,
    },
    fmt::AatbeFmt,
    ty::LLVMTyInCtx,
};
use parser::ast::{AtomKind, Expression, PrimitiveType, AST};

pub fn const_atom(module: &AatbeModule, atom: &AtomKind) -> Option<ValueTypePair> {
    match atom {
        AtomKind::StringLiteral(string) => Some(value::str(module, string.as_ref())),
        AtomKind::CharLiteral(ch) => Some(value::char(module, *ch)),
        AtomKind::Integer(val, PrimitiveType::Int(size)) => {
            Some(value::sint(module, size.clone(), *val))
        }
        AtomKind::Integer(val, PrimitiveType::UInt(size)) => {
            Some(value::uint(module, size.clone(), *val))
        }
        AtomKind::Floating(val, PrimitiveType::Float(size)) => {
            Some(value::floating(module, size.clone(), *val))
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
            export: _,
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
            export: _,
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
