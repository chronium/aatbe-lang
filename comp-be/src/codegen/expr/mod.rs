use crate::{
    codegen::{
        builder::value,
        unit::{CompilerContext, Mutability, Slot},
        AatbeModule, CompileError, ValueTypePair,
    },
    fmt::AatbeFmt,
    ty::LLVMTyInCtx,
};
use parser::ast::{AtomKind, Expression, Type, AST};

pub fn const_atom(ctx: &CompilerContext, atom: &AtomKind) -> Option<ValueTypePair> {
    match atom {
        AtomKind::StringLiteral(string) => Some(value::str(ctx, &string)),
        AtomKind::CharLiteral(ch) => Some(value::char(ctx, *ch)),
        AtomKind::Cast(box AtomKind::Integer(val, _), Type::Char) => {
            Some(value::char(ctx, *val as u8 as char))
        }
        AtomKind::Parenthesized(box atom) => fold_expression(ctx, atom),
        _ => panic!("ICE fold_atom {:?}", atom),
    }
}

fn fold_expression(ctx: &CompilerContext, expr: &Expression) -> Option<ValueTypePair> {
    match expr {
        Expression::Atom(atom) => const_atom(ctx, atom),
        _ => panic!("ICE fold_expression {:?}", expr),
    }
}

pub fn fold_constant(module: &mut AatbeModule, ast: &AST) -> Option<Slot> {
    /*match ast {
        AST::Global {
            ty:
                Type::NamedType {
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
                Slot::Variable {
                    mutable: Mutability::Global,
                    name: name.clone(),
                    ty: val.prim().clone(),
                    value: val_ref,
                }
            }),
        AST::Constant {
            ty:
                Type::NamedType {
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
            .map(|val| Slot::Variable {
                mutable: Mutability::Constant,
                name: name.clone(),
                ty: val.prim().clone(),
                value: *val,
            }),
        _ => unreachable!(),
    }*/
    todo!("{:?}", ast)
}
