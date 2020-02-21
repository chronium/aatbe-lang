use crate::{
    codegen::{unit::Mutability, AatbeModule, CodegenUnit, CompileError, ValueTypePair},
    fmt::AatbeFmt,
    ty::{record::store_named_field, LLVMTyInCtx, TypeKind},
};

use parser::ast::{AtomKind, Expression, LValue, PrimitiveType};

pub fn alloc_variable(module: &mut AatbeModule, variable: &Expression) -> PrimitiveType {
    match variable {
        Expression::Decl {
            ty: PrimitiveType::NamedType { name, ty },
            value,
            exterior_bind,
        } => {
            let mut vtp = None;
            let ty = match ty {
                Some(ty) => ty.clone(),
                None => box PrimitiveType::NamedType {
                    name: name.clone(),
                    ty: Some(box if let Some(e) = value {
                        if let box Expression::RecordInit { record, values: _ } = e {
                            PrimitiveType::TypeRef(record.clone())
                        } else {
                            let pair = module
                                .codegen_expr(e)
                                .expect(format!("Cannot codegen variable {} value", name).as_ref());
                            let ty = pair.prim().clone();
                            vtp = Some(pair);
                            ty
                        }
                    } else {
                        unreachable!();
                    }),
                },
            };
            let var_ref = module
                .llvm_builder_ref()
                .build_alloca_with_name(ty.llvm_ty_in_ctx(module), name.as_ref());

            module.push_in_scope(
                name,
                CodegenUnit::Variable {
                    mutable: Mutability::from(exterior_bind),
                    name: name.clone(),
                    ty: PrimitiveType::NamedType {
                        name: name.clone(),
                        ty: Some(ty.clone()),
                    },
                    value: var_ref,
                },
            );

            if let Some(e) = value {
                if let box Expression::RecordInit { record, values } = e {
                    if ty.inner() != &PrimitiveType::TypeRef(record.clone()) {
                        module.add_error(CompileError::ExpectedType {
                            expected_ty: ty.inner().fmt(),
                            found_ty: record.clone(),
                            value: value.as_ref().unwrap().fmt(),
                        });
                    }
                    init_record(
                        module,
                        &LValue::Ident(name.clone()),
                        &Expression::RecordInit {
                            record: record.clone(),
                            values: values.to_vec(),
                        },
                    );
                } else {
                    let val = if vtp.is_none() {
                        let val = module
                            .codegen_expr(e)
                            .expect(format!("Cannot codegen variable {} value", name).as_ref());

                        if val.prim() != ty.inner() {
                            module.add_error(CompileError::ExpectedType {
                                expected_ty: ty.inner().fmt(),
                                found_ty: val.prim().fmt(),
                                value: value.as_ref().unwrap().fmt(),
                            });
                        };
                        val
                    } else {
                        vtp.unwrap()
                    };
                    module.llvm_builder_ref().build_store(val.val(), var_ref);
                }
            }

            PrimitiveType::NamedType {
                name: name.clone(),
                ty: Some(ty.clone()),
            }
        }
        _ => unreachable!(),
    }
}

pub fn init_record(
    module: &mut AatbeModule,
    lval: &LValue,
    rec: &Expression,
) -> Option<ValueTypePair> {
    fn get_lval(module: &mut AatbeModule, lvalue: &LValue) -> Option<ValueTypePair> {
        match lvalue {
            LValue::Ident(name) => match module.get_var(name) {
                None => panic!("Cannot find variable {}", name),
                Some(var) => Some(var.into()),
            },
            LValue::Accessor(parts) => Some(module.get_interior_pointer(parts.clone())),
            LValue::Deref(_) => unimplemented!(),
            LValue::Index(lval, index) => {
                let val = get_lval(module, lval);
                let index = module.codegen_expr(index).expect("ICE init_record index");

                if let Some(val) = val {
                    match val.ty() {
                        TypeKind::Primitive(PrimitiveType::Str) => {
                            let gep = module
                                .llvm_builder_ref()
                                .build_inbounds_gep(val.val(), &mut [index.val()]);

                            Some(
                                (
                                    module.llvm_builder_ref().build_load(gep),
                                    TypeKind::Primitive(PrimitiveType::Char),
                                )
                                    .into(),
                            )
                        }
                        _ => {
                            module.add_error(CompileError::NotIndexable {
                                ty: val.prim().fmt(),
                                lval: lvalue.fmt(),
                            });
                            None
                        }
                    }
                } else {
                    None
                }
            }
        }
    }

    match rec {
        Expression::RecordInit { record, values } => get_lval(module, lval)
            .map(|var| {
                let mut err = false;

                values.iter().for_each(|val| match val {
                    AtomKind::NamedValue { name, val } => {
                        let val_ref = module
                            .codegen_expr(val)
                            .expect(format!("ICE could not codegen {:?}", val).as_str());
                        let val_ty = val_ref.prim().fmt();
                        match store_named_field(
                            module,
                            var.val(),
                            &lval.into(),
                            module
                                .typectx_ref()
                                .get_record(record)
                                .expect(format!("ICE could not find record {}", record).as_str()),
                            name,
                            val_ref,
                        ) {
                            Ok(_) => {}
                            Err(expected) => {
                                err = true;
                                module.add_error(CompileError::StoreMismatch {
                                    expected_ty: expected.fmt(),
                                    found_ty: val_ty,
                                    value: val.fmt(),
                                    lval: lval.fmt(),
                                })
                            }
                        };
                    }
                    _ => panic!("ICE init_record unexpected {:?}", val),
                });

                match err {
                    false => Some(var),
                    true => None,
                }
            })
            .flatten(),
        _ => unreachable!(),
    }
}

pub fn store_value(
    module: &mut AatbeModule,
    lval: &LValue,
    value: &Expression,
) -> Option<ValueTypePair> {
    fn get_lval(module: &mut AatbeModule, lvalue: &LValue) -> Option<ValueTypePair> {
        match lvalue {
            LValue::Ident(name) => match module.get_var(name) {
                None => panic!("Cannot find variable {}", name),
                Some(var) => {
                    match var.get_mutability() {
                        Mutability::Mutable => {}
                        _ => panic!("Cannot reassign to immutable/constant {}", name),
                    };
                    Some(var.into())
                }
            },
            LValue::Accessor(parts) => Some(module.get_interior_pointer(parts.clone())),
            LValue::Deref(_) => unimplemented!(),
            LValue::Index(lval, index) => {
                let val = get_lval(module, lval);
                let index = module.codegen_expr(index).expect("ICE store_value index");

                if let Some(val) = val {
                    match val.ty() {
                        TypeKind::Primitive(PrimitiveType::Str) => {
                            let load = module.llvm_builder_ref().build_load(val.val());
                            let gep = module
                                .llvm_builder_ref()
                                .build_inbounds_gep(load, &mut [index.val()]);

                            Some((gep, TypeKind::Primitive(PrimitiveType::Char)).into())
                        }
                        _ => {
                            module.add_error(CompileError::NotIndexable {
                                ty: val.prim().fmt(),
                                lval: lvalue.fmt(),
                            });
                            None
                        }
                    }
                } else {
                    None
                }
            }
        }
    }

    get_lval(module, lval)
        .map(|var| {
            let val = module
                .codegen_expr(value)
                .expect(format!("Cannot codegen assignment for {:?} value", lval).as_ref());
            if var.prim() != val.prim().inner() {
                module.add_error(CompileError::AssignMismatch {
                    expected_ty: var.prim().fmt(),
                    found_ty: val.prim().fmt(),
                    value: value.fmt(),
                    var: lval.fmt(),
                });

                None
            } else {
                Some(
                    (
                        module.llvm_builder_ref().build_store(val.val(), var.val()),
                        var.ty(),
                    )
                        .into(),
                )
            }
        })
        .flatten()
}
