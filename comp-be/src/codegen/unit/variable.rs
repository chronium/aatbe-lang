use crate::ty::infer::infer_type;
use crate::{
    codegen::{
        unit::{Mutability, Slot},
        AatbeModule, CompileError, ValueTypePair,
    },
    fmt::AatbeFmt,
    ty::{record::store_named_field, LLVMTyInCtx, TypeKind},
};

use parser::ast::{AtomKind, Expression, LValue, PrimitiveType};

macro_rules! rec_name {
    ($name:expr, $types:expr) => {{
        format!(
            "{}{}",
            $name,
            if $types.len() > 0 {
                format!(
                    "[{}]",
                    $types
                        .iter()
                        .map(|ty| ty.fmt())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            } else {
                String::default()
            }
        )
    }};
}

pub fn alloc_variable(module: &mut AatbeModule, variable: &Expression) -> Option<PrimitiveType> {
    match variable {
        Expression::Decl {
            ty: PrimitiveType::NamedType { name, ty },
            value,
            exterior_bind,
        } => {
            /*let mut vtp = None;
            let ty = match ty {
                Some(ty) => match ty {
                    box PrimitiveType::GenericTypeRef(name, types) => {
                        let rec = rec_name!(name.clone(), types);
                        if !module.typectx_ref().get_record(&rec).is_ok() {
                            module.propagate_types_in_record(name, types.clone());
                        }
                        PrimitiveType::TypeRef(rec.clone())
                    }
                    box PrimitiveType::VariantType(_) => {
                        if let Some(e) = value {
                            let pair = module.codegen_expr(e)?;

                            let ty = pair.prim().clone();
                            vtp = Some(pair);
                            ty
                        } else {
                            unreachable!();
                        }
                    }
                    _ => *ty.clone(),
                },
                None => {
                    if let Some(e) = value {
                        if let box Expression::RecordInit {
                            record,
                            types,
                            values: _,
                        } = e
                        {
                            let rec = rec_name!(record.clone(), types);
                            if types.len() == 0 {
                                PrimitiveType::TypeRef(rec.clone())
                            } else {
                                if !module.typectx_ref().get_record(&rec).is_ok() {
                                    module.propagate_types_in_record(record, types.clone());
                                }

                                PrimitiveType::TypeRef(rec.clone())
                            }
                        } else {
                            let pair = module.codegen_expr(e)?;

                            let ty = pair.prim().clone();
                            vtp = Some(pair);
                            ty
                        }
                    } else {
                        unreachable!();
                    }
                }
            };*/

            /*if let PrimitiveType::Newtype(..) | PrimitiveType::VariantType(..) = ty {
                module.push_in_scope(
                    name,
                    Slot::Variable {
                        mutable: Mutability::from(exterior_bind),
                        name: name.clone(),
                        ty: ty.clone(),
                        value: *vtp.unwrap(),
                    },
                );
                return Some(ty.clone());
            }*/

            if value.is_none() {
                let ty = ty.as_ref().expect("ICE: Ty none for none value");
                let val_ref = module
                    .llvm_builder_ref()
                    .build_alloca_with_name(ty.llvm_ty_in_ctx(module), name.as_ref());

                module.push_in_scope(
                    name,
                    Slot::Variable {
                        mutable: Mutability::from(exterior_bind),
                        name: name.clone(),
                        ty: *ty.clone(),
                        value: val_ref,
                    },
                );

                return Some(*ty.clone());
            }

            // TODO: Variants, generic records
            let ty = infer_type(
                module,
                &*(value.as_ref().expect("ICE: Value cannot be none")),
            );

            if ty.is_none() {
                panic!("ICE: ty is none {:?} value", value);
            }
            let ty = ty.unwrap();

            let var_ref = module
                .llvm_builder_ref()
                .build_alloca_with_name(ty.llvm_ty_in_ctx(module), name.as_ref());

            module.push_in_scope(
                name,
                Slot::Variable {
                    mutable: Mutability::from(exterior_bind),
                    name: name.clone(),
                    ty: ty.clone(),
                    value: var_ref,
                },
            );

            if let Some(e) = value {
                if let box Expression::RecordInit {
                    record,
                    types,
                    values,
                } = e
                {
                    if ty.inner() != &PrimitiveType::TypeRef(rec_name!(record.clone(), types)) {
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
                            types: types.clone(),
                            values: values.to_vec(),
                        },
                    );
                } else {
                    /*let val = if vtp.is_none() {
                        let val = module.codegen_expr(e)?;

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
                    };*/
                    let val = module.codegen_expr(e)?;

                    if val.prim() != ty.inner() {
                        module.add_error(CompileError::ExpectedType {
                            expected_ty: ty.inner().fmt(),
                            found_ty: val.prim().fmt(),
                            value: value.as_ref().unwrap().fmt(),
                        });
                    };
                    module.llvm_builder_ref().build_store(*val, var_ref);
                }
            }
            Some(ty.clone())
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
            LValue::Accessor(parts) => Some(module.get_interior_pointer(parts.clone())?),
            LValue::Deref(_) => unimplemented!("{:?}", lvalue),
            LValue::Index(lval, index) => {
                let val = get_lval(module, lval);
                let index = module.codegen_expr(index).expect("ICE init_record index");

                if let Some(val) = val {
                    match val.ty() {
                        TypeKind::Primitive(PrimitiveType::Str) => {
                            let gep = module
                                .llvm_builder_ref()
                                .build_inbounds_gep(*val, &mut [*index]);

                            Some(
                                (
                                    module.llvm_builder_ref().build_load(gep),
                                    PrimitiveType::Char,
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
        Expression::RecordInit {
            record,
            types,
            values,
        } => get_lval(module, lval).and_then(|var| {
            let mut err = false;
            let rec = rec_name!(record, types);

            values.iter().for_each(|val| match val {
                AtomKind::NamedValue { name, val } => {
                    let val_ref = module
                        .codegen_expr(val)
                        .expect(format!("ICE could not codegen {:?}", val).as_str());
                    let val_ty = val_ref.prim().fmt();
                    match store_named_field(
                        module,
                        *var,
                        &lval.into(),
                        module
                            .typectx_ref()
                            .get_record(&rec)
                            .expect(format!("ICE could not find record {}", rec).as_str()),
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
        }),
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
                        Mutability::Mutable | Mutability::Global => {}
                        _ => panic!("Cannot reassign to immutable/constant {}", name),
                    };
                    Some(var.into())
                }
            },
            LValue::Accessor(parts) => Some(module.get_interior_pointer(parts.clone())?),
            LValue::Deref(_) => unimplemented!("{:?}", lvalue),
            LValue::Index(lval, index) => {
                let val = get_lval(module, lval);
                let index = module.codegen_expr(index).expect("ICE store_value index");

                if let Some(val) = val {
                    match val.ty() {
                        TypeKind::Primitive(PrimitiveType::Str) => {
                            let load = module.llvm_builder_ref().build_load(*val);
                            let gep = module
                                .llvm_builder_ref()
                                .build_inbounds_gep(load, &mut [*index]);

                            Some((gep, PrimitiveType::Char).into())
                        }
                        TypeKind::Primitive(PrimitiveType::Array { ty: box ty, .. }) => {
                            let gep = module.llvm_builder_ref().build_inbounds_gep(
                                *val,
                                &mut [module.llvm_context_ref().SInt32(0), *index],
                            );

                            Some((gep, ty).into())
                        }
                        TypeKind::Primitive(PrimitiveType::Slice { ty: box ty }) => {
                            let arr = module.llvm_builder_ref().build_struct_gep(*val, 0);
                            let gep = module.llvm_builder_ref().build_inbounds_gep(
                                module.llvm_builder_ref().build_load(arr),
                                &mut [module.llvm_context_ref().SInt32(0), *index],
                            );

                            Some((gep, ty).into())
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

    get_lval(module, lval).and_then(|var| {
        let val = module.codegen_expr(value)?;
        if var.prim() != val.prim().inner() {
            module.add_error(CompileError::AssignMismatch {
                expected_ty: var.prim().fmt(),
                found_ty: val.prim().fmt(),
                value: value.fmt(),
                var: lval.fmt(),
            });

            None
        } else {
            Some((module.llvm_builder_ref().build_store(*val, *var), var.ty()).into())
        }
    })
}
