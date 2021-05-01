use crate::ty::infer::infer_type;
use crate::{
    codegen::{
        builder::{base, value},
        unit::{cg::expr, CompilerContext, Message, Mutability, Query, QueryResponse, Slot},
        AatbeModule, CompileError, ValueTypePair,
    },
    fmt::AatbeFmt,
    ty::{record::store_named_field, LLVMTyInCtx, TypeKind},
};

use guard::guard;
use parser::ast::{AtomKind, Expression, LValue, Type};

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

pub fn alloc_variable(variable: &Expression, ctx: &CompilerContext) -> Option<Type> {
    match variable {
        Expression::Decl {
            ty: Type::NamedType { name, ty },
            value,
            exterior_bind,
        } => {
            let mut vtp = None;
            let ty = match ty {
                Some(ty) => match ty {
                    box Type::GenericTypeRef(name, types) => {
                        /*let rec = rec_name!(name.clone(), types);
                        if !module.typectx_ref().get_record(&rec).is_ok() {
                            module.propagate_types_in_record(name, types.clone());
                        }
                        Type::TypeRef(rec.clone())*/
                        todo!()
                    }
                    box Type::VariantType(_) => {
                        /*if let Some(e) = value {
                            let pair = module.codegen_expr(e)?;

                            let ty = pair.prim().clone();
                            vtp = Some(pair);
                            ty
                        } else {
                            unreachable!();
                        }*/
                        todo!()
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
                            /*let rec = rec_name!(record.clone(), types);
                            if types.len() == 0 {
                                Type::TypeRef(rec.clone())
                            } else {
                                if !module.typectx_ref().get_record(&rec).is_ok() {
                                    module.propagate_types_in_record(record, types.clone());
                                }

                                Type::TypeRef(rec.clone())
                            }*/
                            todo!()
                        } else {
                            let pair = expr::cg(e, ctx)?;

                            let ty = pair.prim().clone();
                            vtp = Some(pair);
                            ty
                        }
                    } else {
                        unreachable!();
                    }
                }
            };

            if let Type::Newtype(..) | Type::VariantType(..) = ty {
                /*module.push_in_scope(
                    name,
                    Slot::Variable {
                        mutable: Mutability::from(exterior_bind),
                        name: name.clone(),
                        ty: ty.clone(),
                        value: *vtp.unwrap(),
                    },
                );
                return Some(ty.clone());*/
                todo!()
            }

            if value.is_none() {
                let val_ref = base::alloca_with_name(ctx, ty.llvm_ty_in_ctx(ctx), &name);

                ctx.dispatch(Message::PushInScope(
                    name.clone(),
                    Slot::Variable {
                        mutable: Mutability::from(exterior_bind),
                        name: name.clone(),
                        ty: ty.clone(),
                        value: val_ref,
                    },
                ));

                return Some(ty);
            }

            // TODO: Variants, generic records
            let ty = infer_type(ctx, &*(value.as_ref().expect("ICE: Value cannot be none")));

            if ty.is_none() {
                panic!("ICE: ty is none {:?} value", value);
            }
            let (ty, constant) = ty.unwrap();

            let var_ref = base::alloca_with_name(ctx, ty.llvm_ty_in_ctx(ctx), &name);

            ctx.dispatch(Message::PushInScope(
                name.clone(),
                Slot::Variable {
                    mutable: Mutability::from(exterior_bind),
                    name: name.clone(),
                    ty: ty.clone(),
                    value: var_ref,
                },
            ));

            if let Some(e) = value {
                if let box Expression::RecordInit {
                    record,
                    types,
                    values,
                } = e
                {
                    /*if ty.inner() != &Type::TypeRef(rec_name!(record.clone(), types)) {
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
                    );*/
                    todo!()
                } else {
                    match ty.clone() {
                        Type::Array { ref ty, len } if !constant => {
                            /*if let box Expression::Atom(AtomKind::Array(exprs)) = e {
                                let vals = exprs
                                    .iter()
                                    .filter_map(|e| module.codegen_expr(e))
                                    .collect::<Vec<_>>();

                                if vals.len() != len as usize {
                                    panic!("ICE: array vals len != array len");
                                }

                                vals.iter().for_each(|v| {
                                    if v.prim() != ty.as_ref() {
                                        module.add_error(CompileError::ExpectedType {
                                            expected_ty: ty.inner().fmt(),
                                            found_ty: v.prim().fmt(),
                                            value: value.as_ref().unwrap().fmt(),
                                        });
                                    }
                                });

                                vals.iter().enumerate().for_each(|(i, v)| {
                                    let ptr = module.llvm_builder_ref().build_inbounds_gep(
                                        var_ref,
                                        &mut [
                                            module.llvm_context_ref().SInt32(0),
                                            module.llvm_context_ref().SInt32(i as u64),
                                        ],
                                    );
                                    module.llvm_builder_ref().build_store(**v, ptr);
                                });
                            } else {
                                panic!("ICE: arr ty not arr val");
                            }*/
                            todo!()
                        }
                        _ => {
                            let val = expr::cg(e, ctx)?;

                            if val.prim() != ty.inner() {
                                /*module.add_error(CompileError::ExpectedType {
                                    expected_ty: ty.inner().fmt(),
                                    found_ty: val.prim().fmt(),
                                    value: value.as_ref().unwrap().fmt(),
                                });*/
                                todo!("Error")
                            };

                            base::store(ctx, *val, var_ref);
                        }
                    };
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
    todo!()
    /*fn get_lval(module: &mut AatbeModule, lvalue: &LValue) -> Option<ValueTypePair> {
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
                        TypeKind::Primitive(Type::Str) => {
                            let gep = module
                                .llvm_builder_ref()
                                .build_inbounds_gep(*val, &mut [*index]);

                            Some(
                                (
                                    module.llvm_builder_ref().build_load(gep),
                                    Type::Char,
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
    }*/
}

pub fn store_value(
    ctx: &CompilerContext,
    lval: &LValue,
    value: &Expression,
) -> Option<ValueTypePair> {
    fn get_lval(ctx: &CompilerContext, lvalue: &LValue) -> Option<ValueTypePair> {
        match lvalue {
            LValue::Ident(name) => {
                guard!(let QueryResponse::Slot(Some(slot)) = ctx.query(Query::Slot(name)) else { panic!("Cannot find variable {}", name) });
                match slot.get_mutability() {
                    Mutability::Mutable | Mutability::Global => {}
                    _ => panic!("Cannot reassign to immutable/constant {}", name),
                };
                Some(slot.into())
            }
            LValue::Accessor(parts) => todo!(), //Some(module.get_interior_pointer(parts.clone())?),
            LValue::Deref(_) => unimplemented!("{:?}", lvalue),
            LValue::Index(lval, index) => {
                let val = get_lval(ctx, lval);
                let index = expr::cg(index, ctx)?;

                if let Some(val) = val {
                    match val.ty() {
                        TypeKind::Primitive(Type::Str) => Some(
                            (
                                base::inbounds_gep(ctx, base::load(ctx, *val), &mut vec![*index]),
                                Type::Char,
                            )
                                .into(),
                        ),
                        TypeKind::Primitive(Type::Array { ty: box ty, .. }) => Some(
                            (
                                base::inbounds_gep(
                                    ctx,
                                    *val,
                                    &mut vec![*value::s32(ctx, 0), *index],
                                ),
                                ty,
                            )
                                .into(),
                        ),
                        TypeKind::Primitive(Type::Slice { ty: box ty }) => Some(
                            (
                                base::inbounds_gep(
                                    ctx,
                                    base::load(ctx, base::struct_gep(ctx, *val, 0)),
                                    &mut vec![*value::s32(ctx, 0), *index],
                                ),
                                ty,
                            )
                                .into(),
                        ),
                        _ => {
                            // TODO: Error
                            /*
                            module.add_error(CompileError::NotIndexable {
                                ty: val.prim().fmt(),
                                lval: lvalue.fmt(),
                            });*/
                            todo!();
                            None
                        }
                    }
                } else {
                    None
                }
            }
        }
    }

    get_lval(ctx, lval).and_then(|var| {
        let val = expr::cg(value, ctx)?;
        if var.prim() != val.prim().inner() {
            // TODO: Error
            todo!("ERROR");
            /*module.add_error(CompileError::AssignMismatch {
                expected_ty: var.prim().fmt(),
                found_ty: val.prim().fmt(),
                value: value.fmt(),
                var: lval.fmt(),
            });*/

            None
        } else {
            Some((base::store(ctx, *val, *var), var.ty()).into())
        }
    })
}
