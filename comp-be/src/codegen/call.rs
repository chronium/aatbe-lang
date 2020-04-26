use crate::{
    codegen::{
        builder::{cast, core, ty, value},
        mangle_v1::NameMangler,
        AatbeModule, CompileError, ValueTypePair,
    },
    fmt::AatbeFmt,
};

use parser::ast::{AtomKind, Expression, PrimitiveType};

impl AatbeModule {
    pub fn codegen_call(&mut self, call_expr: &Expression) -> Option<ValueTypePair> {
        match call_expr {
            Expression::Call {
                name: raw_name,
                types,
                args,
            } => {
                if self.has_internal(&raw_name) {
                    return self.get_internal(&raw_name).upgrade().unwrap()(
                        self,
                        &args,
                        raw_name.clone(),
                    );
                }

                let mut call_types = vec![];

                let mut error = false;
                let mut call_args = args
                    .iter()
                    .filter_map(|arg| match arg {
                        Expression::Atom(AtomKind::SymbolLiteral(sym)) => {
                            call_types.push(PrimitiveType::Symbol(sym.clone()));
                            None
                        }
                        _ => {
                            let expr = self.codegen_expr(arg);

                            if expr.is_none() {
                                error = true;
                                None
                            } else {
                                expr.map_or(None, |arg| match arg.prim().clone() {
                                    PrimitiveType::VariantType(name) => {
                                        call_types.push(PrimitiveType::VariantType(name.clone()));
                                        let ty =
                                            self.typectx_ref().get_parent_for_variant(&name)?;
                                        Some(core::load(
                                            self,
                                            cast::bitcast_to(self, *arg, ty::pointer(self, ty)),
                                        ))
                                    }
                                    PrimitiveType::Array { ty: box ty, len } => {
                                        let arr =
                                            cast::bitcast_to(self, *arg, ty::slice_ptr(self, &ty));
                                        let slice = value::slice(self, arr, ty.clone(), len);

                                        call_types.push(PrimitiveType::Slice { ty: box ty });
                                        Some(*slice)
                                    }
                                    PrimitiveType::Ref(box PrimitiveType::Array {
                                        ty: box ty,
                                        len,
                                    }) => {
                                        let slice = core::alloca(self, ty::slice_type(self, &ty));
                                        let arr =
                                            cast::bitcast_to(self, *arg, ty::slice_ptr(self, &ty));

                                        let arr_ptr = core::struct_gep(self, slice, 0);
                                        let len_ptr = core::struct_gep(self, slice, 1);
                                        core::store(self, arr, arr_ptr);
                                        core::store(self, *value::u32(self, len), len_ptr);

                                        call_types.push(PrimitiveType::Ref(
                                            box PrimitiveType::Slice { ty: box ty },
                                        ));
                                        Some(slice)
                                    }
                                    _ => {
                                        call_types.push(arg.prim().clone());
                                        Some(*arg)
                                    }
                                })
                            }
                        }
                    })
                    .collect::<Vec<_>>();
                if error {
                    return None;
                }

                let name = if (!self.is_extern(raw_name) || types.len() > 0) && call_types.len() > 0
                {
                    format!(
                        "{}{}A{}",
                        raw_name,
                        if types.len() > 0 {
                            format!("G{}", types.len())
                        } else {
                            String::default()
                        },
                        call_types
                            .iter()
                            .map(|arg| arg.mangle(self))
                            .collect::<Vec<_>>()
                            .join(".")
                    )
                } else {
                    raw_name.clone()
                };

                let mut mismatch = false;

                let params = match self.get_params_generic(&raw_name, &name, types.to_vec()) {
                    None => {
                        self.add_error(CompileError::UnknownFunction {
                            name: format!(
                                "{}{}",
                                raw_name.clone(),
                                if types.len() > 0 {
                                    format!(
                                        "[{}]",
                                        types
                                            .iter()
                                            .map(|ty| ty.fmt())
                                            .collect::<Vec<_>>()
                                            .join(", ")
                                    )
                                } else {
                                    String::default()
                                }
                            ),
                            values: args
                                .iter()
                                .map(|val| format!("{}", val.fmt()))
                                .collect::<Vec<_>>()
                                .join(", "),
                        });
                        return None;
                    }
                    Some(params) => params,
                };

                for (i, fty) in params.iter().enumerate() {
                    match fty {
                        PrimitiveType::Slice { ty: box ty } => match &call_types[i] {
                            PrimitiveType::Array { ty: box aty, .. } => {
                                if ty != aty {
                                    mismatch = true;
                                };
                            }
                            _ => mismatch = true,
                        },
                        PrimitiveType::Pointer(ty) => match &call_types[i] {
                            PrimitiveType::Ref(pty) | PrimitiveType::Pointer(pty) => {
                                if ty != pty {
                                    mismatch = true;
                                }
                            }
                            _ => mismatch = true,
                        },
                        PrimitiveType::TypeRef(name) => match &call_types[i] {
                            PrimitiveType::VariantType(var_name) => {
                                if &self
                                    .typectx_ref()
                                    .get_parent_for_variant(var_name)
                                    .expect("")
                                    .type_name
                                    != name
                                {
                                    mismatch = true
                                }
                            }
                            PrimitiveType::TypeRef(tr) => {
                                if name != tr {
                                    mismatch = true;
                                }
                            }
                            _ => mismatch = true,
                        },
                        PrimitiveType::Varargs => break,
                        _ => mismatch = &call_types[i] != fty,
                    }
                }

                if mismatch {
                    println!("{:?} {:?}", params, call_types);
                    self.add_error(CompileError::MismatchedArguments {
                        function: raw_name.clone(),
                        expected_ty: params
                            .iter()
                            .map(|p| p.fmt())
                            .collect::<Vec<_>>()
                            .join(", "),
                        found_ty: call_types
                            .iter()
                            .map(|arg| arg.fmt())
                            .collect::<Vec<_>>()
                            .join(", "),
                    });
                }

                Some(core::call(
                    self,
                    self.get_func(&name).unwrap(),
                    &mut call_args,
                ))
            }
            _ => unreachable!(),
        }
    }

    pub fn internal_len(
        module: &mut AatbeModule,
        values: &Vec<Expression>,
        _name: String,
    ) -> Option<ValueTypePair> {
        if values.len() != 1 {
            return None;
        }
        let arr = module.codegen_expr(&values[0])?;
        match arr.prim() {
            PrimitiveType::Array { len, .. } => {
                return Some(value::u32(module, *len));
            }
            PrimitiveType::Slice { .. } => {
                return Some(core::extract_u32(module, *arr, 1));
            }
            _ => {
                module.add_error(CompileError::MismatchedArguments {
                    function: String::from("len"),
                    expected_ty: String::from("[x; n]"),
                    found_ty: arr.prim().clone().fmt(),
                });
                return None;
            }
        }
    }
}
