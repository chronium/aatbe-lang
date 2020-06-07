use crate::{
    codegen::{
        builder::{cast, core, ty, value},
        mangle_v1::NameMangler,
        unit::function::find_call,
        AatbeModule, CompileError, ValueTypePair,
    },
    fmt::AatbeFmt,
};

use parser::ast::{AtomKind, Expression, PrimitiveType};

impl AatbeModule {
    pub fn codegen_call(&mut self, call_expr: &Expression) -> Option<ValueTypePair> {
        match call_expr {
            Expression::Call { name, types, args } => {
                if self.has_internal(&name) {
                    return self.get_internal(&name).upgrade().unwrap()(self, &args, name.clone());
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

                let group = self.get_func_group(name);

                if group.is_none() {
                    let args = args
                        .iter()
                        .map(|val| format!("{}", val.fmt()))
                        .collect::<Vec<_>>()
                        .join(", ");
                    self.add_error(CompileError::UnknownFunction {
                        name: name.clone(),
                        values: args,
                    });
                    return None;
                }

                let func = find_call(group.unwrap(), &call_types);

                if func.is_none() {
                    let found = group
                        .unwrap()
                        .iter()
                        .map(|gr| gr.fmt())
                        .collect::<Vec<_>>()
                        .join("\t\n");
                    let args = args
                        .iter()
                        .map(|val| format!("{}", val.fmt()))
                        .collect::<Vec<_>>()
                        .join(", ");
                    self.add_error(CompileError::NoFunctionOverload {
                        name: name.clone(),
                        found,
                        values: args,
                    });
                    return None;
                }

                Some(core::call(self, func.unwrap(), &mut call_args))
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
