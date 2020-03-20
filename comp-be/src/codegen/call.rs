use crate::{
    codegen::{mangle_v1::NameMangler, AatbeModule, CompileError, ValueTypePair},
    fmt::AatbeFmt,
    ty::LLVMTyInCtx,
};

use parser::ast::{AtomKind, Expression, IntSize, PrimitiveType};

use llvm_sys_wrapper::Struct;

impl AatbeModule {
    pub fn codegen_call(&mut self, call: &Expression) -> Option<ValueTypePair> {
        match call {
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

                let mut call_args = args
                    .iter()
                    .filter_map(|arg| match arg {
                        Expression::Atom(AtomKind::SymbolLiteral(sym)) => {
                            call_types.push(PrimitiveType::TypeRef(sym.clone()));
                            None
                        }
                        _ => self
                            .codegen_expr(arg)
                            .map_or(None, |arg| match arg.prim().clone() {
                                PrimitiveType::Array { ty: box ty, len } => {
                                    let arr = self.llvm_builder_ref().build_bitcast(
                                        *arg,
                                        self.llvm_context_ref().PointerType(
                                            self.llvm_context_ref()
                                                .ArrayType(ty.llvm_ty_in_ctx(self), 0),
                                        ),
                                    );
                                    let slice = Struct::new_const_struct(
                                        &mut [arr, self.llvm_context_ref().UInt32(len as u64)],
                                        false,
                                    );

                                    call_types.push(PrimitiveType::Slice { ty: box ty });
                                    Some(slice)
                                }
                                PrimitiveType::Ref(box PrimitiveType::Array {
                                    ty: box ty,
                                    len,
                                }) => {
                                    let slice_ty = self.llvm_context_ref().StructType(
                                        &mut vec![
                                            self.llvm_context_ref().PointerType(
                                                self.llvm_context_ref()
                                                    .ArrayType(ty.llvm_ty_in_ctx(self), 0),
                                            ),
                                            self.llvm_context_ref().Int32Type(),
                                        ],
                                        false,
                                    );
                                    let slice =
                                        self.llvm_builder_ref().build_alloca(slice_ty.as_ref());
                                    let arr = self.llvm_builder_ref().build_bitcast(
                                        *arg,
                                        self.llvm_context_ref().PointerType(
                                            self.llvm_context_ref()
                                                .ArrayType(ty.llvm_ty_in_ctx(self), 0),
                                        ),
                                    );

                                    let arr_ptr =
                                        self.llvm_builder_ref().build_struct_gep(slice, 0);
                                    let len_ptr =
                                        self.llvm_builder_ref().build_struct_gep(slice, 1);
                                    self.llvm_builder_ref().build_store(arr, arr_ptr);
                                    self.llvm_builder_ref().build_store(
                                        self.llvm_context_ref().UInt32(len as u64),
                                        len_ptr,
                                    );

                                    call_types.push(PrimitiveType::Ref(box PrimitiveType::Slice {
                                        ty: box ty,
                                    }));
                                    Some(slice)
                                }
                                _ => {
                                    call_types.push(arg.prim().clone());
                                    Some(*arg)
                                }
                            }),
                    })
                    .collect::<Vec<_>>();

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
                            .map(|arg| arg.mangle())
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
                        PrimitiveType::Varargs => break,
                        _ => mismatch = &call_types[i] != fty,
                    }
                }

                if mismatch {
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

                let func = self.get_func(&name).unwrap();

                Some(
                    (
                        self.llvm_builder_ref()
                            .build_call(func.into(), &mut call_args),
                        func.ret_ty(),
                    )
                        .into(),
                )
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
                return Some(
                    (
                        module.llvm_context_ref().SInt32(*len as u64),
                        PrimitiveType::Int(IntSize::Bits32),
                    )
                        .into(),
                );
            }
            PrimitiveType::Slice { .. } => {
                return Some(
                    (
                        module.llvm_builder_ref().build_extract_value(*arr, 1),
                        PrimitiveType::Int(IntSize::Bits32),
                    )
                        .into(),
                );
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
