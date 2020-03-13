use crate::{
    codegen::{expr::const_expr::const_atom, AatbeModule, CompileError, ValueTypePair},
    fmt::AatbeFmt,
    ty::{LLVMTyInCtx, TypeKind},
};

use parser::ast::{AtomKind, Boolean, FloatSize, PrimitiveType};

impl AatbeModule {
    pub fn codegen_atom(&mut self, atom: &AtomKind) -> Option<ValueTypePair> {
        match atom {
            AtomKind::Cast(box val, ty) => {
                let (val, val_ty) = self
                    .codegen_atom(val)
                    .expect("ICE codegen_atom cast val")
                    .into();
                match (val_ty, ty) {
                    (TypeKind::Primitive(PrimitiveType::Int(_)), PrimitiveType::Pointer(_)) => {
                        return Some(
                            (
                                self.llvm_builder_ref()
                                    .build_int_to_ptr(val, ty.llvm_ty_in_ctx(self)),
                                ty.clone(),
                            )
                                .into(),
                        )
                    }
                    (TypeKind::Primitive(PrimitiveType::Int(_)), PrimitiveType::Str) => {
                        return Some(
                            (
                                self.llvm_builder_ref()
                                    .build_int_to_ptr(val, ty.llvm_ty_in_ctx(self)),
                                ty.clone(),
                            )
                                .into(),
                        )
                    }
                    (TypeKind::Primitive(PrimitiveType::Int(_)), PrimitiveType::Float(_)) => {
                        return Some(
                            (
                                self.llvm_builder_ref()
                                    .build_si_to_fp(val, ty.llvm_ty_in_ctx(self)),
                                ty.clone(),
                            )
                                .into(),
                        )
                    }
                    (TypeKind::Primitive(PrimitiveType::UInt(_)), PrimitiveType::Float(_)) => {
                        return Some(
                            (
                                self.llvm_builder_ref()
                                    .build_ui_to_fp(val, ty.llvm_ty_in_ctx(self)),
                                ty.clone(),
                            )
                                .into(),
                        )
                    }
                    (
                        TypeKind::Primitive(PrimitiveType::Float(FloatSize::Bits64)),
                        PrimitiveType::Float(FloatSize::Bits32),
                    ) => {
                        return Some(
                            (
                                self.llvm_builder_ref()
                                    .build_fp_trunc(val, ty.llvm_ty_in_ctx(self)),
                                ty.clone(),
                            )
                                .into(),
                        )
                    }
                    (TypeKind::Primitive(PrimitiveType::Float(_)), PrimitiveType::Int(_)) => {
                        return Some(
                            (
                                self.llvm_builder_ref()
                                    .build_fp_to_si(val, ty.llvm_ty_in_ctx(self)),
                                ty.clone(),
                            )
                                .into(),
                        )
                    }
                    (TypeKind::Primitive(PrimitiveType::Float(_)), PrimitiveType::UInt(_)) => {
                        return Some(
                            (
                                self.llvm_builder_ref()
                                    .build_fp_to_ui(val, ty.llvm_ty_in_ctx(self)),
                                ty.clone(),
                            )
                                .into(),
                        )
                    }
                    _ => {}
                };

                Some(
                    (
                        self.llvm_builder_ref()
                            .build_bitcast(val, ty.llvm_ty_in_ctx(self)),
                        ty.clone(),
                    )
                        .into(),
                )
            }
            AtomKind::Index(box val, box index) => {
                let index = self.codegen_expr(index).expect("ICE codegen_atom index");
                let (val, ty) = self
                    .codegen_atom(val)
                    .expect("ICE codegen_atom index val")
                    .indexable(self)
                    .expect(format!("{:?} is not indexable", val).as_str())
                    .into();

                let mut arr = false;
                let ty = match ty {
                    TypeKind::Primitive(PrimitiveType::Str) => {
                        TypeKind::Primitive(PrimitiveType::Char)
                    }
                    TypeKind::Primitive(PrimitiveType::Array { ty: box ty, .. }) => {
                        arr = true;

                        TypeKind::Primitive(ty)
                    }
                    TypeKind::Primitive(PrimitiveType::Slice { ty: box ty }) => {
                        arr = true;

                        TypeKind::Primitive(ty)
                    }
                    _ => ty,
                };

                let mut ind = vec![];
                if arr {
                    ind.push(self.llvm_context_ref().UInt32(0));
                }
                ind.push(*index);

                let gep = self.llvm_builder_ref().build_inbounds_gep(val, &mut ind);

                Some((self.llvm_builder_ref().build_load(gep), ty).into())
            }
            AtomKind::NamedValue { name: _, val } => self.codegen_expr(&*val),
            /*AtomKind::Deref(path) => {
                let acc = self.codegen_atom(path).expect("");
                Some(self.llvm_builder_ref().build_load(acc))
            }*/
            AtomKind::Access(path) => {
                let int = self.get_interior_pointer(path.to_vec());
                Some(
                    (
                        self.llvm_builder_ref()
                            .build_load_with_name(*int, path.join(".").as_str()),
                        int.ty(),
                    )
                        .into(),
                )
            }
            AtomKind::Ident(name) => {
                let var_ref = self.get_var(name)?;

                Some(
                    (
                        var_ref.load_var(self.llvm_builder_ref()),
                        var_ref.var_ty().clone(),
                    )
                        .into(),
                )
            }
            AtomKind::Ref(box AtomKind::Ident(name)) => {
                let var_ref = self.get_var(name)?;

                let var: ValueTypePair = var_ref.into();

                Some((*var, PrimitiveType::Ref(box var.prim().clone())).into())
            }
            atom @ (AtomKind::StringLiteral(_) | AtomKind::CharLiteral(_)) => {
                const_atom(self, atom)
            }
            atom @ AtomKind::Integer(_, _) => const_atom(self, atom),
            atom @ AtomKind::Floating(_, _) => const_atom(self, atom),
            AtomKind::Bool(Boolean::True) => {
                Some((self.llvm_context_ref().SInt1(1), PrimitiveType::Bool).into())
            }
            AtomKind::Bool(Boolean::False) => {
                Some((self.llvm_context_ref().SInt1(0), PrimitiveType::Bool).into())
            }
            AtomKind::Expr(expr) => self.codegen_expr(expr),
            AtomKind::Unit => None,
            AtomKind::Unary(op, val) if op == &String::from("-") => {
                let value = self
                    .codegen_atom(val)
                    .expect(format!("ICE Cannot negate {:?}", val).as_str());

                match value.prim() {
                    prim @ (PrimitiveType::Int(_) | PrimitiveType::UInt(_)) => {
                        Some((self.llvm_builder_ref().build_neg(*value), prim.clone()).into())
                    }
                    prim @ PrimitiveType::Float(_) => {
                        Some((self.llvm_builder_ref().build_fneg(*value), prim.clone()).into())
                    }
                    _ => {
                        self.add_error(CompileError::UnaryMismatch {
                            op: op.clone(),
                            expected_ty: String::from("any number/float"),
                            found_ty: value.prim().fmt(),
                            value: val.fmt(),
                        });
                        None
                    }
                }
            }
            AtomKind::Unary(op, val) if op == &String::from("!") => {
                let value = self
                    .codegen_atom(val)
                    .expect(format!("ICE Cannot negate {:?}", val).as_str());

                if value.prim() != &PrimitiveType::Bool {
                    self.add_error(CompileError::UnaryMismatch {
                        op: op.clone(),
                        expected_ty: PrimitiveType::Bool.fmt(),
                        found_ty: value.prim().fmt(),
                        value: val.fmt(),
                    })
                }

                Some(
                    (
                        self.llvm_builder_ref().build_not(*value),
                        PrimitiveType::Bool,
                    )
                        .into(),
                )
            }
            AtomKind::Parenthesized(expr) => self.codegen_expr(expr),
            AtomKind::Array(exprs) => {
                let values = exprs
                    .iter()
                    .filter_map(|expr| self.codegen_expr(expr))
                    .collect::<Vec<_>>();
                if exprs.len() != values.len() {
                    return None;
                }

                let types = values
                    .iter()
                    .map(|val| val.prim().clone())
                    .collect::<Vec<_>>();

                let fst = types.first().unwrap();
                if !types.iter().all(|ty| ty == fst) {
                    self.add_error(CompileError::ArrayTypesNotUniform {
                        values: format!(
                            "[{}]",
                            exprs
                                .iter()
                                .map(|val| val.fmt())
                                .collect::<Vec<_>>()
                                .join(", ")
                        ),
                    });

                    return None;
                };

                let len = values.len() as u32;
                Some(
                    (
                        self.llvm_context_ref().ConstArray(
                            fst.clone().llvm_ty_in_ctx(self),
                            &mut values.iter().map(|val| **val).collect::<Vec<_>>(),
                            len,
                        ),
                        PrimitiveType::Array {
                            ty: box fst.clone(),
                            len: len,
                        },
                    )
                        .into(),
                )
            }
            _ => panic!("ICE codegen_atom {:?}", atom),
        }
    }
}
