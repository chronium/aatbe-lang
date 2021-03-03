use crate::{
    codegen::{
        builder::{cast, core, op, ty},
        expr::const_expr::const_atom,
        unit::Slot,
        AatbeModule, CompileError, ValueTypePair,
    },
    fmt::AatbeFmt,
    ty::{LLVMTyInCtx, TypeKind, TypedefKind},
};

use super::builder::value;
use parser::ast::{AtomKind, Boolean, FloatSize, PrimitiveType};

impl AatbeModule {
    pub fn codegen_atom(&mut self, atom: &AtomKind) -> Option<ValueTypePair> {
        todo!()
        /*match atom {
            AtomKind::Cast(box val, ty) => {
                let val = self.codegen_atom(val)?;
                match (val.prim(), ty) {
                    (PrimitiveType::Char, PrimitiveType::UInt(_))
                    | (PrimitiveType::Char, PrimitiveType::Int(_)) => {
                        return Some(cast::zext(self, val, ty))
                    }
                    (PrimitiveType::Int(from), PrimitiveType::Int(to)) if from < to => {
                        return Some(cast::zext(self, val, ty))
                    }
                    (PrimitiveType::Int(from), PrimitiveType::Int(to)) if from > to => {
                        return Some(cast::trunc(self, val, ty))
                    }
                    (PrimitiveType::Int(from), PrimitiveType::UInt(to)) if from < to => {
                        return Some(cast::zext(self, val, ty))
                    }
                    (PrimitiveType::Int(from), PrimitiveType::UInt(to)) if from > to => {
                        return Some(cast::trunc(self, val, ty))
                    }
                    (PrimitiveType::UInt(from), PrimitiveType::Int(to)) if from < to => {
                        return Some(cast::zext(self, val, ty))
                    }
                    (PrimitiveType::UInt(from), PrimitiveType::Int(to)) if from > to => {
                        return Some(cast::trunc(self, val, ty))
                    }
                    (PrimitiveType::Int(_), PrimitiveType::Pointer(_)) => {
                        return Some(cast::itop(self, val, ty))
                    }
                    (PrimitiveType::Pointer(_), PrimitiveType::Int(_)) => {
                        return Some(cast::ptoi(self, val, ty))
                    }
                    (PrimitiveType::Str, PrimitiveType::Int(_)) => {
                        return Some(cast::ptoi(self, val, ty))
                    }
                    (PrimitiveType::Str, PrimitiveType::UInt(_)) => {
                        return Some(cast::ptoi(self, val, ty))
                    }
                    (PrimitiveType::UInt(_), PrimitiveType::Pointer(_)) => {
                        return Some(cast::itop(self, val, ty))
                    }
                    (PrimitiveType::Pointer(_), PrimitiveType::UInt(_)) => {
                        return Some(cast::ptoi(self, val, ty))
                    }
                    (PrimitiveType::Int(_), PrimitiveType::Str) => {
                        return Some(cast::itop(self, val, ty))
                    }
                    (PrimitiveType::Int(_), PrimitiveType::Float(_)) => {
                        return Some(cast::stof(self, val, ty))
                    }
                    (PrimitiveType::UInt(_), PrimitiveType::Float(_)) => {
                        return Some(cast::utof(self, val, ty))
                    }
                    (
                        PrimitiveType::Float(FloatSize::Bits64),
                        PrimitiveType::Float(FloatSize::Bits32),
                    ) => return Some(cast::ftrunc(self, val, ty)),
                    (PrimitiveType::Float(_), PrimitiveType::Int(_)) => {
                        return Some(cast::ftos(self, val, ty))
                    }
                    (PrimitiveType::Float(_), PrimitiveType::UInt(_)) => {
                        return Some(cast::ftou(self, val, ty))
                    }
                    // FIXME: Huge hack to work for llvm test
                    (PrimitiveType::Array { .. }, PrimitiveType::Slice { ty: box ty }) => {
                        return Some(
                            (
                                self.llvm_builder_ref().build_inbounds_gep(
                                    *val,
                                    &mut [
                                        self.llvm_context_ref().SInt32(0),
                                        self.llvm_context_ref().SInt32(0),
                                    ],
                                ),
                                PrimitiveType::Pointer(box ty.clone()),
                            )
                                .into(),
                        )
                    }
                    _ => {}
                };

                Some(cast::bitcast_ty(self, val, ty))
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
                    ind.push(*value::u32(self, 0));
                }
                ind.push(*index);

                Some(core::load_ty(
                    self,
                    core::inbounds_gep(self, val, &mut ind),
                    ty,
                ))
            }
            AtomKind::NamedValue { name: _, val } => self.codegen_expr(&*val),
            AtomKind::Deref(path) => {
                let acc = self.codegen_atom(path)?;
                match acc.prim() {
                    PrimitiveType::Newtype(name) => {
                        let val = core::struct_gep(self, *acc, 0);
                        match self.typectx_ref().get_type(name) {
                            Some(TypeKind::Typedef(TypedefKind::Newtype(_, ty))) => {
                                Some(core::load_prim(self, val, ty.clone()))
                            }
                            _ => unimplemented!(),
                        }
                    }
                    PrimitiveType::Box(box ty) => Some(core::load_prim(self, *acc, ty.clone())),
                    _ => unimplemented!(),
                }
            }
            AtomKind::Access(path) => {
                let int = self.get_interior_pointer(path.to_vec())?;
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

                match var_ref.var_ty() {
                    ty @ PrimitiveType::Newtype(_) | ty @ PrimitiveType::VariantType(_) => {
                        let val: ValueTypePair = var_ref.into();
                        Some((*val, ty).into())
                    }
                    ty => Some((var_ref.load_var(self.llvm_builder_ref()), ty).into()),
                }
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
            AtomKind::Bool(Boolean::True) => Some(value::t(self)),
            AtomKind::Bool(Boolean::False) => Some(value::f(self)),
            AtomKind::Expr(expr) => self.codegen_expr(expr),
            AtomKind::Unit => None,
            AtomKind::Unary(op, val) if op == &String::from("-") => {
                let value = self
                    .codegen_atom(val)
                    .expect(format!("ICE Cannot negate {:?}", val).as_str());

                match value.prim() {
                    PrimitiveType::Int(_) | PrimitiveType::UInt(_) => Some(op::neg(self, value)),
                    PrimitiveType::Float(_) => Some(op::fneg(self, value)),
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

                Some(op::not(self, value))
            }
            AtomKind::Parenthesized(expr) => self.codegen_expr(expr),
            /*AtomKind::Array(exprs) => {
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
            }*/
            AtomKind::Is(box val, ty) => match val {
                AtomKind::Ident(val) => {
                    let var_ref = self.get_var(val).expect("").clone();
                    if let PrimitiveType::TypeRef(var) = var_ref.var_ty().clone() {
                        let variant_ty = self.typectx_ref().get_typedef(&var);
                        match variant_ty {
                            Ok(TypedefKind::VariantType(variant)) if variant.has_variant(ty) => {
                                let discriminant =
                                    core::struct_gep(self, var_ref.clone().into(), 0);

                                let disc = self.typectx_ref().get_variant(ty).unwrap();
                                let var_disc = *const_atom(
                                    self,
                                    &AtomKind::Integer(
                                        disc.discriminant as u64,
                                        variant.discriminant_type.clone(),
                                    ),
                                )
                                .unwrap();

                                let variant_cast = cast::bitcast_to(
                                    self,
                                    var_ref.clone().into(),
                                    ty::pointer_to(self, disc.ty),
                                );

                                self.push_in_scope(
                                    val,
                                    Slot::Variable {
                                        mutable: var_ref.get_mutability().clone(),
                                        name: val.clone(),
                                        ty: PrimitiveType::Variant {
                                            parent: var,
                                            variant: ty.clone(),
                                        },
                                        value: variant_cast,
                                    },
                                );

                                Some(op::ieq(self, core::load(self, discriminant), var_disc))
                            }
                            _ => Some(value::f(self)),
                        }
                    } else {
                        panic!("{} must be a variant", val);
                    }
                }
                _ => panic!(
                    "ICE only identifiers are supported on a lhs for 'is' at {}",
                    atom.fmt()
                ),
            },
            _ => panic!("ICE codegen_atom {:?}", atom),
        }*/
    }
}
