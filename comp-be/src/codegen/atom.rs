use crate::{
    codegen::{AatbeModule, CompileError, ValueTypePair},
    fmt::AatbeFmt,
    ty::{LLVMTyInCtx, TypeKind},
};

use parser::ast::{AtomKind, Boolean, IntSize, PrimitiveType};

impl AatbeModule {
    pub fn codegen_atom(&mut self, atom: &AtomKind) -> Option<ValueTypePair> {
        match atom {
            AtomKind::Cast(box val, ty) => {
                let (val, _) = self
                    .codegen_atom(val)
                    .expect("ICE codegen_atom cast val")
                    .into();

                Some(
                    (
                        self.llvm_builder_ref()
                            .build_bitcast(val, ty.llvm_ty_in_ctx(self)),
                        TypeKind::Primitive(ty.clone()),
                    )
                        .into(),
                )
            }
            AtomKind::Index(box val, box index) => {
                let index = self.codegen_expr(index).expect("ICE codegen_atom index");
                let (val, ty) = self
                    .codegen_atom(val)
                    .expect("ICE codegen_atom index val")
                    .indexable()
                    .expect(format!("{:?} is not indexable", val).as_str())
                    .into();

                let gep = self
                    .llvm_builder_ref()
                    .build_inbounds_gep(val, &mut [index.val()]);

                let ty = match ty {
                    TypeKind::Primitive(PrimitiveType::Str) => {
                        TypeKind::Primitive(PrimitiveType::Char)
                    }
                    _ => panic!("ICE ty[] {:?}", ty),
                };

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
                            .build_load_with_name(int.val(), path.join(".").as_str()),
                        int.ty(),
                    )
                        .into(),
                )
            }
            AtomKind::Ident(name) => {
                let var_ref = self.get_var(name);

                match var_ref {
                    None => panic!("Cannot find variable {}", name),
                    Some(var) => Some(
                        (
                            var.load_var(self.llvm_builder_ref()),
                            TypeKind::Primitive(var.var_ty().clone()),
                        )
                            .into(),
                    ),
                }
            }
            AtomKind::StringLiteral(string) => Some(
                (
                    self.llvm_builder_ref()
                        .build_global_string_ptr(string.as_str()),
                    TypeKind::Primitive(PrimitiveType::Str),
                )
                    .into(),
            ),
            AtomKind::CharLiteral(ch) => Some(
                (
                    self.llvm_context_ref().SInt8(*ch as u64),
                    TypeKind::Primitive(PrimitiveType::Char),
                )
                    .into(),
            ),
            AtomKind::Integer(val, prim @ PrimitiveType::Int(IntSize::Bits32)) => Some(
                (
                    self.llvm_context_ref().SInt32(*val),
                    TypeKind::Primitive(prim.clone()),
                )
                    .into(),
            ),
            AtomKind::Bool(Boolean::True) => Some(
                (
                    self.llvm_context_ref().SInt1(1),
                    TypeKind::Primitive(PrimitiveType::Bool),
                )
                    .into(),
            ),
            AtomKind::Bool(Boolean::False) => Some(
                (
                    self.llvm_context_ref().SInt1(0),
                    TypeKind::Primitive(PrimitiveType::Bool),
                )
                    .into(),
            ),
            AtomKind::Expr(expr) => self.codegen_expr(expr),
            AtomKind::Unit => None,
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
                        self.llvm_builder_ref().build_not(value.val()),
                        TypeKind::Primitive(PrimitiveType::Bool),
                    )
                        .into(),
                )
            }
            AtomKind::Parenthesized(expr) => self.codegen_expr(expr),
            _ => panic!("ICE codegen_atom {:?}", atom),
        }
    }
}
