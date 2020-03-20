use crate::{
    codegen::{AatbeModule, CompileError, ValueTypePair},
    fmt::AatbeFmt,
    ty::LLVMTyInCtx,
};

use parser::ast::{Expression, LoopType, PrimitiveType};

use llvm_sys_wrapper::Phi;

impl AatbeModule {
    pub fn codegen_if(&mut self, if_expr: &Expression) -> Option<ValueTypePair> {
        match if_expr {
            Expression::If {
                cond_expr,
                then_expr,
                else_expr,
                is_expr,
            } => {
                let then_bb = self.basic_block(String::default());
                let else_bb = if let Some(_) = else_expr {
                    Some(self.basic_block(String::default()))
                } else {
                    None
                };
                let end_bb = self.basic_block(String::default());

                let cond = self.codegen_expr(cond_expr)?;

                if *cond.prim().inner() != PrimitiveType::Bool {
                    self.add_error(CompileError::ExpectedType {
                        expected_ty: PrimitiveType::Bool.fmt(),
                        found_ty: cond.prim().fmt(),
                        value: cond_expr.fmt(),
                    });
                }

                self.llvm_builder_ref()
                    .build_cond_br(*cond, then_bb, else_bb.unwrap_or(end_bb));

                self.llvm_builder_ref().position_at_end(then_bb);

                let then_val = self.codegen_expr(then_expr);
                let else_val = if let Some(bb) = else_bb {
                    self.llvm_builder_ref().build_br(end_bb);
                    self.llvm_builder_ref().position_at_end(bb);

                    if let Some(else_expr) = else_expr {
                        self.codegen_expr(&*else_expr)
                    } else {
                        unreachable!()
                    }
                } else {
                    None
                };
                if !self.has_ret(then_expr) {
                    self.llvm_builder_ref().build_br(end_bb);
                }
                self.llvm_builder_ref().position_at_end(end_bb);

                if !is_expr {
                    return None;
                }

                if let Some(then_val) = then_val {
                    let ty = then_val.prim().clone();
                    if ty != PrimitiveType::Unit {
                        if else_val.is_some() {
                            let phi = Phi::new(
                                self.llvm_builder_ref().as_ref(),
                                ty.llvm_ty_in_ctx(self),
                                "",
                            );

                            phi.add_incoming(*then_val, then_bb);

                            if let Some(else_val) = else_val {
                                if &ty != else_val.prim() {
                                    self.add_error(CompileError::ExpectedType {
                                        expected_ty: ty.clone().fmt(),
                                        found_ty: else_val.prim().fmt(),
                                        value: else_expr.as_ref().unwrap().fmt(),
                                    });
                                }
                                phi.add_incoming(*else_val, else_bb.unwrap());
                            }

                            Some((phi.as_ref(), ty).into())
                        } else {
                            Some((*then_val, ty).into())
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn codegen_basic_loop(&mut self, basic_loop: &Expression) -> Option<ValueTypePair> {
        match basic_loop {
            Expression::Loop {
                loop_type,
                cond_expr,
                body,
            } => {
                let cond_bb = self.basic_block(String::default());
                let body_bb = self.basic_block(String::default());
                let end_bb = self.basic_block(String::default());

                self.llvm_builder_ref().build_br(cond_bb);
                self.llvm_builder_ref().position_at_end(cond_bb);
                let cond = self.codegen_expr(cond_expr)?;

                if *cond.prim().inner() != PrimitiveType::Bool {
                    self.add_error(CompileError::ExpectedType {
                        expected_ty: PrimitiveType::Bool.fmt(),
                        found_ty: cond.prim().fmt(),
                        value: cond_expr.fmt(),
                    });
                };

                match loop_type {
                    LoopType::While => self
                        .llvm_builder_ref()
                        .build_cond_br(*cond, body_bb, end_bb),
                    LoopType::Until => self
                        .llvm_builder_ref()
                        .build_cond_br(*cond, end_bb, body_bb),
                };

                self.llvm_builder_ref().position_at_end(body_bb);
                self.codegen_expr(body);

                self.llvm_builder_ref().build_br(cond_bb);
                self.llvm_builder_ref().position_at_end(end_bb);

                None
            }
            _ => unreachable!(),
        }
    }
}
