use crate::{
    codegen::{unit::Mutability, AatbeModule, CodegenUnit},
    ty::{record::store_named_field, LLVMTyInCtx},
};

use parser::ast::{AtomKind, Expression, LValue, PrimitiveType};

use llvm_sys_wrapper::LLVMValueRef;

pub fn alloc_variable(module: &mut AatbeModule, variable: &Expression) {
    match variable {
        Expression::Decl {
            ty: PrimitiveType::NamedType { name, ty },
            value,
            exterior_bind,
        } => {
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
                        ty: ty.clone(),
                    },
                    value: var_ref,
                },
            );

            if let Some(e) = value {
                if let box Expression::Atom(AtomKind::RecordInit { record, values }) = e {
                    init_record(
                        module,
                        &LValue::Ident(name.clone()),
                        &AtomKind::RecordInit {
                            record: record.clone(),
                            values: values.to_vec(),
                        },
                    );
                } else {
                    let val = module
                        .codegen_expr(e)
                        .expect(format!("Cannot codegen variable {} value", name).as_ref());
                    module.llvm_builder_ref().build_store(val, var_ref);
                }
            }
        }
        _ => unreachable!(),
    }
}

pub fn init_record(module: &mut AatbeModule, lval: &LValue, rec: &AtomKind) -> LLVMValueRef {
    match rec {
        AtomKind::RecordInit { record, values } => {
            let var: LLVMValueRef = match lval {
                LValue::Ident(name) => match module.get_var(name) {
                    None => panic!("Cannot find variable {}", name),
                    Some(var) => var.into(),
                },
                LValue::Accessor(parts) => module.get_interior_pointer(parts.clone()),
            };

            values.iter().for_each(|val| match val {
                AtomKind::NamedValue { name, val } => {
                    let val_ref = module
                        .codegen_expr(val)
                        .expect(format!("ICE could not codegen {:?}", val).as_str());
                    store_named_field(
                        module,
                        var,
                        &lval.into(),
                        module
                            .typectx_ref()
                            .get_type(record)
                            .expect(format!("ICE could not find record {}", record).as_str()),
                        name,
                        val_ref,
                    );
                }
                _ => panic!("ICE init_record unexpected {:?}", val),
            });

            var
        }
        _ => unreachable!(),
    }
}

pub fn store_value(module: &mut AatbeModule, lval: &LValue, value: &Expression) -> LLVMValueRef {
    let var: LLVMValueRef = match lval {
        LValue::Ident(name) => match module.get_var(name) {
            None => panic!("Cannot find variable {}", name),
            Some(var) => {
                match var.get_mutability() {
                    Mutability::Mutable => {}
                    _ => panic!("Cannot reassign to immutable/constant {}", name),
                };
                var.into()
            }
        },
        LValue::Accessor(parts) => module.get_interior_pointer(parts.clone()),
    };

    let val = module
        .codegen_expr(value)
        .expect(format!("Cannot codegen assignment for {:?} value", lval).as_ref());

    module.llvm_builder_ref().build_store(val, var)
}
