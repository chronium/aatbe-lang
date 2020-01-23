use crate::{
    codegen::{unit::Mutability, AatbeModule, CodegenUnit},
    ty::{record::store_named_field, LLVMTyInCtx},
};

use parser::ast::{AtomKind, Expression, PrimitiveType};

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
                        name,
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

pub fn init_record(module: &mut AatbeModule, ref_name: &String, rec: &AtomKind) -> LLVMValueRef {
    match rec {
        AtomKind::RecordInit { record, values } => {
            let var_ref = module
                .get_var(ref_name)
                .expect(format!("ICE Cannot find record {} instance {}", record, ref_name).as_str())
                .into();

            values.iter().for_each(|val| match val {
                AtomKind::NamedValue { name, val } => {
                    let val_ref = module
                        .codegen_expr(val)
                        .expect(format!("ICE could not codegen {:?}", val).as_str());
                    store_named_field(
                        module,
                        var_ref,
                        ref_name,
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

            var_ref
        }
        _ => unreachable!(),
    }
}

pub fn store_value(module: &mut AatbeModule, name: &String, value: &Expression) -> LLVMValueRef {
    let parts = name
        .split('.')
        .collect::<Vec<&str>>()
        .iter()
        .map(|s| String::from(*s))
        .collect::<Vec<String>>();

    let var: LLVMValueRef = if parts.len() == 1 {
        let var_ref = module.get_var(name);
        match var_ref {
            None => panic!("Cannot find variable {}", name),
            Some(var) => {
                match var.get_mutability() {
                    Mutability::Mutable => {}
                    _ => panic!("Cannot reassign to immutable/constant {}", name),
                };
                var.into()
            }
        }
    } else {
        module.get_interior_pointer(parts)
    };

    let val = module
        .codegen_expr(value)
        .expect(format!("Cannot codegen assignment for {} value", name).as_ref());

    module.llvm_builder_ref().build_store(val, var)
}
