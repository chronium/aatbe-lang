use crate::{
    codegen::{unit::Mutability, AatbeModule, CodegenUnit},
    ty::LLVMTyInCtx,
};

use parser::ast::{Expression, PrimitiveType};

use llvm_sys_wrapper::LLVMValueRef;

pub fn alloc_variable(module: &mut AatbeModule, variable: &Expression) {
    match variable {
        Expression::Decl {
            ty: PrimitiveType::NamedType { name, ty },
            value,
            exterior_bind,
        } => {
            let var_ref = module.llvm_builder_ref().build_alloca_with_name(
                ty.llvm_ty_in_ctx(module.llvm_context_ref()),
                name.as_ref(),
            );

            if let Some(e) = value {
                let val = module
                    .codegen_expr(e)
                    .expect(format!("Cannot codegen variable {} value", name).as_ref());
                module.llvm_builder_ref().build_store(val, var_ref);
            }

            module.push_ref_in_scope(
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
        }
        _ => unreachable!(),
    }
}

pub fn store_value(module: &mut AatbeModule, name: &String, value: &Expression) -> LLVMValueRef {
    let var_ref = module.get_var(name);

    let var = match var_ref {
        None => panic!("Cannot find variable {}", name),
        Some(var) => {
            match var.get_mutability() {
                Mutability::Mutable => {}
                _ => panic!("Cannot reassign to immutable/constant {}", name),
            };
            var.into()
        }
    };

    let val = module
        .codegen_expr(value)
        .expect(format!("Cannot codegen assignment for {} value", name).as_ref());

    module.llvm_builder_ref().build_store(val, var)
}
