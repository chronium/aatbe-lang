use crate::{
    codegen::{mangle_v1::NameMangler, unit::Mutability, AatbeModule, CodegenUnit},
    ty::LLVMTyInCtx,
};

use parser::ast::{Expression, PrimitiveType};

pub fn declare_function(module: &mut AatbeModule, function: &Expression) {
    match function {
        Expression::Function {
            name: _,
            ty,
            attributes: _,
            body: _,
        } => {
            let name = function.mangle();

            let func = module
                .llvm_module_ref()
                .get_or_add_function(&name, ty.llvm_ty_in_ctx(module));

            module.push_in_scope(&name, CodegenUnit::Function(func, ty.clone()));
        }
        _ => panic!("Unimplemented declare_function {:?}", function),
    }
}

pub fn codegen_function(module: &mut AatbeModule, function: &Expression) {
    match function {
        Expression::Function {
            name,
            ty: _,
            attributes,
            body: _,
        } => {
            let func = module.get_func(&function.mangle()).unwrap();

            if !attributes.is_empty() {
                for attr in attributes {
                    match attr.to_lowercase().as_ref() {
                        "entry" => module
                            .llvm_builder_ref()
                            .position_at_end(func.append_basic_block("entry".to_string())),
                        _ => panic!("Cannot decorate function with {}", name),
                    };
                }
            } else {
                module
                    .llvm_builder_ref()
                    .position_at_end(func.append_basic_block(String::default()));
            }
        }
        _ => unreachable!(),
    }
}

pub fn inject_function_in_scope(module: &mut AatbeModule, function: &Expression) {
    match function {
        Expression::Function {
            name: _,
            ty,
            attributes: _,
            body: _,
        } => {
            let fun_name = function.mangle();
            match ty {
                PrimitiveType::Function {
                    ret_ty: _,
                    params,
                    ext: false,
                } => {
                    for (pos, ty) in params
                        .into_iter()
                        .filter(|ty| match ty {
                            PrimitiveType::TypeRef(_name) => false,
                            _ => true,
                        })
                        .enumerate()
                    {
                        match ty {
                            PrimitiveType::NamedType {
                                name,
                                ty: box PrimitiveType::TypeRef(_),
                            } => {
                                let arg = module
                                    .get_func(&fun_name)
                                    .expect("Compiler borked. Functions borked")
                                    .get_param(pos as u32);
                                let llty = ty.llvm_ty_in_ctx(module);
                                let ptr = module
                                    .llvm_builder_ref()
                                    .build_alloca_with_name(llty, name.as_ref());
                                module.llvm_builder_ref().build_store(arg, ptr);
                                module.push_in_scope(
                                    name,
                                    CodegenUnit::Variable {
                                        mutable: Mutability::Immutable,
                                        name: name.clone(),
                                        ty: ty.clone(),
                                        value: ptr,
                                    },
                                );
                            }
                            PrimitiveType::NamedType { name, ty } => {
                                module.push_in_scope(
                                    name,
                                    CodegenUnit::FunctionArgument(
                                        module
                                            .get_func(&fun_name)
                                            .expect("Compiler borked. Functions borked")
                                            .get_param(pos as u32),
                                        *ty.clone(),
                                    ),
                                );
                            }
                            PrimitiveType::Unit => {}
                            _ => panic!("ICE: Unimplemented func args for {:?}", ty),
                        }
                    }
                }
                _ => unreachable!(),
            };
        }
        _ => unreachable!(),
    }
}
