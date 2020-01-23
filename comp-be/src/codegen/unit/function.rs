use crate::{
    codegen::{AatbeModule, CodegenUnit},
    ty::LLVMTyInCtx,
};

use parser::ast::{Expression, PrimitiveType};

pub fn declare_function(module: &mut AatbeModule, function: &Expression) {
    match function {
        Expression::Function {
            name,
            ty,
            attributes: _,
            body: _,
        } => {
            let func = module
                .llvm_module_ref()
                .get_or_add_function(name, ty.llvm_ty_in_ctx(module));

            module.push_in_scope(name, CodegenUnit::Function(func));
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
            let func = module.get_func(name).unwrap();

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
            name: fun_name,
            ty,
            attributes: _,
            body: _,
        } => {
            match ty {
                PrimitiveType::Function {
                    ret_ty: _,
                    params,
                    ext: false,
                } => {
                    for (pos, ty) in params.into_iter().enumerate() {
                        match ty {
                            PrimitiveType::NamedType { name, ty: _ } => {
                                module.push_in_scope(
                                    name,
                                    CodegenUnit::FunctionArgument(
                                        module
                                            .get_func(fun_name)
                                            .expect("Compiler borked. Functions borked")
                                            .get_param(pos as u32),
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
