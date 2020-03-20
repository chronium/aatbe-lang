use crate::{
    codegen::{
        mangle_v1::NameMangler, unit::Mutability, AatbeModule, CodegenUnit, CompileError,
        ValueTypePair,
    },
    fmt::AatbeFmt,
    ty::LLVMTyInCtx,
};

use parser::ast::{Expression, PrimitiveType};

use llvm_sys_wrapper::Builder;

pub fn declare_function(module: &mut AatbeModule, function: &Expression) {
    match function {
        Expression::Function {
            name: _,
            ty,
            attributes: _,
            body: _,
            type_names,
        } if type_names.len() == 0 => {
            let name = function.mangle();

            let func = module
                .llvm_module_ref()
                .get_or_add_function(&name, ty.llvm_ty_in_ctx(module));

            module.push_in_scope(&name, CodegenUnit::Function(func, ty.clone()));
        }
        _ => panic!("Unimplemented declare_function {:?}", function),
    }
}

pub fn declare_and_compile_function(
    module: &mut AatbeModule,
    func: &Expression,
) -> Option<ValueTypePair> {
    match func {
        Expression::Function { ty, body, .. } => match ty {
            PrimitiveType::Function {
                ret_ty: _,
                params: _,
                ext: true,
            } => None,
            _ => {
                let builder = Builder::new_in_context(module.llvm_context_ref().as_ref());
                module.start_scope_with_function(&func.mangle(), builder);
                codegen_function(module, func);
                inject_function_in_scope(module, func);
                let ret = module.codegen_expr(
                    &body
                        .as_ref()
                        .expect("ICE Function with no body but not external"),
                );

                // TODO: Typechecks
                if has_return_type(ty) {
                    if let Some(ret) = ret {
                        module.llvm_builder_ref().build_ret(ret.0);
                    } else {
                        module.add_error(CompileError::ExpectedReturn {
                            function: func.clone().fmt(),
                            ty: ty.fmt(),
                        })
                    }
                } else {
                    module.llvm_builder_ref().build_ret_void();
                }

                module.exit_scope();

                None
            }
        },
        _ => unreachable!(),
    }
}

pub fn codegen_function(module: &mut AatbeModule, function: &Expression) {
    match function {
        Expression::Function {
            name: _,
            ty: _,
            attributes,
            body: _,
            type_names: _,
        } => {
            let name = &function.mangle();
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
            name: _,
            ty,
            attributes: _,
            body: _,
            type_names: _,
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
                            PrimitiveType::Symbol(_) => false,
                            _ => true,
                        })
                        .enumerate()
                    {
                        match ty {
                            PrimitiveType::NamedType {
                                name,
                                ty:
                                    Some(
                                        box PrimitiveType::TypeRef(_)
                                        | box PrimitiveType::Array { .. },
                                    ),
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
                                        mutable: match ty {
                                            PrimitiveType::Array { .. }
                                            | PrimitiveType::NamedType {
                                                ty: Some(box PrimitiveType::Array { .. }),
                                                ..
                                            } => Mutability::Mutable,
                                            _ => Mutability::Immutable,
                                        },
                                        name: name.clone(),
                                        ty: ty.clone(),
                                        value: ptr,
                                    },
                                );
                            }
                            PrimitiveType::NamedType {
                                name,
                                ty: Some(box PrimitiveType::Ref(ty) | ty),
                            } => {
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
                            PrimitiveType::Unit | PrimitiveType::Symbol(_) => {}
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

fn has_return_type(ty: &PrimitiveType) -> bool {
    match ty {
        PrimitiveType::Function {
            ret_ty,
            params: _,
            ext: _,
        } => match ret_ty {
            box PrimitiveType::Unit => false,
            _ => true,
        },
        _ => panic!("Not a function type {:?}", ty),
    }
}
