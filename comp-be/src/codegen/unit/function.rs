use crate::{
    codegen::builder::cast,
    codegen::{
        builder::core,
        mangle_v1::NameMangler,
        unit::{FuncType, Message, Mutability, Query, QueryResponse, Slot},
        AatbeModule, CompileError, ValueTypePair,
    },
    fmt::AatbeFmt,
    ty::LLVMTyInCtx,
};
use llvm_sys_wrapper::Function;
use std::{
    borrow::Borrow,
    collections::HashMap,
    ops::Deref,
    rc::{Rc, Weak},
};

use parser::ast::{Expression, FunctionType, PrimitiveType};

use llvm_sys_wrapper::{Builder, LLVMBasicBlockRef};

use super::ModuleContext;

#[derive(Debug)]
pub struct Func {
    ty: FunctionType,
    name: String,
    inner: Function,
}

impl AatbeFmt for &Func {
    fn fmt(self) -> String {
        format!(
            "{}fn {} {}",
            if self.is_extern() {
                String::from("extern ")
            } else {
                String::default()
            },
            self.name,
            (&self.ty).fmt()
        )
    }
}

pub type FuncTyMap = Vec<Rc<Func>>;
pub type FunctionMap = HashMap<String, FuncTyMap>;

pub fn find_func<'a>(map: &'a FuncTyMap, ty: &FunctionType) -> Option<Weak<Func>> {
    for func in map {
        if &func.ty == ty {
            return Some(Rc::downgrade(func));
        }
    }
    return None;
}

pub fn find_function<'a>(map: &'a FuncTyMap, args: &Vec<PrimitiveType>) -> Option<&'a Func> {
    for func in map {
        if func.accepts(args) {
            return Some(func);
        }
    }
    return None;
}

impl Deref for Func {
    type Target = Function;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl Func {
    pub fn new(ty: FunctionType, name: String, inner: Function) -> Self {
        Self { ty, name, inner }
    }

    pub fn ty(&self) -> &FunctionType {
        &self.ty
    }

    pub fn ret_ty(&self) -> &PrimitiveType {
        &self.ty.ret_ty
    }

    pub fn is_extern(&self) -> bool {
        self.ty.ext
    }

    pub fn bb(&self, name: String) -> LLVMBasicBlockRef {
        self.inner.append_basic_block(name.as_ref())
    }

    fn accepts(&self, args: &Vec<PrimitiveType>) -> bool {
        let params = &self.ty.params;

        if params.len() != args.len() && !(params.contains(&PrimitiveType::Varargs)) {
            return false;
        };

        for (i, param) in params.iter().enumerate() {
            if matches!(param, PrimitiveType::Varargs) {
                continue;
            }
            let arg = &args[i];

            match param {
                PrimitiveType::Varargs => continue,
                PrimitiveType::NamedType {
                    ty: Some(box t), ..
                } if t != arg => return false,
                PrimitiveType::NamedType {
                    ty: Some(box t), ..
                } if t == arg => return true,
                t if t != arg => return false,
                _ => continue,
            }
        }

        return true;
    }
}

pub fn declare_function(ctx: &ModuleContext, function: &Expression) {
    match function {
        Expression::Function {
            ty,
            export,
            type_names,
            name,
            ..
        } if type_names.len() == 0 => {
            let func = ctx
                .llvm_module
                .get_or_add_function(&function.mangle(&ctx), ty.llvm_ty_in_ctx(&ctx));

            let func = Func::new(ty.clone(), name.clone(), func);
            if !export {
                ctx.dispatch(Message::RegisterFunction(&name, func, FuncType::Local));
            } else {
                ctx.dispatch(Message::RegisterFunction(&name, func, FuncType::Export));
            }
        }
        _ => unimplemented!("{:?}", function),
    }
}

pub fn declare_and_compile_function<'ctx>(
    ctx: ModuleContext,
    func: &Expression,
) -> Option<ValueTypePair> {
    //todo!("{:?}", func);
    match func {
        Expression::Function { ty, body, name, .. } => match ty {
            FunctionType {
                ret_ty: _,
                params: _,
                ext: true,
            } => None,
            _ => {
                ctx.in_function_scope((name.clone(), ty.clone()), |ctx| {
                    codegen_function(&ctx, func);

                    if !has_return_type(ty) {
                        core::ret_void(&ctx);
                    } else {
                        todo!();
                    }

                    None
                })
                /*
                inject_function_in_scope(module, func);
                let ret_val = module.codegen_expr(
                    &body
                        .as_ref()
                        .expect("ICE Function with no body but not external"),
                );

                // TODO: Typechecks
                if has_return_type(ty) {
                    if let Some(val) = ret_val {
                        match val.prim() {
                            PrimitiveType::VariantType(variant) => {
                                let parent_ty = module
                                    .typectx_ref()
                                    .get_parent_for_variant(variant)
                                    .expect("ICE: Variant without parent");
                                let ret_ty = *ty.ret_ty.clone();
                                core::ret(
                                    module,
                                    (cast::child_to_parent(module, val, parent_ty), ret_ty)
                                        .into(),
                                )
                            }
                            _ => core::ret(module, val),
                        };
                    } else {
                        module.add_error(CompileError::ExpectedReturn {
                            function: func.clone().fmt(),
                            ty: ty.fmt(),
                        })
                    }
                } else {
                    core::ret_void(module);
                }

                module.exit_scope();*/
            }
        },
        _ => unreachable!(),
    }
}

pub fn codegen_function(ctx: &ModuleContext, function: &Expression) {
    match function {
        Expression::Function {
            attributes,
            name,
            ty,
            ..
        } => {
            let func = ctx.query(Query::Function((name, ty)));

            if let QueryResponse::Function(Some(func)) = func {
                let func = func.upgrade().expect("ICE");

                if !attributes.is_empty() {
                    for attr in attributes {
                        match attr.to_lowercase().as_ref() {
                            "entry" => core::pos_at_end(ctx, func.bb("entry".to_string())),
                            _ => panic!("Cannot decorate function with {}", name),
                        };
                    }
                } else {
                    core::pos_at_end(ctx, func.bb(String::default()));
                }
            }
        }
        _ => unreachable!(),
    }
}

pub fn inject_function_in_scope(module: &mut AatbeModule, function: &Expression) {
    todo!()
    /*match function {
        Expression::Function {
            name: fname, ty, ..
        } => {
            match ty {
                fty
                @
                FunctionType {
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
                                    .get_func((fname.clone(), fty.clone()))
                                    .expect("Compiler borked. Functions borked")
                                    .get_param(pos as u32);
                                let ptr = core::alloca_with_name_ty(module, ty, name.as_ref());
                                core::store(module, arg, ptr);
                                module.push_in_scope(
                                    name,
                                    Slot::Variable {
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
                                    Slot::FunctionArgument(
                                        module
                                            .get_func((fname.clone(), fty.clone()))
                                            .expect("Compiler borked. Functions borked")
                                            .get_param(pos as u32),
                                        *ty.clone(),
                                    ),
                                );
                            }
                            PrimitiveType::Unit | PrimitiveType::Symbol(_) => {}
                            _ => unimplemented!("{:?}", ty),
                        }
                    }
                }
                _ => unreachable!(),
            };
        }
        _ => unreachable!(),
    }*/
}

fn has_return_type(func: &FunctionType) -> bool {
    match func.ret_ty {
        box PrimitiveType::Unit => false,
        _ => true,
    }
}
