use crate::{
    codegen::builder::cast,
    codegen::{
        builder::base,
        mangle_v1::NameMangler,
        unit::{
            cg::expr, CompilerContext, FunctionVisibility, Message, Mutability, Query,
            QueryResponse, Slot,
        },
        CompileError, ValueTypePair,
    },
    fmt::AatbeFmt,
    prefix,
    ty::LLVMTyInCtx,
};
use llvm_sys_wrapper::Function;
use std::{
    cell::RefCell,
    collections::HashMap,
    ops::Deref,
    rc::{Rc, Weak},
};

use guard::guard;

use parser::ast::{Expression, FunctionType, Type};

use llvm_sys_wrapper::{Builder, LLVMBasicBlockRef};

#[derive(Clone)]
pub struct Func {
    ty: FunctionType,
    ident: String,
    inner: Function,
}

impl std::fmt::Debug for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", AatbeFmt::fmt(self))
    }
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
            self.ident,
            (&self.ty).fmt()
        )
    }
}

pub type FuncTyMap = Vec<Rc<Func>>;
pub type FunctionMap = HashMap<String, RefCell<FuncTyMap>>;

pub fn find_func<'a>(map: RefCell<FuncTyMap>, ty: &FunctionType) -> Option<Weak<Func>> {
    for func in map.borrow().iter() {
        if &func.ty == ty {
            return Some(Rc::downgrade(func));
        }
    }
    return None;
}

pub fn find_function<'a>(map: RefCell<FuncTyMap>, args: &Vec<Type>) -> Option<Weak<Func>> {
    for func in map.borrow().iter() {
        if func.accepts(args) {
            return Some(Rc::downgrade(func));
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
    pub fn new(ty: FunctionType, ident: String, inner: Function) -> Self {
        Self { ty, ident, inner }
    }

    pub fn ty(&self) -> &FunctionType {
        &self.ty
    }

    pub fn ret_ty(&self) -> &Type {
        &self.ty.ret_ty
    }

    pub fn is_extern(&self) -> bool {
        self.ty.ext
    }

    pub fn bb(&self, name: String) -> LLVMBasicBlockRef {
        self.inner.append_basic_block(&name)
    }

    fn accepts(&self, args: &Vec<Type>) -> bool {
        let params = &self.ty.params;

        if params.len() != args.len() && !(params.contains(&Type::Varargs)) {
            return false;
        };

        for (i, param) in params.iter().enumerate() {
            if matches!(param, Type::Varargs) {
                continue;
            }
            let arg = &args[i];

            match param {
                Type::Varargs => continue,
                Type::NamedType {
                    ty: Some(box t), ..
                } if t != arg => return false,
                Type::NamedType {
                    ty: Some(box t), ..
                } if t == arg => return true,
                t if t != arg => return false,
                _ => continue,
            }
        }

        return true;
    }
}

pub fn declare_function(ctx: &CompilerContext, function: &Expression) {
    match function {
        Expression::Function {
            ty,
            public,
            type_names,
            name,
            ..
        } if type_names.len() == 0 => {
            let func = ctx
                .llvm_module
                .get_or_add_function(&function.mangle(&ctx), ty.llvm_ty_in_ctx(&ctx));

            let name = prefix!(ctx, name.clone()).join("::");
            let func = Func::new(ty.clone(), name.clone(), func);
            if !public {
                ctx.dispatch(Message::DeclareFunction(
                    name,
                    func,
                    FunctionVisibility::Local,
                ));
            } else {
                ctx.dispatch(Message::DeclareFunction(
                    name,
                    func,
                    FunctionVisibility::Public,
                ));
            }
        }
        _ => unimplemented!("{:?}", function),
    }
}

pub fn declare_and_compile_function<'ctx>(
    ctx: &CompilerContext,
    func: &Expression,
) -> Option<ValueTypePair> {
    match func {
        Expression::Function { ty, body, name, .. } => match ty {
            FunctionType {
                ret_ty: _,
                params: _,
                ext: true,
            } => None,
            _ => {
                ctx.in_function_scope((name.clone(), ty.clone()), |ctx| {
                    inject_function_in_scope(&ctx, func);

                    codegen_function(&ctx, func);

                    let ret_val = expr::cg(
                        &body
                            .as_ref()
                            .expect("ICE Function with no body but not external"),
                        &ctx,
                    );

                    if !has_return_type(ty) {
                        base::ret_void(&ctx);
                    } else {
                        if let Some(val) = ret_val {
                            match val.prim() {
                                Type::VariantType(_variant) => todo!(),
                                _ => base::ret(&ctx, val),
                            };
                        } else {
                            // TODO: Error
                            /*
                                module.add_error(CompileError::ExpectedReturn {
                                function: func.clone().fmt(),
                                ty: ty.fmt(),
                            }) */
                            todo!()
                        }
                    }

                    None
                })
            }
        },
        _ => unreachable!(),
    }
}

pub fn codegen_function(ctx: &CompilerContext, function: &Expression) {
    match function {
        Expression::Function {
            attributes,
            name,
            ty,
            ..
        } => {
            let func = ctx.query(Query::Function((prefix!(ctx).join("::"), ty)));

            guard!(let QueryResponse::Function(Some(func)) = func else { unreachable!(); });
            let func = func.upgrade().expect("ICE");

            if !attributes.is_empty() {
                for attr in attributes {
                    match attr.to_lowercase().as_ref() {
                        "entry" => base::pos_at_end(ctx, func.bb("entry".to_string())),
                        _ => panic!("Cannot decorate function with {}", name),
                    };
                }
            } else {
                base::pos_at_end(ctx, func.bb(String::default()));
            }
        }
        _ => unreachable!(),
    }
}

pub fn inject_function_in_scope(ctx: &CompilerContext, function: &Expression) {
    match function {
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
                            Type::Symbol(_) => false,
                            _ => true,
                        })
                        .enumerate()
                    {
                        match ty {
                            Type::NamedType {
                                name,
                                ty: Some(box Type::TypeRef(_) | box Type::Array { .. }),
                            } => {
                                /*let arg = module
                                    .get_func((fname.clone(), fty.clone()))
                                    .expect("Compiler borked. Functions borked")
                                    .get_param(pos as u32);
                                let ptr = core::alloca_with_name_ty(module, ty, name.as_ref());
                                core::store(module, arg, ptr);
                                module.push_in_scope(
                                    name,
                                    Slot::Variable {
                                        mutable: match ty {
                                            Type::Array { .. }
                                            | Type::NamedType {
                                                ty: Some(box Type::Array { .. }),
                                                ..
                                            } => Mutability::Mutable,
                                            _ => Mutability::Immutable,
                                        },
                                        name: name.clone(),
                                        ty: ty.clone(),
                                        value: ptr,
                                    },
                                );*/
                                todo!()
                            }
                            Type::NamedType {
                                name,
                                ty: Some(box Type::Ref(ty) | ty),
                            } => {
                                guard!(let QueryResponse::Function(Some(func)) =
                                    ctx.query(Query::Function((prefix!(ctx).join("::"), fty)))
                                else { unreachable!() });

                                let func = func.upgrade().expect("ICE");
                                ctx.dispatch(Message::PushInScope(
                                    name.clone(),
                                    Slot::FunctionArgument(func.get_param(pos as u32), *ty.clone()),
                                ))
                            }
                            Type::Unit | Type::Symbol(_) => {}
                            _ => unimplemented!("{:?}", ty),
                        }
                    }
                }
                _ => unreachable!(),
            };
        }
        _ => unreachable!(),
    }
}

fn has_return_type(func: &FunctionType) -> bool {
    match func.ret_ty {
        box Type::Unit => false,
        _ => true,
    }
}
