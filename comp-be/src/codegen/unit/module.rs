use std::{
    collections::HashMap,
    rc::{Rc, Weak},
};

use llvm_sys_wrapper::{Builder, Context, LLVMValueRef, Module};
use parser::ast::{Expression, AST};

use crate::{
    codegen::unit::{comp, decl},
    ty::TypeContext,
};

use super::function::Func;

pub enum FuncType {
    Local,
    Export,
}

pub struct ModuleContext<'ctx> {
    pub llvm_context: &'ctx Context,
    pub llvm_module: &'ctx Module,
    pub llvm_builder: &'ctx Builder,
    pub function_templates: HashMap<String, Expression>,
    pub register_function: &'ctx mut dyn FnMut(&String, Func, FuncType) -> (),
}

impl<'ctx> ModuleContext<'ctx> {
    pub fn new(
        llvm_context: &'ctx Context,
        llvm_module: &'ctx Module,
        llvm_builder: &'ctx Builder,
        register_function: &'ctx mut dyn FnMut(&String, Func, FuncType) -> (),
    ) -> Self {
        Self {
            llvm_context,
            llvm_module,
            llvm_builder,
            function_templates: HashMap::new(),
            register_function,
        }
    }
}

pub struct ModuleUnit<'ctx> {
    modules: HashMap<String, ModuleUnit<'ctx>>,
    ast: Box<AST>,
    typectx: TypeContext,
    llvm_context: &'ctx Context,
    llvm_module: &'ctx Module,
    llvm_builder: &'ctx Builder,
}

impl<'ctx> ModuleUnit<'ctx> {
    pub fn new(
        ast: Box<AST>,
        llvm_context: &'ctx Context,
        llvm_module: &'ctx Module,
        llvm_builder: &'ctx Builder,
    ) -> Self {
        Self {
            modules: HashMap::new(),
            ast,
            typectx: TypeContext::new(),
            llvm_context,
            llvm_module,
            llvm_builder,
        }
    }

    fn register_function(&mut self, name: &String, func: Func, func_type: FuncType) {}

    pub fn push(&mut self, name: &String, module: ModuleUnit<'ctx>) -> Option<ModuleUnit<'ctx>> {
        self.modules.insert(name.clone(), module)
    }

    pub fn get_mut(&mut self, name: &String) -> Option<&mut Self> {
        self.modules.get_mut(name)
    }

    pub fn decl(&mut self) {
        let ast = self.ast.clone();
        let llvm_context = self.llvm_context;
        let llvm_module = self.llvm_module;
        let llvm_builder = self.llvm_builder;
        let register_function = &mut |name: &String, func: Func, func_type: FuncType| {
            self.register_function(name, func, func_type)
        };
        let mut ctx =
            ModuleContext::new(llvm_context, llvm_module, llvm_builder, register_function);
        match ast {
            box AST::File(ref nodes) => nodes
                .iter()
                .fold(Some(()), |_, ast| Some(decl(ast, &mut ctx)))
                .unwrap(),
            _ => todo!("{:?}", self.ast),
        }
    }

    pub fn codegen(&mut self) -> Option<LLVMValueRef> {
        let ast = self.ast.clone();
        let llvm_context = self.llvm_context;
        let llvm_module = self.llvm_module;
        let llvm_builder = self.llvm_builder;
        let register_function = &mut |name: &String, func: Func, func_type: FuncType| {
            self.register_function(name, func, func_type)
        };
        let mut ctx =
            ModuleContext::new(llvm_context, llvm_module, llvm_builder, register_function);
        match ast {
            box AST::File(ref nodes) => nodes.iter().fold(None, |_, n| comp(n)),
            _ => todo!("{:?}", self.ast),
        }
    }
}
