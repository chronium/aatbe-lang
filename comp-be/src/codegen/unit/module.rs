use std::{
    collections::HashMap,
    path::PathBuf,
    rc::{Rc, Weak},
};

use llvm_sys_wrapper::{Builder, Context, LLVMValueRef, Module};
use parser::ast::{Expression, AST};

use crate::{
    codegen::{
        unit::{comp, decl},
        Scope,
    },
    ty::TypeContext,
};

use super::function::Func;

pub enum FuncType {
    Local,
    Export,
}

pub enum ModuleCommand<'cmd> {
    RegisterFunction(&'cmd String, Func, FuncType),
}

pub struct ModuleContext<'ctx> {
    pub llvm_context: &'ctx Context,
    pub llvm_module: &'ctx Module,
    pub llvm_builder: &'ctx Builder,
    pub function_templates: HashMap<String, Expression>,
    pub dispatch: &'ctx mut dyn FnMut(ModuleCommand) -> (),
}

impl<'ctx> ModuleContext<'ctx> {
    pub fn new(
        llvm_context: &'ctx Context,
        llvm_module: &'ctx Module,
        llvm_builder: &'ctx Builder,
        dispatch: &'ctx mut dyn FnMut(ModuleCommand) -> (),
    ) -> Self {
        Self {
            llvm_context,
            llvm_module,
            llvm_builder,
            dispatch,
            function_templates: HashMap::new(),
        }
    }
}

pub struct ModuleUnit<'ctx> {
    modules: HashMap<String, ModuleUnit<'ctx>>,
    ast: Box<AST>,
    typectx: TypeContext,
    llvm_context: &'ctx Context,
    llvm_module: &'ctx Module,
    scope_stack: Vec<Scope>,
    path: PathBuf,
}

impl<'ctx> ModuleUnit<'ctx> {
    pub fn new<P>(
        path: P,
        ast: Box<AST>,
        llvm_context: &'ctx Context,
        llvm_module: &'ctx Module,
    ) -> Self
    where
        P: Into<PathBuf>,
    {
        Self {
            path: path.into(),
            ast,
            llvm_context,
            llvm_module,
            modules: HashMap::new(),
            typectx: TypeContext::new(),
            scope_stack: vec![],
        }
    }

    pub fn in_root_scope<F>(&mut self, mut f: F)
    where
        F: FnMut(&mut Self),
    {
        self.enter_root_scope();
        f(self);
        self.exit_scope();
    }

    fn enter_root_scope(&mut self) {
        self.scope_stack.push(Scope::with_builder_and_fdir(
            Builder::new_in_context(self.llvm_context.as_ref()),
            self.path.clone(),
        ));
    }

    fn exit_scope(&mut self) {
        self.scope_stack.pop();
    }

    fn dispatch(&mut self, command: ModuleCommand) {}

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
        let llvm_builder = self.llvm_builder_ref();
        let dispatch = &mut |command: ModuleCommand| self.dispatch(command);
        let mut ctx = ModuleContext::new(llvm_context, llvm_module, &*llvm_builder, dispatch);
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
        let llvm_builder = self.llvm_builder_ref();
        let dispatch = &mut |command: ModuleCommand| self.dispatch(command);
        let mut ctx = ModuleContext::new(llvm_context, llvm_module, &*llvm_builder, dispatch);
        match ast {
            box AST::File(ref nodes) => nodes.iter().fold(None, |_, n| comp(n)),
            _ => todo!("{:?}", self.ast),
        }
    }

    pub fn llvm_builder_ref(&self) -> Rc<Builder> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(builder) = scope.builder() {
                return builder;
            }
        }
        unreachable!();
    }
}
