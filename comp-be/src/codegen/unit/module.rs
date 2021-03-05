use std::{
    borrow::Borrow,
    cell::RefCell,
    collections::HashMap,
    path::PathBuf,
    rc::{Rc, Weak},
};

use llvm_sys_wrapper::{Builder, Context, LLVMValueRef, Module};
use parser::ast::{Expression, FunctionType, AST};

use crate::{
    codegen::{
        unit::{cg, decl, function::find_func},
        Scope, ValueTypePair,
    },
    ty::TypeContext,
};

use super::function::Func;

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum FuncType {
    Local,
    Export,
}

pub enum Message<'cmd> {
    RegisterFunction(&'cmd String, Func, FuncType),
    EnterFunctionScope((String, FunctionType)),
    ExitScope,
}

pub enum Query<'cmd> {
    Function((&'cmd String, &'cmd FunctionType)),
}

pub enum QueryResponse {
    Function(Option<Weak<Func>>),
}

pub struct ModuleContext<'ctx> {
    pub llvm_context: &'ctx Context,
    pub llvm_module: &'ctx Module,
    pub llvm_builder: &'ctx Builder,
    pub function_templates: HashMap<String, Expression>,
    dispatch: &'ctx dyn Fn(Message) -> (),
    query: &'ctx dyn Fn(Query) -> QueryResponse,
}

impl ModuleContext<'ctx> {
    pub fn dispatch(&self, command: Message) {
        (self.dispatch)(command)
    }

    pub fn query(&self, query: Query) -> QueryResponse {
        (self.query)(query)
    }

    pub fn in_function_scope<F>(&self, func: (String, FunctionType), f: F) -> Option<ValueTypePair>
    where
        F: FnOnce(ModuleContext) -> Option<ValueTypePair>,
    {
        self.dispatch(Message::EnterFunctionScope(func));
        let builder = Builder::new_in_context(self.llvm_context.as_ref());
        let ctx = self.with_builder(&builder);
        let res = f(ctx);
        self.dispatch(Message::ExitScope);
        res
    }
}

impl<'ctx> ModuleContext<'ctx> {
    pub fn new(
        llvm_context: &'ctx Context,
        llvm_module: &'ctx Module,
        llvm_builder: &'ctx Builder,
        dispatch: &'ctx dyn Fn(Message) -> (),
        query: &'ctx dyn Fn(Query) -> QueryResponse,
    ) -> Self {
        Self {
            llvm_context,
            llvm_module,
            llvm_builder,
            dispatch,
            query,
            function_templates: HashMap::new(),
        }
    }

    fn with_builder(&'ctx self, builder: &'ctx Builder) -> Self {
        Self {
            llvm_context: self.llvm_context,
            llvm_module: self.llvm_module,
            llvm_builder: builder,
            dispatch: self.dispatch,
            query: self.query,
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
    scope_stack: RefCell<Vec<Scope>>,
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
            scope_stack: RefCell::new(vec![]),
        }
    }

    pub fn in_root_scope<F>(&self, f: F)
    where
        F: FnOnce(&Self),
    {
        self.enter_root_scope();
        f(self);
        self.exit_scope();
    }

    fn enter_root_scope(&self) {
        self.scope_stack
            .borrow_mut()
            .push(Scope::with_fdir(self.path.clone()));
    }

    fn in_function_scope(&self, func: (String, FunctionType), builder: Builder) {
        self.enter_function_scope(func, builder);
        self.exit_scope();
    }

    fn enter_function_scope(&self, func: (String, FunctionType), builder: Builder) {
        self.scope_stack
            .borrow_mut()
            .push(Scope::with_function(func, builder));
    }

    fn exit_scope(&self) {
        self.scope_stack.borrow_mut().pop();
    }

    fn dispatch(&self, command: Message) {
        match command {
            Message::RegisterFunction(name, func, ty) => {
                let mut scope_stack = self.scope_stack.borrow_mut();
                if ty == FuncType::Local {
                    scope_stack.last_mut()
                } else {
                    scope_stack.first_mut()
                }
                .expect("ICE: Scope stack is corrupted.")
                .add_function(name, func);
            }
            Message::EnterFunctionScope(func) => {
                let builder = Builder::new_in_context(self.llvm_context.as_ref());
                self.in_function_scope(func, builder);
            }
            Message::ExitScope => self.exit_scope(),
        }
    }

    fn query(&self, query: Query) -> QueryResponse {
        match query {
            Query::Function(func) => QueryResponse::Function(self.get_func(func)),
        }
    }

    pub fn push(&mut self, name: &String, module: ModuleUnit<'ctx>) -> Option<ModuleUnit<'ctx>> {
        self.modules.insert(name.clone(), module)
    }

    pub fn get_mut(&mut self, name: &String) -> Option<&mut Self> {
        self.modules.get_mut(name)
    }

    pub fn decl(&'ctx self, root_builder: &Builder) {
        let ast = self.ast.clone();
        let dispatch = &|command: Message| self.dispatch(command);
        let query = &|query: Query| self.query(query);

        match ast {
            box AST::File(ref nodes) => nodes
                .iter()
                .fold(Some(()), |_, ast| {
                    Some(decl(
                        ast,
                        &mut ModuleContext::new(
                            self.llvm_context,
                            self.llvm_module,
                            root_builder,
                            dispatch,
                            query,
                        ),
                    ))
                })
                .unwrap(),
            _ => todo!("{:?}", self.ast),
        }
    }

    pub fn codegen(&'ctx self, root_builder: &Builder) -> Option<LLVMValueRef> {
        let ast = self.ast.clone();
        let dispatch = &|command: Message| self.dispatch(command);
        let query = &|query: Query| self.query(query);

        match ast {
            box AST::File(ref nodes) => nodes.iter().fold(None, |_, n| {
                cg(
                    n,
                    ModuleContext::new(
                        self.llvm_context,
                        self.llvm_module,
                        root_builder,
                        dispatch,
                        query,
                    ),
                )
            }),
            _ => todo!("{:?}", self.ast),
        }
    }

    pub fn get_func(&self, func: (&String, &FunctionType)) -> Option<Weak<Func>> {
        for scope in self.scope_stack.borrow().iter().rev() {
            if let Some(group) = scope.func_by_name(func.0) {
                return find_func(group, func.1);
            }
        }

        None
    }

    pub fn llvm_module_ref(&self) -> &Module {
        &self.llvm_module
    }
}
