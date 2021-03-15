use std::{cell::RefCell, collections::HashMap, path::PathBuf, rc::Weak};

use llvm_sys_wrapper::{Builder, Context, LLVMValueRef, Module};
use parser::ast::{Expression, FunctionType, AST};

use crate::{
    codegen::{
        unit::{cg, decl, function::find_func},
        Scope, ValueTypePair,
    },
    fmt::AatbeFmt,
    ty::TypeContext,
};

use super::function::{Func, FuncTyMap};

use log::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum FunctionVisibility {
    Local,
    Export,
}

pub enum Message<'cmd> {
    RegisterFunction(&'cmd String, Func, FunctionVisibility),
    EnterFunctionScope((String, FunctionType)),
    EnterAnonymousScope,
    ExitScope,
}

impl std::fmt::Debug for Message<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Message::RegisterFunction(name, ty, vis) => f
                .debug_tuple("RegisterFunction")
                .field(name)
                .field(&AatbeFmt::fmt(ty))
                .field(vis)
                .finish(),
            Message::EnterFunctionScope(func) => f
                .debug_tuple("EnterFunctionScope")
                .field(&func.0)
                .field(&AatbeFmt::fmt(&func.1))
                .finish(),
            Message::EnterAnonymousScope => write!(f, "EnterAnonymousScope"),
            Message::ExitScope => write!(f, "ExitScope"),
        }
    }
}

pub enum Query<'cmd> {
    Function((&'cmd String, &'cmd FunctionType)),
    FunctionGroup(&'cmd String),
}

impl std::fmt::Debug for Query<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Query::Function(func) => f
                .debug_tuple("Function")
                .field(func.0)
                .field(&AatbeFmt::fmt(func.1))
                .finish(),
            Query::FunctionGroup(name) => f.debug_tuple("FunctionGroup").field(name).finish(),
        }
    }
}

pub enum QueryResponse {
    Function(Option<Weak<Func>>),
    FunctionGroup(Option<RefCell<FuncTyMap>>),
}

impl std::fmt::Debug for QueryResponse {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            QueryResponse::Function(ref func) => f
                .debug_tuple("Function")
                .field(
                    &func
                        .as_ref()
                        .map(|f| AatbeFmt::fmt(f.upgrade().expect("ICE").as_ref())),
                )
                .finish(),
            QueryResponse::FunctionGroup(group) => f
                .debug_tuple("FunctionGroup")
                .field(&format_args!(
                    "{}",
                    match group.as_ref() {
                        None => String::from("[]"),
                        Some(gr) => format!("{:?}", gr.borrow().iter().collect::<Vec<_>>()),
                    }
                ))
                .finish(),
        }
    }
}

pub struct ModuleContext<'ctx> {
    pub llvm_context: &'ctx Context,
    pub llvm_module: &'ctx Module,
    pub llvm_builder: &'ctx Builder,
    pub function_templates: HashMap<String, Expression>,
    dispatch: &'ctx dyn Fn(Message) -> (),
    query: &'ctx dyn Fn(Query) -> QueryResponse,
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

    pub fn dispatch(&self, message: Message) {
        (self.dispatch)(message)
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
        trace!("Enter root scope");
        self.scope_stack
            .borrow_mut()
            .push(Scope::with_fdir(self.path.clone()));
    }

    fn enter_function_scope(&self, func: (String, FunctionType), builder: Builder) {
        trace!("Enter function scope {:?}", func.0);
        self.scope_stack
            .borrow_mut()
            .push(Scope::with_function(func, builder));
    }

    fn exit_scope(&self) {
        trace!("Exit scope");
        self.scope_stack.borrow_mut().pop();
    }

    fn dispatch(&self, message: Message) {
        trace!("Dispatch {:?}", message);
        match message {
            Message::RegisterFunction(name, func, ty) => {
                let mut scope_stack = self.scope_stack.borrow_mut();
                if ty == FunctionVisibility::Local {
                    scope_stack.last_mut()
                } else {
                    scope_stack.first_mut()
                }
                .expect("ICE: Scope stack is corrupted.")
                .add_function(name, func);
            }
            Message::EnterFunctionScope(func) => {
                let builder = Builder::new_in_context(self.llvm_context.as_ref());
                self.enter_function_scope(func, builder);
            }
            Message::EnterAnonymousScope => {}
            Message::ExitScope => self.exit_scope(),
        }
    }

    fn query(&self, query: Query) -> QueryResponse {
        trace!("Query {:?}", query);
        let response = match query {
            Query::Function(func) => QueryResponse::Function(self.get_func(func)),
            Query::FunctionGroup(name) => QueryResponse::FunctionGroup(self.get_func_group(name)),
        };
        trace!("Response {:?}", response);
        response
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
                    &ModuleContext::new(
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

    pub fn get_func_group(&self, name: &String) -> Option<RefCell<FuncTyMap>> {
        for scope in self.scope_stack.borrow().iter().rev() {
            if let Some(func) = scope.func_by_name(name) {
                return Some(func);
            }
        }

        None
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
