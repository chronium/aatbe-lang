use std::{cell::RefCell, collections::HashMap, path::PathBuf, rc::Weak};

use llvm_sys_wrapper::{Builder, Context, LLVMBasicBlock, LLVMBasicBlockRef, LLVMValueRef, Module};
use parser::ast::{Expression, FunctionType, AST};

use crate::{
    codegen::{
        unit::{cg, decl, function::find_func},
        Scope, ValueTypePair,
    },
    fmt::AatbeFmt,
    ty::TypeContext,
};

use super::{
    function::{Func, FuncTyMap},
    Slot,
};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum FunctionVisibility {
    Local,
    Public,
}

pub enum Message {
    PushInScope(String, Slot),
    DeclareFunction(Vec<String>, Func, FunctionVisibility),
    EnterFunctionScope((String, FunctionType)),
    EnterModuleScope(String),
    ExitModuleScope(String),
    RestoreModuleScope(String),
    EnterAnonymousScope,
    ExitScope,
}

impl std::fmt::Debug for Message {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self {
            Message::PushInScope(name, slot) => f
                .debug_tuple("PushInScope")
                .field(name)
                .field(slot)
                .finish(),
            Message::DeclareFunction(name, ty, vis) => f
                .debug_tuple("DeclareFunction")
                .field(name)
                .field(&AatbeFmt::fmt(ty))
                .field(vis)
                .finish(),
            Message::EnterFunctionScope(func) => f
                .debug_tuple("EnterFunctionScope")
                .field(&func.0)
                .field(&AatbeFmt::fmt(&func.1))
                .finish(),
            Message::EnterModuleScope(name) => {
                f.debug_tuple("EnterModuleScope").field(&name).finish()
            }
            Message::ExitModuleScope(name) => {
                f.debug_tuple("ExitModuleScope").field(&name).finish()
            }
            Message::RestoreModuleScope(name) => {
                f.debug_tuple("RestoreModuleScope").field(&name).finish()
            }
            Message::EnterAnonymousScope => write!(f, "EnterAnonymousScope"),
            Message::ExitScope => write!(f, "ExitScope"),
        }
    }
}

pub enum Query<'cmd> {
    Slot(&'cmd String),
    Function((Vec<String>, &'cmd FunctionType)),
    FunctionGroup(Vec<String>),
    Prefix,
}

impl std::fmt::Debug for Query<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Query::Slot(name) => f.debug_tuple("Slot").field(name).finish(),
            Query::Function(func) => f
                .debug_tuple("Function")
                .field(&func.0.join("::"))
                .field(&AatbeFmt::fmt(func.1))
                .finish(),
            Query::FunctionGroup(name) => f.debug_tuple("FunctionGroup").field(name).finish(),
            Query::Prefix => write!(f, "Prefix"),
        }
    }
}

pub enum QueryResponse {
    Slot(Option<Slot>),
    Function(Option<Weak<Func>>),
    FunctionGroup(Option<RefCell<FuncTyMap>>),
    Prefix(Vec<String>),
}

impl std::fmt::Debug for QueryResponse {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            QueryResponse::Slot(slot) => f.debug_tuple("Slot").field(slot).finish(),
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
            QueryResponse::Prefix(prefix) => f
                .debug_tuple("Prefix")
                .field(&format_args!("{}", prefix.join("::")))
                .finish(),
        }
    }
}

pub struct CompilerContext<'ctx> {
    pub path: PathBuf,
    pub llvm_context: &'ctx Context,
    pub llvm_module: &'ctx Module,
    pub llvm_builder: &'ctx Builder,
    pub function_templates: HashMap<String, Expression>,
    dispatch: &'ctx dyn Fn(Message) -> (),
    query: &'ctx dyn Fn(Query) -> QueryResponse,
    trace: &'ctx dyn Fn(String) -> (),
    basic_block: &'ctx dyn Fn(&str) -> LLVMBasicBlockRef,
}

impl<'ctx> CompilerContext<'ctx> {
    pub fn new<P>(
        path: P,
        llvm_context: &'ctx Context,
        llvm_module: &'ctx Module,
        llvm_builder: &'ctx Builder,
        dispatch: &'ctx dyn Fn(Message) -> (),
        query: &'ctx dyn Fn(Query) -> QueryResponse,
        trace: &'ctx dyn Fn(String) -> (),
        basic_block: &'ctx dyn Fn(&str) -> LLVMBasicBlockRef,
    ) -> Self
    where
        P: Into<PathBuf>,
    {
        Self {
            path: path.into(),
            llvm_context,
            llvm_module,
            llvm_builder,
            dispatch,
            query,
            trace,
            basic_block,
            function_templates: HashMap::new(),
        }
    }

    fn with_builder(&'ctx self, builder: &'ctx Builder) -> Self {
        Self {
            path: self.path.clone(),
            llvm_context: self.llvm_context,
            llvm_module: self.llvm_module,
            llvm_builder: builder,
            dispatch: self.dispatch,
            query: self.query,
            trace: self.trace,
            basic_block: self.basic_block,
            function_templates: HashMap::new(),
        }
    }

    pub fn basic_block(&self, name: &str) -> LLVMBasicBlockRef {
        (self.basic_block)(name)
    }

    pub fn trace(&self, message: String) {
        (self.trace)(message)
    }

    pub fn dispatch(&self, message: Message) {
        (self.dispatch)(message)
    }

    pub fn query(&self, query: Query) -> QueryResponse {
        (self.query)(query)
    }

    pub fn in_function_scope<F>(&self, func: (String, FunctionType), f: F) -> Option<ValueTypePair>
    where
        F: FnOnce(CompilerContext) -> Option<ValueTypePair>,
    {
        self.dispatch(Message::EnterFunctionScope(func));
        let builder = Builder::new_in_context(self.llvm_context.as_ref());
        let ctx = self.with_builder(&builder);
        let res = f(ctx);
        self.dispatch(Message::ExitScope);
        res
    }
}

pub struct CompilerUnit<'ctx> {
    modules: HashMap<String, CompilerUnit<'ctx>>,
    ast: Box<AST>,
    _typectx: TypeContext,
    llvm_context: &'ctx Context,
    llvm_module: &'ctx Module,
    scope_stack: RefCell<Vec<Scope>>,
    module_scopes: RefCell<HashMap<String, Scope>>,
    path: PathBuf,
    ident: RefCell<usize>,
}

impl<'ctx> CompilerUnit<'ctx> {
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
            _typectx: TypeContext::new(),
            scope_stack: RefCell::new(vec![]),
            module_scopes: RefCell::new(HashMap::new()),
            ident: RefCell::new(0),
        }
    }

    pub fn in_root_scope<F>(&self, f: F)
    where
        F: FnOnce(&Self),
    {
        self.enter_root_scope();
        f(self);
        println!("└── Exit Root Scope");
        self.exit_scope();
    }

    fn enter_root_scope(&self) {
        println!("Enter Root Scope");
        self.scope_stack
            .borrow_mut()
            .push(Scope::with_fdir(self.path.clone()));
    }

    fn enter_anonymous_scope(&self) {
        self.scope_stack.borrow_mut().push(Scope::new());
    }

    fn enter_function_scope(&self, func: (String, FunctionType), builder: Builder) {
        let mut prefix = self.get_prefix();
        prefix.push(func.0);

        self.scope_stack
            .borrow_mut()
            .push(Scope::with_function((prefix, func.1), builder));
    }

    fn enter_module_scope(&self, name: String) {
        self.scope_stack.borrow_mut().push(Scope::with_name(&name));
    }

    fn exit_module_scope(&self, name: String) {
        let scope = self
            .scope_stack
            .borrow_mut()
            .pop()
            .expect("ICE Scope stack broke");
        self.module_scopes.borrow_mut().insert(name, scope);
    }

    fn restore_module_scope(&self, name: String) {
        let scope = self
            .module_scopes
            .borrow_mut()
            .remove(&name)
            .expect("ICE Scope stack broke");
        self.scope_stack.borrow_mut().push(scope);
    }

    pub fn push_in_scope(&self, name: &String, unit: Slot) {
        self.scope_stack
            .borrow_mut()
            .last_mut()
            .expect("Compiler broke. Scope stack is corrupted.")
            .add_symbol(name, unit);
    }

    pub fn get_from_scope(&self, name: &String) -> Option<Slot> {
        for scope in self.scope_stack.borrow().iter().rev() {
            if let Some(sym) = scope.find_symbol(name) {
                return Some(sym.clone());
            }
        }

        None
    }

    pub fn get_slot(&self, name: &String) -> Option<Slot> {
        if let Some(slot @ (Slot::Variable { .. } | Slot::FunctionArgument(..))) =
            self.get_from_scope(name)
        {
            Some(slot)
        } else {
            None
        }
    }

    fn exit_scope(&self) {
        self.scope_stack
            .borrow_mut()
            .pop()
            .expect("ICE Scope Stack Broke");
    }

    fn dispatch(&self, message: Message) {
        print!("{}", "│   ".repeat(*self.ident.borrow()));
        match message {
            Message::PushInScope(ref name, ref unit) => {
                println!("├── {:?}", message);
                self.push_in_scope(name, unit.clone());
            }
            Message::DeclareFunction(ref name, ref func, ty) => {
                println!("├── {:?}", message);
                let mut scope_stack = self.scope_stack.borrow_mut();
                if ty == FunctionVisibility::Local {
                    scope_stack.last_mut()
                } else {
                    scope_stack.first_mut()
                }
                .expect("ICE: Scope stack is corrupted.")
                .add_function(&name, func.clone());
            }
            Message::EnterFunctionScope(ref func) => {
                println!("├── {:?}", message);
                *self.ident.borrow_mut() += 1;
                let builder = Builder::new_in_context(self.llvm_context.as_ref());
                self.enter_function_scope(func.clone(), builder);
            }
            Message::EnterModuleScope(ref name) => {
                println!("├── {:?}", message);
                *self.ident.borrow_mut() += 1;
                self.enter_module_scope(name.clone())
            }
            Message::ExitModuleScope(ref name) => {
                println!("└── {:?}", message);
                *self.ident.borrow_mut() -= 1;
                self.exit_module_scope(name.clone())
            }
            Message::RestoreModuleScope(ref name) => {
                println!("├── {:?}", message);
                *self.ident.borrow_mut() += 1;
                self.restore_module_scope(name.clone())
            }
            Message::EnterAnonymousScope => {
                println!("├── {:?}", message);
                *self.ident.borrow_mut() += 1;
                self.enter_anonymous_scope()
            }
            Message::ExitScope => {
                println!("└── {:?}", message);
                *self.ident.borrow_mut() -= 1;
                self.exit_scope()
            }
        }
    }

    fn query(&self, query: Query) -> QueryResponse {
        print!("{}", "│   ".repeat(*self.ident.borrow()));
        println!("├── Query {:?}", query);
        let response = match query {
            Query::Function(func) => QueryResponse::Function(self.get_func(func)),
            Query::FunctionGroup(name) => QueryResponse::FunctionGroup(self.get_func_group(&name)),
            Query::Prefix => QueryResponse::Prefix(self.get_prefix()),
            Query::Slot(name) => QueryResponse::Slot(self.get_slot(name)),
        };
        print!("{}", "│   ".repeat(*self.ident.borrow()));
        println!("├── Response {:?}", response);
        response
    }

    fn trace(&self, message: String) {
        print!("{}", "│   ".repeat(*self.ident.borrow()));
        println!("├── {}", message);
    }

    pub fn push(
        &mut self,
        name: &String,
        module: CompilerUnit<'ctx>,
    ) -> Option<CompilerUnit<'ctx>> {
        self.modules.insert(name.clone(), module)
    }

    pub fn get_mut(&mut self, name: &String) -> Option<&mut Self> {
        self.modules.get_mut(name)
    }

    pub fn decl(&'ctx self, root_builder: &Builder) {
        let ast = self.ast.clone();
        let dispatch = &|command: Message| self.dispatch(command);
        let query = &|query: Query| self.query(query);
        let trace = &|message: String| self.trace(message);
        let basic_block = &|name: &str| self.basic_block(name);

        decl::decl(
            &*ast,
            &mut CompilerContext::new(
                self.path.clone(),
                self.llvm_context,
                self.llvm_module,
                root_builder,
                dispatch,
                query,
                trace,
                basic_block,
            ),
        );
    }

    pub fn codegen(&'ctx self, root_builder: &Builder) -> Option<LLVMValueRef> {
        let ast = self.ast.clone();
        let dispatch = &|command: Message| self.dispatch(command);
        let query = &|query: Query| self.query(query);
        let trace = &|message: String| self.trace(message);
        let basic_block = &|name: &str| self.basic_block(name);

        cg::cg(
            &*ast,
            &CompilerContext::new(
                self.path.clone(),
                self.llvm_context,
                self.llvm_module,
                root_builder,
                dispatch,
                query,
                trace,
                basic_block,
            ),
        )
    }

    pub fn get_func_group(&self, name: &Vec<String>) -> Option<RefCell<FuncTyMap>> {
        for scope in self.scope_stack.borrow().iter().rev() {
            if let Some(func) = scope.func_by_name(&name) {
                return Some(func);
            }
        }

        None
    }

    pub fn get_func(&self, func: (Vec<String>, &FunctionType)) -> Option<Weak<Func>> {
        for scope in self.scope_stack.borrow().iter().rev() {
            if let Some(group) = scope.func_by_name(&func.0) {
                return find_func(group, func.1);
            }
        }

        None
    }

    pub fn llvm_module_ref(&self) -> &Module {
        &self.llvm_module
    }

    pub fn get_prefix(&self) -> Vec<String> {
        self.scope_stack
            .borrow()
            .iter()
            .map(|s| s.name())
            .filter(|n| n != &String::default())
            .collect::<Vec<_>>()
    }

    pub fn basic_block(&self, name: &str) -> LLVMBasicBlockRef {
        for scope in self.scope_stack.borrow().iter().rev() {
            let bb = scope.bb(self, &name);
            if let Some(bb) = bb {
                return bb;
            }
        }
        panic!("Compiler broke. Scope stack is corrupted.");
    }
}

#[macro_export]
macro_rules! prefix {
    (call $ctx: expr, $name: expr) => {{
        if let crate::codegen::unit::compiler::QueryResponse::Prefix(mut prefix) =
            $ctx.query(crate::codegen::unit::compiler::Query::Prefix)
        {
            prefix.pop();
            prefix.push($name);
            prefix
        } else {
            panic!("PREFIX ICE")
        }
    }};
    (call module $ctx: expr, $name: expr) => {{
        if let crate::codegen::unit::compiler::QueryResponse::Prefix(mut prefix) =
            $ctx.query(crate::codegen::unit::compiler::Query::Prefix)
        {
            prefix.pop();
            prefix.extend($name);
            prefix
        } else {
            panic!("PREFIX ICE")
        }
    }};
    ($ctx: expr, $name: expr) => {{
        if let crate::codegen::unit::compiler::QueryResponse::Prefix(mut prefix) =
            $ctx.query(crate::codegen::unit::compiler::Query::Prefix)
        {
            prefix.push($name);
            prefix
        } else {
            panic!("PREFIX ICE")
        }
    }};
    ($ctx: expr) => {{
        if let crate::codegen::unit::compiler::QueryResponse::Prefix(prefix) =
            $ctx.query(crate::codegen::unit::compiler::Query::Prefix)
        {
            prefix
        } else {
            panic!("PREFIX ICE")
        }
    }};
}
