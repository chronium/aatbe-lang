use llvm_sys_wrapper::{Builder, Context, LLVMValueRef, Module};
use std::collections::HashMap;

use crate::{
  codegen::{unit::codegen_function, CodegenUnit},
  AST,
};

pub struct AatbeModule {
  llvm_context: Context,
  llvm_module: Module,
  name: String,
  refs: HashMap<String, CodegenUnit>,
}

impl AatbeModule {
  pub fn new(name: String) -> Self {
    let llvm_context = Context::new();
    let llvm_module = llvm_context.create_module(name.as_ref());

    Self {
      llvm_context,
      llvm_module,
      name,
      refs: HashMap::new(),
    }
  }

  pub fn decl_pass(&mut self, ast: &AST) {
    match ast {
      AST::Function {
        name: _,
        ty: _,
        attributes: _,
      } => codegen_function(self, ast),
      AST::Assign(decl, _expr) => match decl {
        box AST::Function {
          name: _,
          ty: _,
          attributes: _,
        } => codegen_function(self, decl),
        _ => {}
      },
      AST::Block(nodes) | AST::File(nodes) => nodes
        .iter()
        .fold(None, |_, n| Some(self.decl_pass(n)))
        .unwrap(),
      _ => panic!("cannot decl {:?}", ast),
    }
  }

  pub fn codegen_pass(&mut self, ast: &AST) -> Option<LLVMValueRef> {
    None
  }

  pub fn add_ref(&mut self, name: &String, unit: CodegenUnit) {
    self.refs.insert(name.clone(), unit);
  }

  pub fn create_builder(&self) -> Builder {
    self.llvm_context.create_builder()
  }

  pub fn llvm_module_ref(&self) -> &Module {
    &self.llvm_module
  }

  pub fn llvm_context_ref(&self) -> &Context {
    &self.llvm_context
  }
}
