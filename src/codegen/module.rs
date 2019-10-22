use llvm_sys_wrapper::{Context, LLVMValueRef, Module};

use crate::AST;

pub struct AatbeModule {
  llvm_context: Context,
  llvm_module: Module,
  name: String,
}

impl AatbeModule {
  pub fn new(name: String) -> Self {
    let llvm_context = Context::new();
    let llvm_module = llvm_context.create_module(name.as_ref());

    Self {
      llvm_context,
      llvm_module,
      name,
    }
  }

  pub fn decl_pass(&mut self, ast: &AST) {
    match ast {
      AST::Function {
        name,
        ty,
        attributes,
      } => {}
      _ => {}
    }
  }

  pub fn codegen_pass(&mut self, ast: &AST) -> Option<LLVMValueRef> {
    None
  }
}
