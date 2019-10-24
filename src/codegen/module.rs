use llvm_sys_wrapper::{Builder, Context, LLVMValueRef, Module};
use std::collections::HashMap;

use crate::{
  codegen::{
    unit::{codegen_function, declare_function},
    CodegenUnit,
  },
  parser::primitive_type::PrimitiveType,
  AST,
};

pub struct AatbeModule {
  llvm_context: Context,
  llvm_module: Module,
  llvm_builder: Builder,
  name: String,
  refs: HashMap<String, CodegenUnit>,
}

impl AatbeModule {
  pub fn new(name: String) -> Self {
    let llvm_context = Context::new();
    let llvm_module = llvm_context.create_module(name.as_ref());
    let llvm_builder = llvm_context.create_builder();

    Self {
      llvm_context,
      llvm_module,
      llvm_builder,
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
      } => declare_function(self, ast),
      AST::Assign(decl, _expr) => match decl {
        box AST::Function {
          name: _,
          ty: _,
          attributes: _,
        } => declare_function(self, decl),
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
    match ast {
      AST::StringLiteral(string) => {
        Some(self.llvm_builder.build_global_string_ptr(string.as_str()))
      }
      AST::Tuple(vals) if vals.len() == 1 && vals[0] == AST::Empty => None,
      AST::Call { name, arg } => {
        let arg_ref = self.codegen_pass(arg);
        let mut args = Vec::new();
        if arg_ref.is_some() {
          args.push(arg_ref.unwrap());
        }
        match self.get_func(name) {
          Some(func) => Some(self.llvm_builder.build_call(func.into(), &mut args)),
          None => panic!("Cannot find function {:?}", name),
        }
      }
      AST::Function {
        name: _,
        ty: _,
        attributes: _,
      } => None,
      AST::Assign(decl, expr) => match decl {
        box AST::Function {
          name: _,
          ty,
          attributes: _,
        } => {
          codegen_function(self, decl);
          self.codegen_pass(expr);

          match has_return_type(ty) {
            true => panic!("Functions that return not implemented"),
            false => self.llvm_builder.build_ret_void(),
          };

          None
        }
        _ => panic!("Cannot assign to {:?}", decl),
      },
      AST::Block(nodes) | AST::File(nodes) => nodes
        .iter()
        .fold(None, |_, n| Some(self.codegen_pass(n)))
        .unwrap(),
      _ => panic!("cannot codegen {:?}", ast),
    }
  }

  pub fn add_ref(&mut self, name: &String, unit: CodegenUnit) {
    self.refs.insert(name.clone(), unit);
  }

  pub fn get_func(&self, name: &String) -> Option<&CodegenUnit> {
    self.refs.get(name)
  }

  pub fn llvm_builder_ref(&self) -> &Builder {
    &self.llvm_builder
  }

  pub fn llvm_module_ref(&self) -> &Module {
    &self.llvm_module
  }

  pub fn llvm_context_ref(&self) -> &Context {
    &self.llvm_context
  }
}

fn has_return_type(ty: &PrimitiveType) -> bool {
  match ty {
    PrimitiveType::FunctionType {
      ret_type,
      param: _,
      ext: _,
    } => match ret_type {
      box PrimitiveType::TupleType(t) if t.len() == 0 => false,
      _ => panic!("Return type {:?} not handled", ret_type),
    },
    _ => panic!("Not a function type"),
  }
}
