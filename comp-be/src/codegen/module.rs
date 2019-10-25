use llvm_sys_wrapper::{Builder, Context, LLVMValueRef, Module, LLVM};
use std::{collections::HashMap, fs::File, io, io::prelude::Read};

use crate::{
  codegen::{
    unit::{codegen_function, declare_function},
    CodegenUnit,
  },
  parser::{aatbe_parser, ast::AST, operations::BinaryOp, primitive_type::PrimitiveType},
};

#[allow(dead_code)]
pub struct AatbeModule {
  llvm_context: Context,
  llvm_module: Module,
  llvm_builder: Builder,
  name: String,
  refs: HashMap<String, CodegenUnit>,
  imported: HashMap<String, AST>,
}

impl AatbeModule {
  pub fn new(name: String) -> Self {
    LLVM::initialize();

    let llvm_context = Context::new();
    let llvm_module = llvm_context.create_module(name.as_ref());
    let llvm_builder = llvm_context.create_builder();

    Self {
      llvm_context,
      llvm_module,
      llvm_builder,
      name,
      imported: HashMap::new(),
      refs: HashMap::new(),
    }
  }

  pub fn parse_import(&mut self, module: &String) -> io::Result<AST> {
    let mut f = File::open(format!("{}.aat", module))?;
    let mut code = String::new();

    f.read_to_string(&mut code)?;

    let parsed = match aatbe_parser::file(code.as_str()) {
      Ok(ast) => ast,
      Err(_) => panic!("Cannot find module {}", module),
    };

    Ok(parsed)
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
      AST::Import(module) => {
        let ast = self
          .parse_import(module)
          .expect("Something is completely broken");
        self.decl_pass(&ast);
        self
          .imported
          .insert(module.clone(), ast)
          .expect_none(format!("Module {} already imported", module).as_ref());
      }
      _ => panic!("cannot decl {:?}", ast),
    }
  }

  pub fn codegen_pass(&mut self, ast: &AST) -> Option<LLVMValueRef> {
    match ast {
      AST::StringLiteral(string) => {
        Some(self.llvm_builder.build_global_string_ptr(string.as_str()))
      }
      AST::IntLiteral(ty, val) => Some(self.codegen_const_int(ty, *val)),
      AST::Binary(op, lhs, rhs) => {
        let lh = self
          .codegen_pass(lhs)
          .expect("Binary op lhs must contain a value");
        let rh = self
          .codegen_pass(rhs)
          .expect("Binary op rhs must contain a value");
        Some(self.codegen_binary_op(op, lh, rh))
      }
      AST::Call { name, arg } => {
        let mut args = Vec::new();
        match arg {
          box AST::Tuple(vals) => {
            for arg in vals {
              let arg_ref = self.codegen_pass(arg);
              if arg_ref.is_some() {
                args.push(arg_ref.unwrap());
              }
            }
          }
          _ => args.push(self.codegen_pass(arg).unwrap()),
        };
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
          let ret = self.codegen_pass(expr);

          // TODO: Typechecks
          match has_return_type(ty) {
            true => self.llvm_builder.build_ret(
              ret.expect("Compiler broke, function returns broke. Everything's on fire"),
            ),
            false => self.llvm_builder.build_ret_void(),
          };

          None
        }
        _ => panic!("Cannot assign to {:?}", decl),
      },
      AST::Empty => None,
      AST::Block(nodes) | AST::File(nodes) => nodes
        .iter()
        .fold(None, |_, n| Some(self.codegen_pass(n)))
        .unwrap(),
      AST::Import(module) => {
        let ast = self
          .get_imported_ast(module)
          .expect("Cannot find already declared imported module? wat")
          .clone();
        self.codegen_pass(&ast);
        None
      }
      _ => panic!("cannot codegen {:?}", ast),
    }
  }

  pub fn codegen_binary_op(&self, op: &BinaryOp, x: LLVMValueRef, y: LLVMValueRef) -> LLVMValueRef {
    match op {
      BinaryOp::Add => self.llvm_builder.build_add(x, y),
      BinaryOp::Subtract => self.llvm_builder.build_sub(x, y),
      BinaryOp::Multiply => self.llvm_builder.build_mul(x, y),
      BinaryOp::Divide => self.llvm_builder.build_sdiv(x, y),
      BinaryOp::Modulo => self.llvm_builder.build_srem(x, y),
      _ => panic!("Cannot binary op {:?}", op),
    }
  }

  pub fn codegen_const_int(&self, ty: &PrimitiveType, val: u64) -> LLVMValueRef {
    match ty {
      PrimitiveType::I8 => self.llvm_context.SInt8(val),
      PrimitiveType::I16 => self.llvm_context.SInt16(val),
      PrimitiveType::I32 => self.llvm_context.SInt32(val),
      PrimitiveType::I64 => self.llvm_context.SInt64(val),
      PrimitiveType::I128 => self.llvm_context.SInt128(val),
      PrimitiveType::U8 => self.llvm_context.UInt8(val),
      PrimitiveType::U16 => self.llvm_context.UInt16(val),
      PrimitiveType::U32 => self.llvm_context.UInt32(val),
      PrimitiveType::U64 => self.llvm_context.UInt64(val),
      PrimitiveType::U128 => self.llvm_context.UInt128(val),
      _ => panic!("Cannot const int {:?}", ty),
    }
  }

  pub fn add_ref(&mut self, name: &String, unit: CodegenUnit) {
    self.refs.insert(name.clone(), unit);
  }

  pub fn get_func(&self, name: &String) -> Option<&CodegenUnit> {
    self.refs.get(name)
  }

  pub fn get_imported_ast(&self, name: &String) -> Option<&AST> {
    self.imported.get(name)
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
      _ => true,
    },
    _ => panic!("Not a function type"),
  }
}