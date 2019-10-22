#![feature(box_syntax, box_patterns, type_ascription)]

mod codegen;
mod parser;

use parser::{aatbe_parser, ast::AST, operations::BinaryOp, primitive_type::PrimitiveType};

use std::{collections::HashMap, fs::File, io, io::prelude::Read};

use llvm_sys_wrapper::{Builder, Context, Function, Module, LLVM, *};

enum CodegenUnit {
  Function(Function),
}

struct Codegen {
  context: Context,
  module: Module,
  builder: Builder,
  refs: HashMap<String, CodegenUnit>,
}

impl Codegen {
  fn new(module_name: String) -> Self {
    let context = Context::new();
    let module = context.create_module(module_name.as_str());
    let builder = context.create_builder();

    Self {
      context,
      module,
      builder,
      refs: HashMap::new(),
    }
  }

  pub fn get_func_ref(&self, name: &String) -> LLVMValueRef {
    match self.refs.get(name).unwrap() {
      CodegenUnit::Function(func) => func.as_ref(),
    }
  }

  pub fn get_func(&self, name: &String) -> &Function {
    match self.refs.get(name).unwrap() {
      CodegenUnit::Function(func) => func,
    }
  }

  pub fn codegen(&mut self, node: &AST) -> LLVMValueRef {
    match node {
      AST::IntLiteral(ty, val) => self.codegen_const_int(ty, *val),
      AST::StringLiteral(string) => self.builder.build_global_string_ptr(string.as_str()),
      AST::Binary(op, x, y) => {
        let x = self.codegen(&x);
        let y = self.codegen(&y);

        self.codegen_binary_op(op, x, y)
      }
      AST::Function { name, ty } => {
        let func = self
          .module
          .get_or_add_function(name, ty.as_ref().llvm_type_in_context(&self.context));

        self.refs.insert(name.clone(), CodegenUnit::Function(func));

        self.get_func_ref(name)
      }
      AST::Call { name, arg } => {
        let arg_ref = self.codegen(arg);
        let mut args = [arg_ref];
        self.builder.build_call(self.get_func_ref(name), &mut args)
      }
      AST::Decorated { dec, expr } => match expr {
        box AST::Function { name, ty } => {
          let func = self.codegen(expr);

          match dec.to_lowercase().as_ref() {
            "entry" => {
              let f = self.get_func(name);
              self.builder.position_at_end(f.append_basic_block("entry"))
            }
            _ => panic!("Cannot decorate function with {}", name),
          };

          func
        }
        box AST::Assign(decl, expr) => match decl {
          box AST::Function { name, ty } => {
            let func = self.codegen(decl);

            match dec.to_lowercase().as_ref() {
              "entry" => {
                let f = self.get_func(name);
                self.builder.position_at_end(f.append_basic_block("entry"))
              }
              _ => panic!("Cannot decorate function with {}", name),
            };

            self.codegen(expr);

            match has_return_type(ty) {
              true => panic!("Functions that return not implemented"),
              false => self.builder.build_ret_void(),
            };

            func
          }
          _ => panic!("Cannot decorate and assign to {:?}", decl),
        },
        _ => panic!("Cannot decorate {:?}", expr),
      },
      AST::Block(nodes) | AST::File(nodes) => nodes
        .iter()
        .fold(None, |_, n| Some(self.codegen(n)))
        .unwrap(),
      _ => panic!("No codegen for {:?}", node),
    }
  }

  pub fn codegen_binary_op(&self, op: &BinaryOp, x: LLVMValueRef, y: LLVMValueRef) -> LLVMValueRef {
    match op {
      BinaryOp::Add => self.builder.build_add(x, y),
      BinaryOp::Subtract => self.builder.build_sub(x, y),
      BinaryOp::Multiply => self.builder.build_mul(x, y),
      BinaryOp::Divide => self.builder.build_sdiv(x, y),
      BinaryOp::Modulo => self.builder.build_srem(x, y),
      _ => panic!("Cannot binary op {:?}", op),
    }
  }

  pub fn codegen_const_int(&self, ty: &PrimitiveType, val: u64) -> LLVMValueRef {
    match ty {
      PrimitiveType::I8 => self.context.SInt8(val),
      PrimitiveType::I16 => self.context.SInt16(val),
      PrimitiveType::I32 => self.context.SInt32(val),
      PrimitiveType::I64 => self.context.SInt64(val),
      PrimitiveType::I128 => self.context.SInt128(val),
      PrimitiveType::U8 => self.context.UInt8(val),
      PrimitiveType::U16 => self.context.UInt16(val),
      PrimitiveType::U32 => self.context.UInt32(val),
      PrimitiveType::U64 => self.context.UInt64(val),
      PrimitiveType::U128 => self.context.UInt128(val),
      _ => panic!("Cannot const int {:?}", ty),
    }
  }
}

fn has_return_type(ty: &PrimitiveType) -> bool {
  match ty {
    PrimitiveType::FunctionType {
      ret_type,
      param,
      ext,
    } => match ret_type {
      box PrimitiveType::TupleType(t) if t.len() == 0 => false,
      _ => panic!("Return type {:?} not handled", ret_type),
    },
    _ => panic!("Not a function type"),
  }
}

fn main() -> io::Result<()> {
  let mut f = File::open("main.aat")?;
  let mut code = String::new();

  f.read_to_string(&mut code)?;

  let parsed = match aatbe_parser::file(code.as_str()) {
    Ok(ast) => ast,
    Err(err) => panic!(err),
  };

  LLVM::initialize();

  println!("{:#?}", parsed);

  let mut codegen = Codegen::new("test_module".to_owned());

  codegen.codegen(&parsed);

  match codegen.module.verify() {
    Ok(_) => {
      codegen.module.dump();

      let interpreter = codegen.module.create_jit_engine().unwrap();
      let named_function = codegen.module.named_function("main");
      let mut params = [];
      let _run_result = interpreter.run_function(named_function.as_ref(), &mut params);
      Ok(())
    }
    Err(err) => panic!(err),
  }
}
