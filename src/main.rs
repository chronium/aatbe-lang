#![feature(box_syntax)]

mod parser;
use parser::{aatbe_parser, ast::AST, operations::BinaryOp, primitive_type::PrimitiveType};

use std::{fs::File, io, io::prelude::Read};

use llvm_sys_wrapper::{Builder, Context, Module, LLVM, *};

struct Codegen {
  context: Context,
  module: Module,
  builder: Option<Builder>,
}

impl Codegen {
  fn new(module_name: String) -> Self {
    let context = Context::new();
    let module = context.create_module(module_name.as_str());

    Self {
      context,
      module,
      builder: None,
    }
  }

  pub fn prelude(&mut self) {
    self.builder = Some(self.context.create_builder());
    let main_type = fn_type!(self.context.VoidType());
    let main_func = self.module.get_or_add_function("main", main_type);
    let entry_block = main_func.append_basic_block("entry");
    self.builder.as_ref().unwrap().position_at_end(entry_block);
  }

  pub fn end(&self, val: LLVMValueRef) {
    self.builder.as_ref().unwrap().build_ret_void();
  }

  pub fn codegen(&self, node: &AST) -> LLVMValueRef {
    let builder: &Builder = self.builder.as_ref().unwrap();

    match node {
      AST::IntLiteral(ty, val) => self.codegen_const_int(ty, *val),
      AST::StringLiteral(string) => builder.build_global_string_ptr(string.as_str()),
      AST::Binary(op, x, y) => {
        let x = self.codegen(&x);
        let y = self.codegen(&y);

        self.codegen_binary_op(op, x, y)
      }
      AST::Function { name, ty } => self
        .module
        .get_or_add_function(name, self.prim_into_llvm_typeref(ty.as_ref()))
        .as_ref(),
      _ => panic!("No codegen for {:?}", node),
    }
  }

  pub fn codegen_binary_op(&self, op: &BinaryOp, x: LLVMValueRef, y: LLVMValueRef) -> LLVMValueRef {
    let builder: &Builder = self.builder.as_ref().unwrap();

    match op {
      BinaryOp::Add => builder.build_add(x, y),
      BinaryOp::Subtract => builder.build_sub(x, y),
      BinaryOp::Multiply => builder.build_mul(x, y),
      BinaryOp::Divide => builder.build_sdiv(x, y),
      BinaryOp::Modulo => builder.build_srem(x, y),
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

  fn prim_into_llvm_typeref(&self, ty: &PrimitiveType) -> LLVMTypeRef {
    match ty {
      PrimitiveType::I8 | PrimitiveType::U8 => self.context.Int8Type(),
      PrimitiveType::I16 | PrimitiveType::U16 => self.context.Int16Type(),
      PrimitiveType::I32 | PrimitiveType::U32 => self.context.Int32Type(),
      PrimitiveType::I64 | PrimitiveType::U64 => self.context.Int64Type(),
      PrimitiveType::I128 | PrimitiveType::U128 => self.context.Int128Type(),
      PrimitiveType::Str => self.context.CharPointerType(),
      PrimitiveType::FunctionType { ret_type, param } => fn_type!(
        self.prim_into_llvm_typeref(ret_type.as_ref()),
        self.prim_into_llvm_typeref(param.as_ref())
      ),
      _ => panic!(
        "PrimitiveType into LLVMTypeRef not implemented for {:?}",
        ty
      ),
    }
  }
}

fn main() -> io::Result<()> {
  let mut f = File::open("main.aat")?;
  let mut code = String::new();

  f.read_to_string(&mut code)?;

  let parsed = match aatbe_parser::expr(code.as_str()) {
    Ok(ast) => ast,
    Err(err) => panic!(err),
  };

  LLVM::initialize();

  println!("{:#?}", parsed);

  let mut codegen = Codegen::new("test_module".to_owned());

  codegen.prelude();
  let ret = codegen.codegen(&parsed);
  codegen.end(ret);

  match codegen.module.verify() {
    Ok(_) => {
      codegen.module.dump();
      Ok(())
    }
    Err(err) => panic!(err),
  }
}
