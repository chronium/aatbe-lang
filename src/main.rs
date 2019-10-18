#![feature(box_syntax)]

mod parser;
use parser::{aatbe_parser, ast::AST, primitive_type::PrimitiveType};

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

  pub fn end(&self) {
    self.builder.as_ref().unwrap().build_ret_void();
  }

  pub fn codegen(&self, node: AST) {
    let builder: &Builder = self.builder.as_ref().unwrap();

    match node {
      AST::IntLiteral(ty, val) => self.codegen_const_int(ty, val),
      AST::StringLiteral(string) => builder.build_global_string_ptr(string.as_str()),
      _ => panic!("No codegen for {:?}", node),
    };
  }

  pub fn codegen_const_int(&self, ty: PrimitiveType, val: u64) -> LLVMValueRef {
    match ty {
      PrimitiveType::I32 => self.context.SInt32(val),
      _ => panic!("Cannot const int {:?}", ty),
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
  codegen.codegen(parsed);
  codegen.end();

  match codegen.module.verify() {
    Ok(_) => {
      codegen.module.dump();
      Ok(())
    }
    Err(err) => panic!(err),
  }
}
