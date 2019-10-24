#![feature(
  box_syntax,
  box_patterns,
  type_ascription,
  vec_remove_item,
  option_expect_none
)]

mod codegen;
mod parser;

use codegen::AatbeModule;
use parser::{aatbe_parser, ast::AST};

use std::{fs::File, io, io::prelude::Read};

fn main() -> io::Result<()> {
  let mut f = File::open("return-test.aat")?;
  let mut code = String::new();

  f.read_to_string(&mut code)?;

  let parsed = match aatbe_parser::file(code.as_str()) {
    Ok(ast) => ast,
    Err(err) => panic!(err),
  };

  println!("Parser output:\n------------\n{:#?}\n", parsed);

  let mut module = AatbeModule::new("main".to_string());
  module.decl_pass(&parsed);
  module.codegen_pass(&parsed);

  println!("LLVM output:\n------------");
  match module.llvm_module_ref().verify() {
    Ok(_) => {
      module.llvm_module_ref().dump();

      println!("\nExecution:\n------------");

      let interpreter = module.llvm_module_ref().create_jit_engine().unwrap();
      let named_function = module.llvm_module_ref().named_function("main");
      let mut params = [];
      let _run_result = interpreter.run_function(named_function.as_ref(), &mut params);
      Ok(())
    }
    Err(err) => panic!(err),
  }
}
