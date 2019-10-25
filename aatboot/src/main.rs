use comp_be::{codegen::AatbeModule, parser::aatbe_parser};

use std::{env, fs::File, io, io::prelude::Read};

fn main() -> io::Result<()> {
  if env::args().len() < 2 {
    panic!("Please provide a file to compile");
  }

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
