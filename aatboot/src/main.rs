use comp_be::{codegen::AatbeModule, parser::aatbe_parser};

use clap::{clap_app, crate_authors, crate_description, crate_version};
use std::{
  fs::{File, OpenOptions},
  io,
  io::{Read, Write},
  path::Path,
};

use log::{debug, error, info, warn};
use simplelog::*;

fn main() -> io::Result<()> {
  let matches = clap_app!(aatboot => 
    (version: crate_version!())
    (author: crate_authors!())
    (about: crate_description!())
    (@arg INPUT: +required "The file to compile")
    (@arg LLVM_OUT: --("emit-llvm") +takes_value "File to output LLVM IR")
    (@arg PARSE_OUT: --("emit-parsetree") +takes_value "File to output Parse Tree")
    (@arg bitcode: -c --bitcode "Emit LLVM Bitcode"))
  .get_matches();

  if let Err(_) = TermLogger::init(LevelFilter::Warn, Config::default(), TerminalMode::Mixed) {
    SimpleLogger::init(LevelFilter::Warn, Config::default())
      .expect("No logger should be already set")
  }

  if matches.is_present("bitcode") {
    warn!("LLVM Bitcode output not yet implemented")
  }

  let input_path = Path::new(matches.value_of_os("INPUT").unwrap());

  let mut f = File::open(input_path.clone())?;
  let mut code = String::new();

  f.read_to_string(&mut code)?;

  let parsed = match aatbe_parser::file(code.as_str()) {
    Ok(ast) => ast,
    Err(err) => panic!(err),
  };

  if let Some(parse_out) = matches.value_of("PARSE_OUT") {
    let mut file = OpenOptions::new()
      .read(true)
      .write(true)
      .create(true)
      .open(Path::new(parse_out).with_extension("txt"))?;

    if let Err(e) = writeln!(file, "{:#?}", parsed) {
      error!("Could not write to file: {}", e);
    }
  }

  let mut module = AatbeModule::new(
    input_path
      .file_stem()
      .and_then(|stem| stem.to_str())
      .map(|stem_str| stem_str.to_string())
      .expect("Could not get spice name from path"),
  );
  module.decl_pass(&parsed);
  module.codegen_pass(&parsed);

  if let Some(llvm_out) = matches.value_of("LLVM_OUT") {
    module
      .llvm_module_ref()
      .print_module_to_file(
        Path::new(llvm_out)
          .with_extension("lli")
          .to_str()
          .expect("Could not create .lli file path"),
      )
      .expect("Could not write .lli file");
  }

  match module.llvm_module_ref().verify() {
    Ok(_) => {
      let interpreter = module.llvm_module_ref().create_jit_engine().unwrap();
      let named_function = module.llvm_module_ref().named_function("main");
      let mut params = [];
      let _run_result = interpreter.run_function(named_function.as_ref(), &mut params);
      Ok(())
    }
    Err(err) => panic!(err),
  }
}
