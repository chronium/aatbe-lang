use comp_be::codegen::AatbeModule;
use parser::{lexer::Lexer, parser::Parser};

use clap::{clap_app, crate_authors, crate_description, crate_version};
use std::{
    fs::{File, OpenOptions},
    io,
    io::{Read, Write},
    path::Path,
};

use log::{error, warn};
use simplelog::*;

fn main() -> io::Result<()> {
    let matches = clap_app!(aatboot =>
    (version: crate_version!())
    (author: crate_authors!())
    (about: crate_description!())
    (@arg INPUT: +required "The file to compile")
    (@arg LLVM_OUT: --("emit-llvm") +takes_value "File to output LLVM IR")
    (@arg PARSE_OUT: --("emit-parsetree") +takes_value "File to output Parse Tree")
    (@arg LLVM_JIT: --("jit") -j "JIT the code")
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

    let mut lexer = Lexer::new(code.as_str());
    lexer.lex();

    let mut parser = Parser::new(lexer.tt());
    match parser.parse() {
        Ok(_) => {}
        Err(err) => panic!(format!("{:#?}", err)),
    };

    if let Some(parse_out) = matches.value_of("PARSE_OUT") {
        let mut file = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(Path::new(parse_out).with_extension("txt"))?;

        if let Err(e) = writeln!(file, "{:#?}", parser.pt()) {
            error!("Could not write to file: {}", e);
        }
    }

    let pt = parser.pt().as_ref().expect("Empty parse tree");

    let mut module = AatbeModule::new(
        input_path
            .file_stem()
            .and_then(|stem| stem.to_str())
            .map(|stem_str| stem_str.to_string())
            .expect("Could not get spice name from path"),
    );
    module.decl_pass(&pt);
    module.codegen_pass(&pt);

    if let Some(llvm_out) = matches.value_of("LLVM_OUT") {
        module
            .llvm_module_ref()
            .print_module_to_file(
                Path::new(llvm_out)
                    .with_extension("ll")
                    .to_str()
                    .expect("Could not create .ll file path"),
            )
            .expect("Could not write .ll file");
    }

    match module.llvm_module_ref().verify() {
        Ok(_) => {
            if matches.is_present("LLVM_JIT") {
                let interpreter = module.llvm_module_ref().create_jit_engine().unwrap();
                let named_function = module.llvm_module_ref().named_function("main");
                let mut params = [];
                let _run_result = interpreter.run_function(named_function.as_ref(), &mut params);
            }
            Ok(())
        }
        Err(err) => panic!(err),
    }
}
