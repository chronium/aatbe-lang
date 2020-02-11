use comp_be::codegen::{AatbeModule, CompileError};
use parser::{lexer::Lexer, parser::Parser};

use clap::{clap_app, crate_authors, crate_description, crate_version};
use std::{
    ffi::CString,
    fs::{File, OpenOptions},
    io,
    io::{Read, Write},
    path::{Path, PathBuf},
};

use glob::glob;
use llvm_sys::support::LLVMLoadLibraryPermanently;
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
    (@arg bitcode: -c --bitcode "Emit LLVM Bitcode")
    (@arg lib: -l ... +takes_value "Link with library without prefix or extension"))
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

    if let Some(libs) = matches.values_of("lib") {
        for lib in libs {
            let globs = glob(format!("/usr/lib/x86_64-linux-gnu/lib{}.so", lib).as_ref())
                .unwrap()
                .filter_map(Result::ok)
                .collect::<Vec<PathBuf>>();
            if globs.len() < 1 {
                error!("lib{}.so cannot be found", lib);
            } else if globs.len() > 1 {
                error!("-l{} found too many matches, please be more specific", lib);
            } else {
                unsafe {
                    LLVMLoadLibraryPermanently(
                        CString::new(globs[0].to_str().unwrap())
                            .expect("cstring failed")
                            .as_ptr(),
                    );
                }
            }
        }
    }

    if module.errors().len() > 0 {
        warn!("Compilation failed. Errors were found");
        module.errors().iter().for_each(|err| match err {
            CompileError::Handled => {}
            CompileError::NotIndexable { ty, lval } => {
                error!("Cannot index `{}`. Type {} is not indexable", lval, ty)
            }
            CompileError::StoreMismatch {
                expected_ty,
                found_ty,
                value,
                lval,
            } => error!(
                "Cannot store {}: {} as {} is of type {}",
                found_ty, value, lval, expected_ty,
            ),
            CompileError::AssignMismatch {
                expected_ty,
                found_ty,
                value,
                var,
            } => error!(
                "Cannot assign {}: {} as {} is of type {}",
                found_ty, value, var, expected_ty
            ),
            CompileError::ExpectedType {
                found_ty,
                expected_ty,
                value,
            } => error!("Expected {} but found {}: {}", expected_ty, found_ty, value),
            CompileError::UnaryMismatch {
                op,
                found_ty,
                expected_ty,
                value,
            } => error!(
                "Cannot `{}{}` expected {} but found {}",
                op, value, expected_ty, found_ty
            ),
            CompileError::MismatchedArguments {
                function,
                expected_ty,
                found_ty,
            } => error!(
                "Mismatched arguments for `{}` expected `{} {}` but found `{} {}`",
                function, function, expected_ty, function, found_ty
            ),
            CompileError::BinaryMismatch { op, values, types } => error!(
                "Mismatched types at `{} {} {}`. Types found are {}, {}",
                values.0, op, values.1, types.0, types.1
            ),
            CompileError::OpMismatch { op, values, types } => error!(
                "Cannot `{} {} {}` for types {}, {}",
                values.0, op, values.1, types.0, types.1
            ),
        });
        std::process::exit(1);
    }

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
