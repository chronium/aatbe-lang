use comp_be::codegen::{comp_unit::CompilationUnit, AatbeModule, CompileError};

use clap::{clap_app, crate_authors, crate_description, crate_version};
use flexi_logger::Logger;
use std::{
    env,
    ffi::CString,
    ffi::OsString,
    fs::OpenOptions,
    io,
    io::Write,
    path::{Path, PathBuf},
    process::Command,
};

use dotenv::dotenv;
use glob::glob;
use llvm_sys::support::LLVMLoadLibraryPermanently;
use llvm_sys_wrapper::{CodegenLevel, CPU, LLVM};
use log::{error, warn};

fn main() -> io::Result<()> {
    dotenv().ok();
    let matches = clap_app!(aatboot =>
    (version: crate_version!())
    (author: crate_authors!())
    (about: crate_description!())
    (@arg INPUT: +required "The file to compile")
    (@arg LLVM_OUT: --("emit-llvm") +takes_value "File to output LLVM IR")
    (@arg PARSE_OUT: --("emit-parsetree") +takes_value "File to output Parse Tree")
    (@arg OUT_FILE: -o +takes_value "File to output the compiled code")
    (@arg LLVM_JIT: --("jit") -j "JIT the code")
    (@arg STDLIB: --("stdlib") +takes_value "Set the Aatbe stdlib path")
    (@arg LIB: -l ... +takes_value "Link with library without prefix or extension"))
    .get_matches();

    Logger::with_env_or_str("trace").start().unwrap();

    let input_path = Path::new(matches.value_of_os("INPUT").unwrap()).to_path_buf();
    let main_cu = CompilationUnit::new(input_path.clone())?;

    if let Some(parse_out) = matches.value_of("PARSE_OUT") {
        let mut file = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(Path::new(parse_out).with_extension("txt"))?;

        if let Err(e) = writeln!(file, "{:#?}", main_cu.ast()) {
            error!("Could not write to file: {}", e);
        }
    }

    let stdlib: Option<PathBuf> = matches
        .value_of("STDLIB")
        .map(|stdlib| String::from(stdlib))
        .or_else(|| env::var("AATBE_STDLIB").ok())
        .map(|stdlib| Path::new(&stdlib).to_path_buf());

    let name = input_path
        .file_stem()
        .and_then(|stem| stem.to_str())
        .map(|stem_str| stem_str.to_string())
        .expect("Could not get spice name from path");
    let mut module = AatbeModule::new(name, main_cu, stdlib);
    module.compile();

    if let Some(libs) = matches.values_of("LIB") {
        for lib in libs {
            let globs = glob(&format!("/usr/lib/x86_64-linux-gnu/lib{}.so", lib))
                .unwrap()
                .filter_map(Result::ok)
                .collect::<Vec<_>>();
            if globs.len() < 1 {
                error!("lib{}.so cannot be found", lib);
            } else if globs.len() > 1 {
                error!("-l{} found too many matches, please be more specific", lib);
            } else {
                unsafe {
                    let cs = CString::new(globs[0].to_str().unwrap()).expect("cstring failed");
                    LLVMLoadLibraryPermanently(cs.as_ptr());
                }
            }
        }
    }

    if module.errors().len() > 0 {
        warn!("Compilation failed. Errors were found");
        module.errors().iter().for_each(|err| match err {
            CompileError::NoFunctionOverload {
                name,
                values,
                found,
            } => error!(
                "Could not find function overload for `{} {}`. Found overloads\n\t{}",
                name, values, found
            ),
            CompileError::FailedBinary { op, lhs, rhs } => {
                error!("Could not compile `{} {} {}`", lhs, op, rhs)
            }
            CompileError::NoGenericFunction { function } => {
                error!("No template found for {}", function)
            }
            CompileError::NoGenericRecord { rec } => error!("No template found for {}", rec),
            CompileError::ExpectedReturn { function, ty } => {
                error!("Expected return of type {} at `{}`", ty, function)
            }
            CompileError::Handled => {}
            CompileError::ArrayTypesNotUniform { values } => {
                error!("Array types are not all the same in `{}`", values)
            }
            CompileError::UnknownFunction { name, values } => {
                error!("Call to unknown function `{} {}`", name, values)
            }
            CompileError::ExpectedValue { name } => error!(
                "Cannot infer type for `{}` without specifying a value",
                name
            ),
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
            if matches.is_present("OUT_FILE") {
                let out_file = matches.value_of("OUT_FILE").unwrap();
                let path = Path::new(out_file);

                let mut tmp_out = env::temp_dir();
                tmp_out.push(path.with_extension("o").file_name().expect(""));

                LLVM::emit(
                    module.llvm_module_ref(),
                    CodegenLevel::O0,
                    String::from(tmp_out.as_path().to_string_lossy()),
                    CPU::X86_64,
                )
                .ok();

                Command::new("clang")
                    .arg(tmp_out.as_path())
                    .arg("-o")
                    .arg(path.with_extension("out"))
                    .args(
                        matches
                            .values_of("LIB")
                            .map(|v| {
                                v.map(|v| OsString::from(&format!("-l{}", v)))
                                    .collect::<Vec<_>>()
                            })
                            .unwrap_or(vec![OsString::from("")]),
                    )
                    .spawn()
                    .expect("could not find clang");
            }
            Ok(())
        }
        Err(err) => panic!("{}", err),
    }
}
