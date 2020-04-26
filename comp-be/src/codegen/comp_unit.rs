use io::Read;
use parser::{ast::AST, lexer::Lexer, parser::Parser};
use std::{fs::File, io, path::PathBuf};

pub struct CompilationUnit {
    path: PathBuf,
    ast: AST,
    name: String,
}

impl CompilationUnit {
    pub fn new(path: PathBuf) -> io::Result<Self> {
        let mut path = path;
        path.set_extension("aat");
        let mut file = File::open(path.clone())?;
        let mut code = String::new();

        file.read_to_string(&mut code)?;

        Ok(Self {
            name: path
                .file_stem()
                .and_then(|stem| stem.to_str())
                .map(|stem_str| stem_str.to_string())
                .expect("Could not get spice name from path"),
            path,
            ast: match Parser::new(Lexer::new(code.as_str()).lex()).parse() {
                Ok(ptree) => ptree,
                Err(err) => panic!(format!("{:#?}", err)),
            },
        })
    }

    pub fn ast(&self) -> &AST {
        &self.ast
    }

    pub fn path(&self) -> &PathBuf {
        &self.path
    }

    pub fn name(&self) -> &String {
        &self.name
    }
}
