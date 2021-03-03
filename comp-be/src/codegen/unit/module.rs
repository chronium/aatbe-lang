use std::collections::HashMap;

use llvm_sys_wrapper::LLVMValueRef;
use parser::ast::AST;

use crate::ty::TypeContext;

pub struct ModuleUnit {
    modules: HashMap<String, ModuleUnit>,
    ast: Box<AST>,
    typectx: TypeContext,
}

impl ModuleUnit {
    pub fn new(ast: Box<AST>) -> Self {
        Self {
            modules: HashMap::new(),
            ast,
            typectx: TypeContext::new(),
        }
    }

    pub fn push(&mut self, name: &String, module: ModuleUnit) -> Option<ModuleUnit> {
        self.modules.insert(name.clone(), module)
    }

    pub fn get_mut(&mut self, name: &String) -> Option<&mut Self> {
        self.modules.get_mut(name)
    }

    pub fn decl(&mut self) {
        match self.ast {
            box AST::File(ref nodes) => nodes
                .iter()
                .fold(Some(()), |_, ast| Some(decl(ast)))
                .unwrap(),
            _ => todo!("{:?}", self.ast),
        }
    }

    pub fn codegen(&mut self) -> Option<LLVMValueRef> {
        match self.ast {
            box AST::File(ref nodes) => nodes.iter().fold(None, |_, n| codegen(n)),
            _ => todo!("{:?}", self.ast),
        }
    }
}

pub fn decl(ast: &AST) {
    todo!()
}

pub fn codegen(ast: &AST) -> Option<LLVMValueRef> {
    todo!()
}
