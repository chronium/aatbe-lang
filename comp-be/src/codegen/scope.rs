use crate::codegen::unit::{
    function::{find_func, Func, FuncTyMap, FunctionMap},
    CompilerUnit, Slot,
};
use std::{cell::RefCell, collections::HashMap, path::PathBuf, rc::Rc};

use llvm_sys_wrapper::{Builder, LLVMBasicBlockRef};
use parser::ast::{Expression, FunctionType};

#[derive(Debug)]
pub struct Scope {
    refs: HashMap<String, Slot>,
    functions: FunctionMap,
    name: String,
    function: Option<(String, FunctionType)>,
    fdir: Option<PathBuf>,
    function_templates: HashMap<String, Expression>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            refs: HashMap::new(),
            functions: HashMap::new(),
            name: String::default(),
            function: None,
            fdir: None,
            function_templates: HashMap::new(),
        }
    }
    pub fn with_name(name: &String) -> Self {
        Self {
            refs: HashMap::new(),
            functions: HashMap::new(),
            name: name.clone(),
            function: None,
            fdir: None,
            function_templates: HashMap::new(),
        }
    }
    pub fn with_builder(builder: Builder) -> Self {
        Self {
            refs: HashMap::new(),
            functions: HashMap::new(),
            name: String::default(),
            function: None,
            fdir: None,
            function_templates: HashMap::new(),
        }
    }
    pub fn with_fdir<P>(fdir: P) -> Self
    where
        P: Into<PathBuf>,
    {
        Self {
            refs: HashMap::new(),
            functions: HashMap::new(),
            name: String::default(),
            function: None,
            fdir: Some(fdir.into()),
            function_templates: HashMap::new(),
        }
    }
    pub fn with_function(func: (String, FunctionType)) -> Self {
        Self {
            refs: HashMap::new(),
            functions: HashMap::new(),
            name: func.0.clone(),
            function: Some(func),
            fdir: None,
            function_templates: HashMap::new(),
        }
    }

    pub fn func_by_name(&self, name: &String) -> Option<RefCell<FuncTyMap>> {
        self.functions.get(name).cloned()
    }

    pub fn get_template(&self, name: &String) -> Option<Expression> {
        self.function_templates.get(name).map(|expr| expr.clone())
    }

    pub fn add_template(&mut self, name: String, func: Expression) {
        if self.function_templates.insert(name, func).is_some() {
            // TODO: Error
            panic!("ERROR");
        }
    }

    pub fn add_function(&mut self, name: &String, func: Func) {
        if !self.functions.contains_key(name) {
            self.functions.insert(name.clone(), RefCell::new(vec![]));
        }

        self.functions
            .get_mut(name)
            .unwrap()
            .get_mut()
            .push(Rc::new(func));
    }

    pub fn find_symbol(&self, name: &String) -> Option<&Slot> {
        self.refs.get(name)
    }
    pub fn add_symbol(&mut self, name: &String, unit: Slot) {
        self.refs.insert(name.clone(), unit);
    }
    pub fn function(&self) -> Option<(String, FunctionType)> {
        self.function.clone()
    }
    pub fn fdir(&self) -> Option<PathBuf> {
        self.fdir.clone()
    }

    pub fn name(&self) -> String {
        self.name.clone()
    }

    pub fn bb(&self, module: &CompilerUnit, name: &str) -> Option<LLVMBasicBlockRef> {
        let (_, ty) = self.function.as_ref()?;

        Some(
            find_func(module.get_func_group(&module.get_prefix().join("::"))?, &ty)
                .unwrap()
                .upgrade()
                .expect("ICE")
                .append_basic_block(name),
        )
    }
}

/* TODO: Implement local dropping
 * impl Drop for Scope {
 *   fn drop(&mut self) {
 *   }
 * }
 */
