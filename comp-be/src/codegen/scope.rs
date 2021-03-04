use crate::codegen::{
    unit::{
        function::{find_func, Func, FuncTyMap, FunctionMap},
        Slot,
    },
    AatbeModule,
};
use std::{collections::HashMap, path::PathBuf, rc::Rc};

use llvm_sys_wrapper::{Builder, LLVMBasicBlockRef};
use parser::ast::FunctionType;

#[derive(Debug)]
pub struct Scope {
    refs: HashMap<String, Slot>,
    functions: FunctionMap,
    name: String,
    function: Option<(String, FunctionType)>,
    builder: Option<Rc<Builder>>,
    fdir: Option<PathBuf>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            refs: HashMap::new(),
            functions: HashMap::new(),
            name: String::default(),
            function: None,
            builder: None,
            fdir: None,
        }
    }
    pub fn with_name(name: &String) -> Self {
        Self {
            refs: HashMap::new(),
            functions: HashMap::new(),
            name: name.clone(),
            function: None,
            builder: None,
            fdir: None,
        }
    }
    pub fn with_builder(builder: Builder) -> Self {
        Self {
            refs: HashMap::new(),
            functions: HashMap::new(),
            name: String::default(),
            function: None,
            builder: Some(Rc::new(builder)),
            fdir: None,
        }
    }
    pub fn with_builder_and_fdir<P>(builder: Builder, fdir: P) -> Self
    where
        P: Into<PathBuf>,
    {
        Self {
            refs: HashMap::new(),
            functions: HashMap::new(),
            name: String::default(),
            function: None,
            builder: Some(Rc::new(builder)),
            fdir: Some(fdir.into()),
        }
    }
    pub fn with_function(func: (String, FunctionType), builder: Builder) -> Self {
        Self {
            refs: HashMap::new(),
            functions: HashMap::new(),
            name: func.0.clone(),
            function: Some(func),
            builder: Some(Rc::new(builder)),
            fdir: None,
        }
    }

    pub fn func_by_name(&self, name: &String) -> Option<&FuncTyMap> {
        self.functions.get(name)
    }

    pub fn add_function(&mut self, name: &String, func: Func) {
        if !self.functions.contains_key(name) {
            self.functions.insert(name.clone(), vec![]);
        }

        self.functions.get_mut(name).unwrap().push(func);
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
    pub fn builder(&self) -> Option<Rc<Builder>> {
        self.builder.clone()
    }
    pub fn fdir(&self) -> Option<PathBuf> {
        self.fdir.clone()
    }

    /*pub fn bb(&self, module: &AatbeModule, name: &String) -> Option<LLVMBasicBlockRef> {
        let func = self.function.as_ref()?;

        Some(
            find_func(module.get_func_group(&func.0)?, &func.1)
                .unwrap()
                .append_basic_block(name.as_ref()),
        )
    }*/
}

/* TODO: Implement local dropping
 * impl Drop for Scope {
 *   fn drop(&mut self) {
 *   }
 * }
 */
