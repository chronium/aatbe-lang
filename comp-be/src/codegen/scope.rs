use std::collections::HashMap;

use crate::codegen::CodegenUnit;

use llvm_sys_wrapper::Builder;

#[derive(Debug)]
pub struct Scope {
    refs: HashMap<String, CodegenUnit>,
    name: String,
    function: Option<String>,
    builder: Option<Builder>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            refs: HashMap::new(),
            name: String::default(),
            function: None,
            builder: None,
        }
    }
    pub fn with_name(name: &String) -> Self {
        Self {
            refs: HashMap::new(),
            name: name.clone(),
            function: None,
            builder: None,
        }
    }
    pub fn with_builder(builder: Builder) -> Self {
        Self {
            refs: HashMap::new(),
            name: String::default(),
            function: None,
            builder: Some(builder),
        }
    }
    pub fn with_function(name: &String, builder: Builder) -> Self {
        Self {
            refs: HashMap::new(),
            name: name.clone(),
            function: Some(name.clone()),
            builder: Some(builder),
        }
    }

    pub fn find_symbol(&self, name: &String) -> Option<&CodegenUnit> {
        self.refs.get(name)
    }
    pub fn add_symbol(&mut self, name: &String, unit: CodegenUnit) {
        self.refs.insert(name.clone(), unit);
    }
    pub fn function(&self) -> Option<String> {
        self.function.clone()
    }
    pub fn builder(&self) -> Option<&Builder> {
        self.builder.as_ref()
    }
}

/* TODO: Implement local dropping
 * impl Drop for Scope {
 *   fn drop(&mut self) {
 *   }
 * }
 */
