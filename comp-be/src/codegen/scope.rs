use std::collections::HashMap;

use crate::codegen::CodegenUnit;

#[derive(Debug)]
pub struct Scope {
    refs: HashMap<String, CodegenUnit>,
    name: String,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            refs: HashMap::new(),
            name: String::default(),
        }
    }
    pub fn new_with_name(name: &String) -> Self {
        Self {
            refs: HashMap::new(),
            name: name.clone(),
        }
    }

    pub fn find_symbol(&self, name: &String) -> Option<&CodegenUnit> {
        self.refs.get(name)
    }
    pub fn add_symbol(&mut self, name: &String, unit: CodegenUnit) {
        self.refs.insert(name.clone(), unit);
    }
}

/* TODO: Implement local dropping
 * impl Drop for Scope {
 *   fn drop(&mut self) {
 *   }
 * }
 */
