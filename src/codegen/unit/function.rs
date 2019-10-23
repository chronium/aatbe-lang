use crate::{
  codegen::{AatbeModule, CodegenUnit},
  AST,
};

pub fn declare_function(module: &mut AatbeModule, function: &AST) {
  match function {
    AST::Function {
      name,
      ty,
      attributes: _,
    } => {
      let func = module.llvm_module_ref().get_or_add_function(
        name,
        ty.as_ref().llvm_type_in_context(module.llvm_context_ref()),
      );

      module.add_ref(name, CodegenUnit::Function(func));
    }
    _ => unreachable!(),
  }
}

pub fn codegen_function(module: &mut AatbeModule, function: &AST) {
  match function {
    AST::Function {
      name,
      ty,
      attributes,
    } => {
      let func = module.get_func(name).unwrap();

      if !attributes.is_empty() {
        for attr in attributes {
          match attr.to_lowercase().as_ref() {
            "entry" => module
              .llvm_builder_ref()
              .position_at_end(func.append_basic_block("entry".to_string())),
            _ => panic!("Cannot decorate function with {}", name),
          };
        }
      } else {
        module
          .llvm_builder_ref()
          .position_at_end(func.append_basic_block("entry".to_string()));
      }
    }
    _ => unreachable!(),
  }
}
