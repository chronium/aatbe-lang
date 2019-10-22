use crate::{
  codegen::{AatbeModule, CodegenUnit},
  AST,
};

pub fn codegen_function(module: &mut AatbeModule, function: &AST) {
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
