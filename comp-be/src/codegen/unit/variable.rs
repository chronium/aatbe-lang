use crate::{
  codegen::{unit::Mutability, AatbeModule, CodegenUnit},
  parser::ast::AST,
};

use llvm_sys_wrapper::LLVMValueRef;

pub fn alloc_variable(module: &mut AatbeModule, variable: &AST) {
  match variable {
    AST::Decl(_mut, ty, name, box expr) => {
      let var_ref = module.llvm_builder_ref().build_alloca_with_name(
        ty.llvm_type_in_context(module.llvm_context_ref()),
        name.as_ref(),
      );

      if let Some(e) = expr {
        let val = module
          .codegen_pass(e)
          .expect(format!("Cannot codegen variable {} value", name).as_ref());
        module.llvm_builder_ref().build_store(val, var_ref);
      }

      module.push_ref_in_scope(
        name,
        CodegenUnit::Variable {
          mutable: Mutability::from(_mut),
          name: name.clone(),
          ty: ty.clone(),
          value: var_ref,
        },
      );
    }
    _ => unreachable!(),
  }
}

pub fn store_value(module: &mut AatbeModule, name: &String, value: &AST) -> LLVMValueRef {
  let var_ref = module.get_var(name);
  let var = match var_ref {
    None => panic!("Cannot find variable {}", name),
    Some(var) => var.into(),
  };
  // TODO: Check mutability

  let val = module
    .codegen_pass(value)
    .expect(format!("Cannot codegen assignment for {} value", name).as_ref());

  module.llvm_builder_ref().build_store(val, var)
}
