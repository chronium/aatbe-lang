use crate::{
  codegen::{unit::Mutability, AatbeModule, CodegenUnit},
  parser::ast::AST,
};

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

      module.add_ref(
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
