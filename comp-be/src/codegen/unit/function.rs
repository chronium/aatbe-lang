use crate::{
  codegen::{AatbeModule, CodegenUnit},
  parser::ast::AST,
  parser::PrimitiveType,
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

      module.push_ref_in_scope(name, CodegenUnit::Function(func));
    }
    _ => unreachable!(),
  }
}

pub fn codegen_function(module: &mut AatbeModule, function: &AST) {
  match function {
    AST::Function {
      name,
      ty: _,
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
          .position_at_end(func.append_basic_block(String::default()));
      }
    }
    _ => unreachable!(),
  }
}

pub fn inject_function_in_scope(module: &mut AatbeModule, function: &AST) {
  match function {
    AST::Function {
      name,
      ty,
      attributes: _,
    } => {
      match ty {
        box PrimitiveType::FunctionType {
          ret_type: _,
          param,
          ext: false,
        } => {
          let func = module
            .get_func(name)
            .expect("Compiler borked. Functions borked");
          match param {
            box PrimitiveType::NamedType { name, ty: _ } => {
              let param = func.get_param(0);
              module.push_ref_in_scope(name, CodegenUnit::FunctionArgument(param));
            }
            box PrimitiveType::TupleType(types) if types.len() == 0 => {}
            _ => panic!("Unimplemented function arguments of type {:?}", param),
          };
        }
        _ => unreachable!(),
      };
    }
    _ => unreachable!(),
  }
}
