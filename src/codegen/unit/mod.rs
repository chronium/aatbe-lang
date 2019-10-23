use llvm_sys_wrapper::{Function, LLVMBasicBlockRef, LLVMValueRef};

pub mod function;

pub use function::{codegen_function, declare_function};

#[derive(Debug)]
pub enum CodegenUnit {
  Function(Function),
}

impl Into<LLVMValueRef> for &CodegenUnit {
  fn into(self) -> LLVMValueRef {
    match self {
      CodegenUnit::Function(func) => func.as_ref(),
    }
  }
}

impl CodegenUnit {
  fn append_basic_block(&self, name: String) -> LLVMBasicBlockRef {
    match self {
      CodegenUnit::Function(func) => func.append_basic_block(name.as_ref()),
      _ => panic!("Cannot append basic block on {:?}", self),
    }
  }
}
