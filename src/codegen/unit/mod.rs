use llvm_sys_wrapper::{Function, LLVMValueRef};

pub mod function;

pub use function::declare_function;

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
