use parser::ast::{IntType, PrimitiveType, UIntType};

use llvm_sys_wrapper::{Context, LLVMFunctionType, LLVMTypeRef};

pub trait LLVMTyInCtx {
  fn llvm_ty_in_ctx(&self, ctx: &Context) -> LLVMTypeRef;
}

impl LLVMTyInCtx for PrimitiveType {
  fn llvm_ty_in_ctx(&self, ctx: &Context) -> LLVMTypeRef {
    match self {
      PrimitiveType::Unit => ctx.VoidType(),
      PrimitiveType::Int(IntType::I8) | PrimitiveType::UInt(UIntType::U8) => ctx.Int8Type(),
      PrimitiveType::Int(IntType::I16) | PrimitiveType::UInt(UIntType::U16) => ctx.Int16Type(),
      PrimitiveType::Int(IntType::I32) | PrimitiveType::UInt(UIntType::U32) => ctx.Int32Type(),
      PrimitiveType::Int(IntType::I64) | PrimitiveType::UInt(UIntType::U64) => ctx.Int64Type(),
      PrimitiveType::Str => ctx.CharPointerType(),
      PrimitiveType::Function {
        ext: _,
        ret_ty,
        params,
      } => {
        let ret = ret_ty.llvm_ty_in_ctx(ctx);
        let mut varargs = false;
        let mut param_types = params
          .iter()
          .filter_map(|t| match t {
            PrimitiveType::Unit => None,
            PrimitiveType::Varargs => {
              varargs = true;
              None
            }
            _ => Some(t.llvm_ty_in_ctx(ctx)),
          })
          .collect::<Vec<LLVMTypeRef>>();

        unsafe {
          LLVMFunctionType(
            ret,
            param_types.as_mut_ptr(),
            param_types.len() as u32,
            varargs as i32,
          )
        }
      }
      _ => panic!("ICE: llvm_ty_in_ctx {:?}", self),
    }
  }
}
