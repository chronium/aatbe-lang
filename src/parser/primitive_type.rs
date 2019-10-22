use llvm_sys_wrapper::{fn_type, Context, LLVMFunctionType, LLVMTypeRef};

#[derive(Debug)]
pub enum PrimitiveType {
  U8,
  U16,
  U32,
  U64,
  U128,
  USize,
  I8,
  I16,
  I32,
  I64,
  I128,
  ISize,
  Boolean,
  Char,
  Str,
  Any,
  NamedType {
    name: String,
    ty: Box<PrimitiveType>,
  },
  TupleType(Vec<PrimitiveType>),
  Pointer(Box<PrimitiveType>),
  Params(Box<Option<PrimitiveType>>),
  FunctionType {
    ret_type: Box<PrimitiveType>,
    param: Box<PrimitiveType>,
    ext: bool,
  },
}

impl PrimitiveType {
  pub fn llvm_type_in_context(&self, ctx: &Context) -> LLVMTypeRef {
    match self {
      PrimitiveType::I8 | PrimitiveType::U8 => ctx.Int8Type(),
      PrimitiveType::I16 | PrimitiveType::U16 => ctx.Int16Type(),
      PrimitiveType::I32 | PrimitiveType::U32 => ctx.Int32Type(),
      PrimitiveType::I64 | PrimitiveType::U64 => ctx.Int64Type(),
      PrimitiveType::I128 | PrimitiveType::U128 => ctx.Int128Type(),
      PrimitiveType::Str => ctx.CharPointerType(),
      PrimitiveType::FunctionType {
        ret_type,
        param,
        ext: _,
      } => {
        let params = param.as_ref().llvm_type_in_context(ctx);
        if params == ctx.VoidType() {
          fn_type!(ret_type.as_ref().llvm_type_in_context(ctx))
        } else {
          fn_type!(ret_type.as_ref().llvm_type_in_context(ctx), params)
        }
      }
      PrimitiveType::TupleType(types) if types.len() == 0 => ctx.VoidType(),
      _ => panic!(
        "PrimitiveType into LLVMTypeRef not implemented for {:?}",
        self
      ),
    }
  }
}
