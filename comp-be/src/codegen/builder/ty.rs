use crate::{codegen::unit::ModuleContext, ty::LLVMTyInCtx};
use llvm_sys_wrapper::LLVMTypeRef;

pub fn pointer(ctx: &ModuleContext, ty: &dyn LLVMTyInCtx) -> LLVMTypeRef {
    ctx.llvm_context.PointerType(ty.llvm_ty_in_ctx(ctx))
}

pub fn slice_type(ctx: &ModuleContext, ty: &dyn LLVMTyInCtx) -> LLVMTypeRef {
    ctx.llvm_context
        .StructType(&mut vec![slice_ptr(ctx, ty), i32(ctx)], false)
        .as_ref()
}

pub fn pointer_to(ctx: &ModuleContext, ty: LLVMTypeRef) -> LLVMTypeRef {
    ctx.llvm_context.PointerType(ty)
}

pub fn array(ctx: &ModuleContext, ty: &dyn LLVMTyInCtx, count: u32) -> LLVMTypeRef {
    ctx.llvm_context.ArrayType(ty.llvm_ty_in_ctx(ctx), count)
}

pub fn slice(ctx: &ModuleContext, ty: &dyn LLVMTyInCtx) -> LLVMTypeRef {
    ctx.llvm_context.ArrayType(ty.llvm_ty_in_ctx(ctx), 0)
}

pub fn slice_ptr(ctx: &ModuleContext, ty: &dyn LLVMTyInCtx) -> LLVMTypeRef {
    pointer_to(ctx, slice(ctx, ty))
}

pub fn i8(ctx: &ModuleContext) -> LLVMTypeRef {
    ctx.llvm_context.Int8Type()
}

pub fn i16(ctx: &ModuleContext) -> LLVMTypeRef {
    ctx.llvm_context.Int16Type()
}

pub fn i32(ctx: &ModuleContext) -> LLVMTypeRef {
    ctx.llvm_context.Int32Type()
}

pub fn i64(ctx: &ModuleContext) -> LLVMTypeRef {
    ctx.llvm_context.Int64Type()
}
