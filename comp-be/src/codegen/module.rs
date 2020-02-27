use llvm_sys_wrapper::{Builder, Context, LLVMValueRef, Module, Phi, LLVM};
use std::{collections::HashMap, fs::File, io, io::prelude::Read};

use crate::{
    codegen::{
        codegen_binary,
        expr::const_expr::fold_constant,
        mangle_v1::NameMangler,
        unit::{
            alloc_variable, codegen_function, declare_function, init_record,
            inject_function_in_scope, store_value,
        },
        CodegenUnit, CompileError, Scope,
    },
    fmt::AatbeFmt,
    ty::{
        record::{store_named_field, Record},
        LLVMTyInCtx, TypeContext, TypeKind,
    },
};

use parser::{
    ast::{AtomKind, Expression, IntSize, LoopType, PrimitiveType, AST},
    lexer::Lexer,
    parser::Parser,
};

pub struct ValueTypePair(LLVMValueRef, TypeKind);

impl From<(LLVMValueRef, TypeKind)> for ValueTypePair {
    fn from((val, ty): (LLVMValueRef, TypeKind)) -> ValueTypePair {
        ValueTypePair(val, ty)
    }
}

impl From<ValueTypePair> for (LLVMValueRef, TypeKind) {
    fn from(vtp: ValueTypePair) -> (LLVMValueRef, TypeKind) {
        (vtp.0, vtp.1)
    }
}

impl ValueTypePair {
    pub fn prim(&self) -> &PrimitiveType {
        match self {
            ValueTypePair(
                _,
                TypeKind::Primitive(PrimitiveType::NamedType {
                    name: _,
                    ty: Some(ty),
                }),
            ) => ty,
            ValueTypePair(_, TypeKind::Primitive(prim)) => prim,
            _ => panic!("ICE prim {:?}"),
        }
    }

    pub fn ty(&self) -> TypeKind {
        TypeKind::Primitive(self.prim().clone())
    }

    pub fn val(&self) -> LLVMValueRef {
        self.0
    }

    pub fn indexable(&self) -> Option<ValueTypePair> {
        match &self {
            ValueTypePair(val, TypeKind::Primitive(prim)) => match prim {
                prim @ PrimitiveType::Str | prim @ PrimitiveType::Array { ty: _, len: _ } => {
                    Some((*val, TypeKind::Primitive(prim.clone())).into())
                }
                PrimitiveType::Pointer(box ty) => {
                    Some((*val, TypeKind::Primitive(ty.clone())).into())
                }
                _ => None,
            },
            _ => None,
        }
    }
}

#[allow(dead_code)]
pub struct AatbeModule {
    llvm_context: Context,
    llvm_module: Module,
    llvm_builder: Builder,
    name: String,
    scope_stack: Vec<Scope>,
    imported: HashMap<String, AST>,
    imported_cg: Vec<String>,
    current_function: Option<String>,
    typectx: TypeContext,
    compile_errors: Vec<CompileError>,
}

impl AatbeModule {
    pub fn new(name: String) -> Self {
        LLVM::initialize();

        let llvm_context = Context::new();
        let llvm_module = llvm_context.create_module(name.as_ref());
        let llvm_builder = llvm_context.create_builder();

        Self {
            llvm_context,
            llvm_module,
            llvm_builder,
            name,
            imported: HashMap::new(),
            imported_cg: Vec::new(),
            scope_stack: Vec::new(),
            current_function: None,
            typectx: TypeContext::new(),
            compile_errors: Vec::new(),
        }
    }

    pub fn parse_import(&mut self, module: &String) -> io::Result<AST> {
        let mut f = File::open(format!("{}.aat", module))?;
        let mut code = String::new();

        f.read_to_string(&mut code)?;

        let mut lexer = Lexer::new(code.as_str());
        lexer.lex();

        let mut parser = Parser::new(lexer.tt(), format!("{}.aat", module));
        match parser.parse() {
            Ok(_) => {}
            Err(err) => panic!(format!("{:#?}", err)),
        };

        let pt = parser
            .pt()
            .as_ref()
            .expect(format!("Empty parse tree in {}", module).as_str());

        Ok(pt.clone())
    }

    pub fn decl_expr(&mut self, expr: &Expression) {
        match expr {
            Expression::Function {
                name: _,
                ty: _,
                attributes: _,
                body: _,
            } => declare_function(self, expr),
            _ => panic!("Top level {:?} unsupported", expr),
        }
    }

    pub fn decl_pass(&mut self, ast: &AST) {
        self.start_scope();
        match ast {
            AST::Constant { ty: _, value: _ } => {}
            AST::Record(name, types) => {
                let rec = Record::new(self, name, types);
                self.typectx.push_type(name, TypeKind::RecordType(rec));
                self.typectx.get_record(name).unwrap().set_body(self, types);
            }
            AST::File(nodes) => nodes
                .iter()
                .fold(None, |_, n| Some(self.decl_pass(n)))
                .unwrap(),
            AST::Expr(expr) => self.decl_expr(expr),
            AST::Import(module) => {
                if !self.imported.contains_key(module) {
                    let ast = self
                        .parse_import(module)
                        .expect("Something is completely broken");
                    self.decl_pass(&ast);
                    self.imported
                        .insert(module.clone(), ast)
                        .expect_none(format!("Module {} already imported", module).as_ref());
                }
            }
            _ => panic!("cannot decl {:?}", ast),
        }
    }

    pub fn get_interior_pointer(&self, parts: Vec<String>) -> ValueTypePair {
        if let ([rec_ref], tail) = parts.split_at(1) {
            match self.get_var(&rec_ref) {
                None => panic!("Could not find record {}", rec_ref),
                Some(rec) => {
                    let rec_type = match rec {
                        CodegenUnit::Variable {
                            mutable: _,
                            name: _,
                            ty: PrimitiveType::TypeRef(rec),
                            value: _,
                        } => rec.clone(),
                        CodegenUnit::Variable {
                            mutable: _,
                            name: _,
                            ty:
                                PrimitiveType::NamedType {
                                    name: _,
                                    ty: Some(box PrimitiveType::TypeRef(rec)),
                                },
                            value: _,
                        } => rec.clone(),
                        CodegenUnit::FunctionArgument(_arg, PrimitiveType::TypeRef(rec)) => {
                            rec.clone()
                        }
                        CodegenUnit::FunctionArgument(
                            _arg,
                            PrimitiveType::Pointer(box PrimitiveType::TypeRef(rec)),
                        ) => rec.clone(),
                        _ => panic!("ICE get_interior_pointer {:?}", rec),
                    };

                    self.typectx_ref()
                        .get_record(&rec_type)
                        .expect("ICE get_record variable without type")
                        .read_field(self, rec.into(), rec_ref, tail.to_vec())
                }
            }
        } else {
            unreachable!()
        }
    }

    pub fn has_ret(&self, expr: &Expression) -> bool {
        match expr {
            Expression::Ret(_) => true,
            Expression::Block(vals) if vals.len() > 0 => {
                vals.iter().fold(false, |_, n| self.has_ret(n))
            }
            _ => false,
        }
    }

    #[allow(unreachable_code)]
    pub fn codegen_expr(&mut self, expr: &Expression) -> Option<ValueTypePair> {
        match expr {
            Expression::Ret(box _value) => {
                let val = self.codegen_expr(_value).unwrap();

                let func_ret = self
                    .get_func(
                        self.current_function
                            .as_ref()
                            .expect("Compiler borked rets"),
                    )
                    .expect("Must be in a function for ret statement")
                    .ret_ty()
                    .clone();

                if val.prim() != &func_ret {
                    self.add_error(CompileError::ExpectedType {
                        expected_ty: func_ret.fmt(),
                        found_ty: val.prim().fmt(),
                        value: _value.fmt(),
                    });
                    None
                } else {
                    Some(
                        (
                            self.llvm_builder_ref().build_ret(val.val()),
                            TypeKind::Primitive(func_ret),
                        )
                            .into(),
                    )
                }
            }
            Expression::RecordInit { record, values } => {
                let rec = self.llvm_builder_ref().build_alloca(
                    self.typectx_ref()
                        .get_type(record)
                        .expect(format!("ICE could not find record {}", record).as_str())
                        .llvm_ty_in_ctx(self),
                );
                values.iter().for_each(|val| match val {
                    AtomKind::NamedValue { name, val } => {
                        let val_ref = self
                            .codegen_expr(val)
                            .expect(format!("ICE could not codegen {:?}", val).as_str());
                        let val_ty = val_ref.prim().fmt();
                        match store_named_field(
                            self,
                            rec,
                            record,
                            self.typectx_ref()
                                .get_record(record)
                                .expect(format!("ICE could not find record {}", record).as_str()),
                            name,
                            val_ref,
                        ) {
                            Ok(_) => {}
                            Err(expected) => self.add_error(CompileError::StoreMismatch {
                                expected_ty: expected.fmt(),
                                found_ty: val_ty,
                                value: val.fmt(),
                                lval: record.clone(),
                            }),
                        };
                    }
                    _ => panic!("ICE codegen_expr {:?}", expr),
                });

                Some(
                    (
                        self.llvm_builder_ref().build_load(rec),
                        TypeKind::Primitive(PrimitiveType::TypeRef(record.clone())),
                    )
                        .into(),
                )
            }
            Expression::Assign { lval, value } => match value {
                box Expression::RecordInit { record, values } => init_record(
                    self,
                    lval,
                    &Expression::RecordInit {
                        record: record.clone(),
                        values: values.to_vec(),
                    },
                ),
                _ => store_value(self, lval, value),
            },
            Expression::Decl {
                ty: PrimitiveType::NamedType { name, ty: Some(ty) },
                value: _,
                exterior_bind: _,
            } => {
                alloc_variable(self, expr);

                Some(
                    (
                        self.get_var(name).expect("Compiler crapped out.").into(),
                        TypeKind::Primitive(*ty.clone()),
                    )
                        .into(),
                )
            }
            Expression::Decl {
                ty: PrimitiveType::NamedType { name, ty: None },
                value,
                exterior_bind: _,
            } => {
                if value.is_none() {
                    self.add_error(CompileError::ExpectedValue { name: name.clone() });
                    return None;
                }

                alloc_variable(self, expr).and_then(|ty| {
                    Some(
                        (
                            self.get_var(name).expect("Compiler crapped out.").into(),
                            TypeKind::Primitive(ty),
                        )
                            .into(),
                    )
                })
            }
            Expression::Loop {
                loop_type,
                cond_expr,
                body,
            } => {
                let func = self
                    .get_func(self.current_function.as_ref().expect("Compiler borked ifs"))
                    .expect("Must be in a function for if statement");

                let cond_bb = func.append_basic_block(String::default());
                let body_bb = func.append_basic_block(String::default());
                let end_bb = func.append_basic_block(String::default());

                self.llvm_builder_ref().build_br(cond_bb);
                self.llvm_builder_ref().position_at_end(cond_bb);
                let cond = match self.codegen_expr(cond_expr) {
                    Some(cond) => cond,
                    None => return None,
                };

                if *cond.prim().inner() != PrimitiveType::Bool {
                    self.add_error(CompileError::ExpectedType {
                        expected_ty: PrimitiveType::Bool.fmt(),
                        found_ty: cond.prim().fmt(),
                        value: cond_expr.fmt(),
                    });
                };

                match loop_type {
                    LoopType::While => {
                        self.llvm_builder_ref()
                            .build_cond_br(cond.val(), body_bb, end_bb)
                    }
                    LoopType::Until => {
                        self.llvm_builder_ref()
                            .build_cond_br(cond.val(), end_bb, body_bb)
                    }
                };

                self.llvm_builder_ref().position_at_end(body_bb);
                self.codegen_expr(body);

                self.llvm_builder_ref().build_br(cond_bb);
                self.llvm_builder_ref().position_at_end(end_bb);

                None
            }
            Expression::If {
                cond_expr,
                then_expr,
                else_expr,
            } => {
                let func = self
                    .get_func(self.current_function.as_ref().expect("Compiler borked ifs"))
                    .expect("Must be in a function for if statement");

                let then_bb = func.append_basic_block(String::default());
                let else_bb = if let Some(_) = else_expr {
                    Some(func.append_basic_block(String::default()))
                } else {
                    None
                };
                let end_bb = func.append_basic_block(String::default());

                let cond = match self.codegen_expr(cond_expr) {
                    Some(cond) => cond,
                    None => return None,
                };

                if *cond.prim().inner() != PrimitiveType::Bool {
                    self.add_error(CompileError::ExpectedType {
                        expected_ty: PrimitiveType::Bool.fmt(),
                        found_ty: cond.prim().fmt(),
                        value: cond_expr.fmt(),
                    });
                }

                self.llvm_builder
                    .build_cond_br(cond.val(), then_bb, else_bb.unwrap_or(end_bb));

                self.llvm_builder.position_at_end(then_bb);

                let then_val = self.codegen_expr(then_expr);
                let else_val = if let Some(bb) = else_bb {
                    self.llvm_builder.build_br(end_bb);
                    self.llvm_builder.position_at_end(bb);

                    if let Some(else_expr) = else_expr {
                        self.codegen_expr(&*else_expr)
                    } else {
                        unreachable!()
                    }
                } else {
                    None
                };
                if !self.has_ret(then_expr) {
                    self.llvm_builder.build_br(end_bb);
                }
                self.llvm_builder.position_at_end(end_bb);

                if let Some(then_val) = then_val {
                    let ty = then_val.prim().clone();
                    if ty != PrimitiveType::Unit {
                        if else_val.is_some() {
                            let phi = Phi::new(
                                self.llvm_builder_ref().as_ref(),
                                ty.llvm_ty_in_ctx(self),
                                "",
                            );

                            phi.add_incoming(then_val.val(), then_bb);

                            if let Some(else_val) = else_val {
                                if &ty != else_val.prim() {
                                    self.add_error(CompileError::ExpectedType {
                                        expected_ty: ty.clone().fmt(),
                                        found_ty: else_val.prim().fmt(),
                                        value: else_expr.as_ref().unwrap().fmt(),
                                    });
                                }
                                phi.add_incoming(else_val.val(), else_bb.unwrap());
                            }

                            Some((phi.as_ref(), TypeKind::Primitive(ty)).into())
                        } else {
                            Some((then_val.val(), TypeKind::Primitive(ty)).into())
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            Expression::Call {
                name: raw_name,
                args,
            } => {
                let mut call_types = vec![];

                let mut call_args = args
                    .iter()
                    .filter_map(|arg| match arg {
                        Expression::Atom(AtomKind::SymbolLiteral(sym)) => {
                            call_types.push(PrimitiveType::TypeRef(sym.clone()));
                            None
                        }
                        _ => self.codegen_expr(arg).map_or(None, |arg| {
                            call_types.push(arg.prim().clone());
                            Some(arg.val())
                        }),
                    })
                    .collect::<Vec<LLVMValueRef>>();

                let name = if !self.is_extern(raw_name) && call_types.len() > 0 {
                    format!(
                        "{}A{}",
                        raw_name,
                        call_types
                            .iter()
                            .map(|arg| arg.mangle())
                            .collect::<Vec<String>>()
                            .join(".")
                    )
                } else {
                    raw_name.clone()
                };

                let mut mismatch = false;

                let params = match self.get_params(&name) {
                    None => {
                        self.add_error(CompileError::UnknownFunction {
                            name: raw_name.clone(),
                            values: call_types
                                .iter()
                                .zip(args)
                                .map(|(ty, val)| format!("{}: {}", val.mangle(), ty.fmt()))
                                .collect::<Vec<String>>()
                                .join(", "),
                        });
                        return None;
                    }
                    Some(params) => params,
                };

                for (i, fty) in params.iter().enumerate() {
                    if fty == &PrimitiveType::Varargs {
                        break;
                    }

                    if &call_types[i] != fty {
                        mismatch = true;
                    }
                }

                if mismatch {
                    self.add_error(CompileError::MismatchedArguments {
                        function: raw_name.clone(),
                        expected_ty: params
                            .iter()
                            .map(|p| p.fmt())
                            .collect::<Vec<String>>()
                            .join(", "),
                        found_ty: call_types
                            .iter()
                            .map(|arg| arg.fmt())
                            .collect::<Vec<String>>()
                            .join(", "),
                    });
                }

                let func = self.get_func(&name).unwrap();

                Some(
                    (
                        self.llvm_builder.build_call(func.into(), &mut call_args),
                        TypeKind::Primitive(func.ret_ty()),
                    )
                        .into(),
                )
            }
            Expression::Binary(lhs, op, rhs) => match codegen_binary(self, op, lhs, rhs) {
                Ok(val) => Some(val),
                Err(err) => {
                    self.add_error(err);
                    None
                }
            },
            Expression::Function {
                name: _,
                ty,
                body,
                attributes: _,
            } => {
                match ty {
                    PrimitiveType::Function {
                        ret_ty: _,
                        params: _,
                        ext: true,
                    } => None,
                    _ => {
                        let name = codegen_function(self, expr);
                        self.current_function = Some(name.clone());
                        self.start_scope_with_name(&name);
                        inject_function_in_scope(self, expr);
                        let ret = self.codegen_expr(
                            &body
                                .as_ref()
                                .expect("ICE Function with no body but not external"),
                        );

                        // TODO: Typechecks
                        match has_return_type(ty) {
                            true => self.llvm_builder.build_ret(
                                ret.expect(
                                    "Compiler broke, function returns broke. Everything's on fire",
                                )
                                .0,
                            ),
                            false => self.llvm_builder.build_ret_void(),
                        };

                        self.exit_scope();

                        None
                    }
                }
            }
            Expression::Block(nodes) if nodes.len() == 0 => None,
            Expression::Block(nodes) => {
                self.start_scope();

                let ret = nodes
                    .iter()
                    .fold(None, |_, n| Some(self.codegen_expr(n)))
                    .unwrap();

                self.exit_scope();

                ret
            }
            Expression::Atom(atom) => self.codegen_atom(atom),
            _ => panic!(format!("ICE: codegen_expr {:?}", expr)),
        }
    }

    pub fn codegen_pass(&mut self, ast: &AST) -> Option<LLVMValueRef> {
        match ast {
            AST::Constant {
                ty: PrimitiveType::NamedType { name, ty: _ },
                value: _,
            } => {
                fold_constant(self, ast).map(|constant| self.push_in_scope(name, constant));
                None
            }
            AST::File(nodes) => nodes
                .iter()
                .fold(None, |_, n| Some(self.codegen_pass(n)))
                .unwrap(),
            AST::Expr(expr) => self.codegen_expr(expr).map(|e| e.val()),
            AST::Import(module) => {
                if !self.imported_cg.contains(module) {
                    let ast = self
                        .get_imported_ast(module)
                        .expect("Cannot find already declared imported module? wat")
                        .clone();
                    self.codegen_pass(&ast);
                    self.imported_cg.push(module.clone());
                }
                None
            }
            AST::Record(_, _) => None,
            _ => panic!("cannot codegen {:?}", ast),
        }
    }

    pub fn codegen_const_int(&self, ty: &PrimitiveType, val: u64) -> LLVMValueRef {
        match ty {
            PrimitiveType::Int(IntSize::Bits8) => self.llvm_context.SInt8(val),
            PrimitiveType::Int(IntSize::Bits16) => self.llvm_context.SInt16(val),
            PrimitiveType::Int(IntSize::Bits32) => self.llvm_context.SInt32(val),
            PrimitiveType::Int(IntSize::Bits64) => self.llvm_context.SInt64(val),
            PrimitiveType::UInt(IntSize::Bits8) => self.llvm_context.UInt8(val),
            PrimitiveType::UInt(IntSize::Bits16) => self.llvm_context.UInt16(val),
            PrimitiveType::UInt(IntSize::Bits32) => self.llvm_context.UInt32(val),
            PrimitiveType::UInt(IntSize::Bits64) => self.llvm_context.UInt64(val),
            _ => panic!("Cannot const int {:?}", ty),
        }
    }

    pub fn start_scope(&mut self) {
        self.scope_stack.push(Scope::new());
    }

    pub fn start_scope_with_name(&mut self, name: &String) {
        self.scope_stack.push(Scope::new_with_name(name));
    }

    pub fn exit_scope(&mut self) {
        self.scope_stack.pop();
    }

    pub fn get_in_scope(&self, name: &String) -> Option<&CodegenUnit> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(sym) = scope.find_symbol(name) {
                return Some(sym);
            }
        }

        None
    }

    pub fn push_in_scope(&mut self, name: &String, unit: CodegenUnit) {
        self.scope_stack
            .first_mut()
            .expect("Compiler broke. Scope stack is corrupted.")
            .add_symbol(name, unit);
    }

    pub fn get_func(&self, name: &String) -> Option<&CodegenUnit> {
        let val_ref = self.get_in_scope(name);
        match val_ref {
            Some(CodegenUnit::Function(_, _)) => val_ref,
            _ => None,
        }
    }

    pub fn get_params(&self, name: &String) -> Option<Vec<PrimitiveType>> {
        let val_ref = self.get_in_scope(name);
        match val_ref {
            Some(CodegenUnit::Function(_, _)) => val_ref.map(|fun| fun.param_types()),
            _ => None,
        }
    }

    pub fn is_extern(&self, name: &String) -> bool {
        let val_ref = self.get_in_scope(name);
        match val_ref {
            Some(CodegenUnit::Function(_, ty)) => ty.ext(),
            _ => false,
        }
    }

    pub fn get_var(&self, name: &String) -> Option<&CodegenUnit> {
        let val_ref = self.get_in_scope(name);
        match val_ref {
            Some(CodegenUnit::Variable {
                mutable: _,
                name: _,
                ty: _,
                value: _,
            }) => val_ref,
            Some(CodegenUnit::FunctionArgument(_arg, _)) => val_ref,
            _ => None,
        }
    }

    pub fn get_imported_ast(&self, name: &String) -> Option<&AST> {
        self.imported.get(name)
    }

    pub fn llvm_builder_ref(&self) -> &Builder {
        &self.llvm_builder
    }

    pub fn llvm_module_ref(&self) -> &Module {
        &self.llvm_module
    }

    pub fn llvm_context_ref(&self) -> &Context {
        &self.llvm_context
    }

    pub fn typectx_ref(&self) -> &TypeContext {
        &self.typectx
    }

    pub fn add_error(&mut self, error: CompileError) {
        self.compile_errors.push(error);
    }

    pub fn errors(&self) -> &Vec<CompileError> {
        &self.compile_errors
    }
}

fn has_return_type(ty: &PrimitiveType) -> bool {
    match ty {
        PrimitiveType::Function {
            ret_ty,
            params: _,
            ext: _,
        } => match ret_ty {
            box PrimitiveType::Unit => false,
            _ => true,
        },
        _ => panic!("Not a function type {:?}", ty),
    }
}
