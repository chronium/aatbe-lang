use llvm_sys_wrapper::{Builder, Context, LLVMBasicBlockRef, LLVMValueRef, Module, LLVM};
use std::{
    collections::HashMap,
    fs::File,
    io,
    io::prelude::Read,
    path::{Path, PathBuf},
    rc::{Rc, Weak},
};

use crate::{
    codegen::{
        codegen_binary,
        expr::const_expr::fold_constant,
        mangle_v1::NameMangler,
        unit::{
            alloc_variable, declare_and_compile_function, declare_function, init_record,
            store_value,
        },
        CodegenUnit, CompileError, Scope, ValueTypePair,
    },
    fmt::AatbeFmt,
    ty::{
        record::{store_named_field, Record},
        LLVMTyInCtx, TypeContext, TypeKind, TypedefKind,
    },
};

use parser::{
    ast,
    ast::{AtomKind, Expression, PrimitiveType, AST},
    lexer::Lexer,
    parser::Parser,
};

pub type InternalFunc = dyn Fn(&mut AatbeModule, &Vec<Expression>, String) -> Option<ValueTypePair>;

#[allow(dead_code)]
pub struct AatbeModule {
    llvm_context: Context,
    llvm_module: Module,
    name: String,
    scope_stack: Vec<Scope>,
    imported: HashMap<String, AST>,
    imported_cg: Vec<String>,
    typectx: TypeContext,
    compile_errors: Vec<CompileError>,
    record_templates: HashMap<String, AST>,
    function_templates: HashMap<String, Expression>,
    stdlib_path: Option<PathBuf>,
    internal_functions: HashMap<String, Rc<InternalFunc>>,
}

impl AatbeModule {
    pub fn new(name: String, stdlib_path: Option<PathBuf>) -> Self {
        LLVM::initialize();

        let llvm_context = Context::new();
        let llvm_module = llvm_context.create_module(name.as_ref());

        let mut internal_functions: HashMap<String, Rc<InternalFunc>> = HashMap::new();
        internal_functions.insert(String::from("len"), Rc::new(AatbeModule::internal_len));

        Self {
            llvm_context,
            llvm_module,
            name,
            imported: HashMap::new(),
            imported_cg: vec![],
            scope_stack: vec![],
            typectx: TypeContext::new(),
            compile_errors: vec![],
            record_templates: HashMap::new(),
            function_templates: HashMap::new(),
            stdlib_path,
            internal_functions,
        }
    }

    pub fn parse_import(&mut self, module: &String) -> io::Result<AST> {
        let mut f = File::open(format!("{}.aat", module)).or_else(|err| {
            if let Some(stdlib) = &self.stdlib_path {
                File::open(stdlib.join(&Path::new(format!("{}.aat", module).as_str())))
            } else {
                Err(err)
            }
        })?;
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
            Expression::Function { type_names, .. } if type_names.len() == 0 => {
                declare_function(self, expr)
            }
            Expression::Function { name, .. } => {
                if !self.function_templates.contains_key(name) {
                    self.function_templates.insert(name.clone(), expr.clone());
                }
            }
            _ => panic!("Top level {:?} unsupported", expr),
        }
    }

    pub fn decl_pass(&mut self, ast: &AST) {
        self.scope_stack
            .push(Scope::with_builder(Builder::new_in_context(
                self.llvm_context.as_ref(),
            )));
        match ast {
            AST::Constant { .. } | AST::Global { .. } => {}
            AST::Record(name, None, types) => {
                let rec = Record::new(self, name, types);
                self.typectx.push_type(name, TypeKind::RecordType(rec));
                self.typectx.get_record(name).unwrap().set_body(self, types);
            }
            AST::Record(name, Some(type_names), ..) => {
                if self.record_templates.contains_key(name) {
                    panic!("Record {}[{}] already exists", name, type_names.join(", "));
                }
                self.record_templates.insert(name.clone(), ast.clone());
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
            AST::Typedef {
                name,
                type_names: None,
                variants: None,
            } => {
                self.typectx.push_type(
                    &name,
                    TypeKind::Typedef(TypedefKind::Opaque(
                        self.llvm_context_ref()
                            .StructTypeNamed(name.as_ref())
                            .as_ref(),
                    )),
                );
            }
            AST::Typedef {
                type_names: None, ..
            } => self.gen_newtype_ctors(&ast),
            _ => panic!("cannot decl {:?}", ast),
        }
    }

    pub fn gen_newtype_ctors(&mut self, typedef: &AST) {
        match typedef {
            AST::Typedef {
                name,
                type_names: None,
                variants: Some(variants),
            } => {
                if variants.len() == 1 {
                    match &variants[0] {
                        ast::TypeKind::Newtype(ty) => {
                            let newtystruct =
                                self.llvm_context_ref().StructTypeNamed(name.as_ref());
                            newtystruct.set_body(&mut vec![ty.llvm_ty_in_ctx(self)], false);

                            self.internal_functions.insert(
                                name.clone(),
                                Rc::new(|module, values, name| {
                                    let newtystruct = module
                                        .typectx_ref()
                                        .get_type(&name)?
                                        .llvm_ty_in_ctx(module);
                                    if values.len() != 1 {
                                        return None;
                                    }

                                    let val = *(module.codegen_expr(&values[0])?);
                                    let res = module.llvm_builder_ref().build_alloca(newtystruct);
                                    module.llvm_builder_ref().build_store(
                                        val,
                                        module.llvm_builder_ref().build_struct_gep(res, 0),
                                    );
                                    Some((res, PrimitiveType::Newtype(name.clone())).into())
                                }),
                            );
                            self.typectx.push_type(
                                &name.clone(),
                                TypeKind::Typedef(TypedefKind::Newtype(
                                    newtystruct.as_ref(),
                                    ty.clone(),
                                )),
                            );
                        }
                        td => panic!("ICE gen_newtype_ctors {:?}", td),
                    }
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn get_interior_pointer(&self, parts: Vec<String>) -> ValueTypePair {
        if let ([rec_ref], tail) = parts.split_at(1) {
            match self.get_var(&rec_ref) {
                None => panic!("Could not find record {}", rec_ref),
                Some(rec) => {
                    let rec_type = match rec {
                        CodegenUnit::Variable {
                            ty: PrimitiveType::TypeRef(rec),
                            ..
                        } => rec.clone(),
                        CodegenUnit::Variable {
                            ty:
                                PrimitiveType::NamedType {
                                    name: _,
                                    ty: Some(box PrimitiveType::TypeRef(rec)),
                                },
                            ..
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

    pub fn codegen_expr(&mut self, expr: &Expression) -> Option<ValueTypePair> {
        match expr {
            Expression::Ret(box _value) => {
                let val = self.codegen_expr(_value).unwrap();

                let func_ret = self
                    .get_func(self.get_function().as_ref().expect("Compiler borked rets"))
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
                    Some((self.llvm_builder_ref().build_ret(*val), func_ret).into())
                }
            }
            Expression::RecordInit {
                record,
                types,
                values,
            } if types.len() == 0 => {
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
                        PrimitiveType::TypeRef(record.clone()),
                    )
                        .into(),
                )
            }
            Expression::Assign { lval, value } => match value {
                box Expression::RecordInit {
                    record,
                    types,
                    values,
                } => init_record(
                    self,
                    lval,
                    &Expression::RecordInit {
                        record: record.clone(),
                        types: types.clone(),
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
                        *ty.clone(),
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
                            ty,
                        )
                            .into(),
                    )
                })
            }
            Expression::Loop { .. } => self.codegen_basic_loop(expr),
            Expression::If { .. } => self.codegen_if(expr),
            Expression::Call { .. } => self.codegen_call(expr),
            Expression::Binary(lhs, op, rhs) => match codegen_binary(self, op, lhs, rhs) {
                Ok(val) => Some(val),
                Err(err) => {
                    self.add_error(err);
                    None
                }
            },
            Expression::Function { ty, type_names, .. } if type_names.len() == 0 => match ty {
                PrimitiveType::Function {
                    ret_ty: _,
                    params: _,
                    ext: true,
                } => None,
                _ => declare_and_compile_function(self, expr),
            },
            Expression::Function { .. } => None,
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
            }
            | AST::Global {
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
            AST::Expr(expr) => self.codegen_expr(expr).map(|e| *e),
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
            AST::Record(..) => None,
            AST::Typedef { .. } => None,
            _ => panic!("cannot codegen {:?}", ast),
        }
    }

    pub fn start_scope(&mut self) {
        self.scope_stack.push(Scope::new());
    }

    pub fn start_scope_with_name(&mut self, name: &String) {
        self.scope_stack.push(Scope::with_name(name));
    }

    pub fn start_scope_with_function(&mut self, name: &String, builder: Builder) {
        self.scope_stack.push(Scope::with_function(name, builder));
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

    pub fn get_function(&self) -> Option<String> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(function) = scope.function() {
                return Some(function);
            }
        }
        None
    }

    pub fn basic_block(&self, name: String) -> LLVMBasicBlockRef {
        let func = self
            .get_func(self.get_function().as_ref().expect("Compiler borked ifs"))
            .expect("Must be in a function for if statement");

        func.append_basic_block(name)
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

    pub fn propagate_generic_body(
        body: Box<Expression>,
        type_map: HashMap<&String, PrimitiveType>,
    ) -> Box<Expression> {
        box match body {
            box Expression::Atom(atom) => Expression::Atom(match atom {
                AtomKind::Cast(box atom, PrimitiveType::TypeRef(ty_ref)) => {
                    AtomKind::Cast(box atom, type_map[&ty_ref].clone())
                }
                AtomKind::Parenthesized(expr) => AtomKind::Parenthesized(
                    AatbeModule::propagate_generic_body(expr, type_map.clone()),
                ),
                atom => atom,
            }),
            box expr => expr,
        }
    }

    pub fn get_params_generic(
        &mut self,
        template: &String,
        name: &String,
        types: Vec<PrimitiveType>,
    ) -> Option<Vec<PrimitiveType>> {
        self.get_params(name).or_else(|| {
            self.propagate_types_in_function(template, types.clone())
                .and_then(|ty| match self.function_templates.get(template) {
                    Some(Expression::Function {
                        name: fname,
                        body,
                        attributes,
                        type_names,
                        ..
                    }) => {
                        let type_map = type_names.iter().zip(types).collect::<HashMap<_, _>>();
                        let function = Expression::Function {
                            name: fname.clone(),
                            ty: ty.clone(),
                            body: body.as_ref().map(|body| {
                                AatbeModule::propagate_generic_body(body.clone(), type_map)
                            }),
                            attributes: attributes.clone(),
                            type_names: type_names.clone(),
                        };
                        declare_and_compile_function(self, &function);
                        if let PrimitiveType::Function { params, .. } = ty {
                            Some(
                                params
                                    .iter()
                                    .map(|p| match p {
                                        PrimitiveType::NamedType {
                                            ty: Some(box ty), ..
                                        } => ty.clone(),
                                        _ => p.clone(),
                                    })
                                    .collect::<Vec<_>>(),
                            )
                        } else {
                            unreachable!();
                        }
                    }
                    Some(_) => unreachable!(),
                    None => unreachable!(),
                })
        })
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

    pub fn propagate_types_in_function(
        &mut self,
        name: &String,
        types: Vec<PrimitiveType>,
    ) -> Option<PrimitiveType> {
        if types.len() == 0 {
            return None;
        }

        let func = format!(
            "{}[{}]",
            name,
            types
                .iter()
                .map(|ty| ty.fmt())
                .collect::<Vec<_>>()
                .join(", ")
        );
        if !self.function_templates.contains_key(name) {
            self.add_error(CompileError::NoGenericFunction {
                function: func.clone(),
            });
            return None;
        }

        if let Some(Expression::Function {
            type_names,
            ty: PrimitiveType::Function { ret_ty, params, .. },
            ..
        }) = self.function_templates.get(name)
        {
            if type_names.len() != types.len() {
                self.compile_errors
                    .push(CompileError::NoGenericFunction { function: func });
                return None;
            }

            let type_map = type_names.iter().zip(types).collect::<HashMap<_, _>>();

            let ret_ty = match ret_ty {
                box PrimitiveType::TypeRef(ty) => {
                    if type_map.contains_key(ty) {
                        box type_map[ty].clone()
                    } else {
                        box PrimitiveType::TypeRef(ty.clone())
                    }
                }
                _ => ret_ty.clone(),
            };
            let params = params
                .iter()
                .map(|param| match param {
                    PrimitiveType::TypeRef(ty) => {
                        if type_map.contains_key(ty) {
                            type_map[ty].clone()
                        } else {
                            PrimitiveType::TypeRef(ty.clone())
                        }
                    }
                    PrimitiveType::NamedType {
                        name,
                        ty: Some(box PrimitiveType::TypeRef(ty)),
                    } => {
                        if type_map.contains_key(ty) {
                            PrimitiveType::NamedType {
                                name: name.clone(),
                                ty: Some(box type_map[ty].clone()),
                            }
                        } else {
                            PrimitiveType::NamedType {
                                name: name.clone(),
                                ty: Some(box PrimitiveType::TypeRef(ty.clone())),
                            }
                        }
                    }
                    _ => param.clone(),
                })
                .collect::<Vec<_>>();

            let ty = PrimitiveType::Function {
                ext: false,
                ret_ty,
                params,
            };

            let function = Expression::Function {
                name: name.clone(),
                type_names: type_names.clone(),
                ty: ty.clone(),
                body: None,
                attributes: vec![],
            };
            let mangled = function.mangle();

            self.push_in_scope(
                &mangled,
                CodegenUnit::Function(
                    self.llvm_module_ref()
                        .get_or_add_function(mangled.as_ref(), ty.llvm_ty_in_ctx(self)),
                    ty.clone(),
                ),
            );
            Some(ty)
        } else {
            None
        }
    }

    pub fn propagate_types_in_record(&mut self, name: &String, types: Vec<PrimitiveType>) {
        let rec = format!(
            "{}[{}]",
            name,
            types
                .iter()
                .map(|ty| ty.fmt())
                .collect::<Vec<_>>()
                .join(", ")
        );
        if !self.record_templates.contains_key(name) {
            self.add_error(CompileError::NoGenericRecord { rec: rec.clone() })
        }

        if let Some(AST::Record(_, Some(type_names), fields)) = self.record_templates.get(name) {
            if type_names.len() != types.len() {
                self.compile_errors
                    .push(CompileError::NoGenericRecord { rec: rec.clone() })
            }

            let type_map = type_names.iter().zip(types).collect::<HashMap<_, _>>();
            let fields = fields
                .iter()
                .map(|field| {
                    if let PrimitiveType::NamedType { name, ty } = field {
                        if let Some(box PrimitiveType::TypeRef(ty_name)) = ty {
                            if type_map.contains_key(ty_name) {
                                PrimitiveType::NamedType {
                                    name: name.clone(),
                                    ty: Some(box type_map[ty_name].clone()),
                                }
                            } else {
                                *ty.as_ref().unwrap().clone()
                            }
                        } else {
                            *ty.as_ref().unwrap().clone()
                        }
                    } else {
                        panic!("ICE propagate_type {:?}", field)
                    }
                })
                .collect::<Vec<_>>();
            self.typectx
                .push_type(&rec, TypeKind::RecordType(Record::new(self, &rec, &fields)));
            self.typectx
                .get_record(&rec)
                .unwrap()
                .set_body(self, &fields);
        }
    }

    pub fn get_imported_ast(&self, name: &String) -> Option<&AST> {
        self.imported.get(name)
    }

    pub fn llvm_builder_ref(&self) -> &Builder {
        for scope in self.scope_stack.iter().rev() {
            if let Some(builder) = scope.builder() {
                return builder;
            }
        }
        unreachable!();
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

    pub fn has_internal(&self, key: &String) -> bool {
        self.internal_functions.contains_key(key)
    }

    pub fn get_internal(&self, key: &String) -> Weak<InternalFunc> {
        Rc::downgrade(&self.internal_functions[key])
    }
}
