use crate::codegen::unit::function::Func;
use llvm_sys_wrapper::{Builder, Context, LLVMBasicBlockRef, LLVMValueRef, Module, LLVM};
use std::{
    collections::HashMap,
    io,
    path::{Path, PathBuf},
    rc::{Rc, Weak},
};

use crate::{
    codegen::{
        comp_unit::CompilationUnit,
        mangle_v1::NameMangler,
        unit::{
            alloc_variable, declare_and_compile_function, declare_function, init_record,
            store_value, CompilerUnit, Slot,
        },
        CompileError, Scope, ValueTypePair,
    },
    fmt::AatbeFmt,
    ty::{
        record::store_named_field,
        size::AatbeSizeOf,
        variant::{Variant, VariantType},
        LLVMTyInCtx, Record, TypeContext, TypeKind, TypedefKind,
    },
};

use super::{
    builder::{base, cast},
    unit::function::{find_func, FuncTyMap},
};
use parser::{
    ast,
    ast::{AtomKind, Expression, FunctionType, Type, AST},
};

pub type InternalFunc = dyn Fn(&mut AatbeModule, &Vec<Expression>, String) -> Option<ValueTypePair>;

#[allow(dead_code)]
pub struct AatbeModule {
    llvm_context: Context,
    llvm_module: Module,
    name: String,
    compile_errors: Vec<CompileError>,
    record_templates: HashMap<String, AST>,
    function_templates: HashMap<String, Expression>,
    stdlib_path: Option<PathBuf>,
    internal_functions: HashMap<String, Rc<InternalFunc>>,
    compilation_units: HashMap<String, CompilationUnit>,
}

impl AatbeModule {
    pub fn new(name: String, base_cu: CompilationUnit, stdlib_path: Option<PathBuf>) -> Self {
        LLVM::initialize();

        let llvm_context = Context::new();
        let llvm_module = llvm_context.create_module(&name);

        let mut internal_functions: HashMap<String, Rc<InternalFunc>> = HashMap::new();
        internal_functions.insert(String::from("len"), Rc::new(AatbeModule::internal_len));
        internal_functions.insert(String::from("box"), Rc::new(AatbeModule::internal_box));

        let mut compilation_units = HashMap::new();
        compilation_units.insert(name.clone(), base_cu);

        Self {
            llvm_module,
            llvm_context,
            name,
            compile_errors: vec![],
            record_templates: HashMap::new(),
            function_templates: HashMap::new(),
            stdlib_path,
            internal_functions,
            compilation_units,
        }
    }

    pub fn compile(&mut self) {
        let base_cu = self.compilation_units.get(&self.name).unwrap();
        let main_ast = self
            .compilation_units
            .get(&self.name.clone())
            .unwrap()
            .ast()
            .clone();

        let root_builder = Builder::new_in_context(self.llvm_context.as_ref());

        CompilerUnit::new(
            base_cu.path().clone(),
            &self.llvm_context,
            &self.llvm_module,
        )
        .in_root_scope(|root_module| {
            root_module.decl(&root_builder, &main_ast);
            //root_module.generics(&root_builder, &main_ast);
            //root_module.decl(&root_builder, &main_ast);
            //root_module.codegen(&root_builder, &main_ast);
            root_module.codegen(&root_builder, &main_ast);
        });
    }

    /*pub fn parse_import(&mut self, module: &String) -> io::Result<CompilationUnit> {
        let mut path = self.fdir().with_file_name(module);
        path.set_extension("aat");
        if !path.exists() {
            path = PathBuf::from(
                self.stdlib_path
                    .as_ref()
                    .unwrap()
                    .join(&Path::new(format!("{}.aat", module).as_str())),
            );
        }

        CompilationUnit::new(path)
    }*/

    pub fn decl_pass(&mut self, root_module: &mut CompilerUnit) {

        /*match ast {
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
                let cu = self
                    .parse_import(module)
                    .expect("Something is completely broken");
                if !self.compilation_units.contains_key(cu.name()) {
                    self.scope_stack.push(Scope::with_builder_and_fdir(
                        Builder::new_in_context(self.llvm_context.as_ref()),
                        cu.path().clone(),
                    ));

                    self.decl_pass(&cu.ast());
                    self.codegen_pass(&cu.ast());
                    self.compilation_units.insert(cu.name().clone(), cu);

                    self.exit_scope();
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
            AST::Module(name, ast) => {
                self.root_module.push(name, ModuleUnit::new(ast.clone()));
            }
            _ => panic!("cannot decl {:?}", ast),
        }*/
    }

    #[allow(unused_unsafe)]
    pub fn gen_variants(&mut self, typedef: &AST) {
        todo!()
        /*match typedef {
            AST::Typedef {
                name,
                type_names: None,
                variants: Some(variants),
            } => {
                let max_size = variants.iter().map(|vari| vari.size_of()).max().unwrap();
                let smallest = variants.iter().map(|vari| vari.smallest()).min().unwrap();
                let size = max_size / smallest;

                let smallest_ty = Type::UInt(smallest.into());

                let td_struct = self.llvm_context_ref().StructTypeNamed(name.as_ref());
                td_struct.set_body(
                    &mut vec![
                        smallest_ty.llvm_ty_in_ctx(self),
                        self.llvm_context_ref()
                            .ArrayType(smallest_ty.llvm_ty_in_ctx(self), size as u32),
                    ],
                    false,
                );

                self.typectx.push_type(
                    name,
                    TypeKind::Typedef(TypedefKind::VariantType(VariantType {
                        type_name: name.clone(),
                        variants: HashMap::new(),
                        discriminant_type: smallest_ty.clone(),
                        ty: td_struct.as_ref(),
                    })),
                );

                variants
                    .iter()
                    .enumerate()
                    .for_each(|(i, variant)| match variant {
                        ast::TypeKind::Variant(variant_name, types) => {
                            let ty = self
                                .llvm_context_ref()
                                .StructTypeNamed(variant_name.as_ref());
                            let mut tys = vec![smallest_ty.llvm_ty_in_ctx(self)];
                            if let Some(types) = types {
                                tys.extend(types.iter().map(|ty| ty.llvm_ty_in_ctx(self)));
                            };
                            ty.set_body(tys.as_mut(), true);

                            self.typectx_ref_mut()
                                .push_variant(
                                    name,
                                    variant_name,
                                    Variant {
                                        parent_name: name.clone(),
                                        name: variant_name.clone(),
                                        types: types.clone(),
                                        ty: ty.as_ref(),
                                        discriminant: i as u32,
                                    },
                                )
                                .expect(format!("Cannot find variant {}", name.clone()).as_ref());
                        }
                        _ => unimplemented!("{:?}", variant),
                    });

                variants
                    .iter()
                    .enumerate()
                    .for_each(|(i, variant)| match variant {
                        ast::TypeKind::Variant(variant_name, _) => {
                            self.internal_functions.insert(
                                variant_name.clone(),
                                Rc::new(move |module, values, name| {
                                    let values = values
                                        .iter()
                                        .filter_map(|value| module.codegen_expr(&value))
                                        .collect::<Vec<_>>();
                                    if let Some(Variant {
                                        name, types, ty, ..
                                    }) = module.typectx_ref().get_variant(&name)
                                    {
                                        let parent_ty = module
                                            .typectx_ref()
                                            .get_parent_for_variant(&name)
                                            .unwrap();
                                        let parent = module
                                            .llvm_builder_ref()
                                            .build_alloca(parent_ty.llvm_ty_in_ctx(module));
                                        let discriminator =
                                            module.llvm_builder_ref().build_bitcast(
                                                parent,
                                                module.llvm_context_ref().PointerType(
                                                    parent_ty
                                                        .discriminant_type
                                                        .llvm_ty_in_ctx(module),
                                                ),
                                            );

                                        module.llvm_builder_ref().build_store(
                                            *const_atom(
                                                module,
                                                &AtomKind::Integer(
                                                    i as u64,
                                                    parent_ty.discriminant_type.clone(),
                                                ),
                                            )
                                            .unwrap(),
                                            discriminator,
                                        );
                                        let variant = module.llvm_builder_ref().build_bitcast(
                                            parent,
                                            module.llvm_context_ref().PointerType(unsafe { *ty }),
                                        );
                                        if let Some(types) = types {
                                            if types.len() != values.len() {
                                                return None;
                                            }

                                            values.iter().enumerate().for_each(|(i, value)| {
                                                module.llvm_builder_ref().build_store(
                                                    **value,
                                                    module
                                                        .llvm_builder_ref()
                                                        .build_struct_gep(variant, (i + 1) as u32),
                                                );
                                            });
                                        }
                                        Some(
                                            (variant, Type::VariantType(name.clone()))
                                                .into(),
                                        )
                                    } else {
                                        None
                                    }
                                }),
                            );
                        }
                        _ => unimplemented!(),
                    });
            }
            _ => unreachable!(),
        }*/
    }

    pub fn gen_newtype_ctors(&mut self, typedef: &AST) {
        todo!()
        /*match typedef {
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
                                    Some((res, Type::Newtype(name.clone())).into())
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
                        ast::TypeKind::Variant(..) => self.gen_variants(typedef),
                    }
                } else {
                    self.gen_variants(typedef);
                }
            }
            _ => unreachable!(),
        }*/
    }

    /*pub fn get_interior_pointer(&self, parts: Vec<String>) -> Option<ValueTypePair> {
        match parts.as_slice() {
            [field, access @ ..] => {
                let agg_bind = self.get_var(&field)?;
                let agg_ty = self
                    .typectx_ref()
                    .get_aggregate_from_prim(&agg_bind.var_ty())?;

                Some(
                    self.typectx_ref()
                        .gep_fields(self, agg_ty, access.to_vec(), agg_bind.into())
                        .expect(format!("ICE get_field_named {}", field).as_str()),
                )
            }
            [] => unreachable!(),
        }
    }*/

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
        todo!()
        /*match expr {
            Expression::Ret(box _value) => {
                let val = self.codegen_expr(_value)?;

                let func_ret = self
                    .get_func(self.get_function().expect("Compiler borked rets"))
                    .expect("Must be in a function for ret statement")
                    .ret_ty()
                    .clone();

                if let Type::VariantType(ty) = val.prim() {
                    let parent_ty = self
                        .typectx_ref()
                        .get_parent_for_variant(ty)
                        .expect("ICE: Variant without parent");
                    if let Type::TypeRef(name) = &func_ret {
                        if &parent_ty.type_name == name {
                            Some(core::ret(
                                self,
                                (cast::child_to_parent(self, val, parent_ty), func_ret).into(),
                            ))
                        } else if &parent_ty.type_name != name && ty != name {
                            self.add_error(CompileError::ExpectedType {
                                expected_ty: func_ret.fmt(),
                                found_ty: val.prim().fmt(),
                                value: _value.fmt(),
                            });
                            None
                        } else {
                            panic!("ICE");
                        }
                    } else {
                        panic!("ICE");
                    }
                } else if val.prim() != &func_ret {
                    self.add_error(CompileError::ExpectedType {
                        expected_ty: func_ret.fmt(),
                        found_ty: val.prim().fmt(),
                        value: _value.fmt(),
                    });
                    None
                } else {
                    Some(core::ret(self, val))
                }
            }
            /*Expression::RecordInit {
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
                        Type::TypeRef(record.clone()),
                    )
                        .into(),
                )
            }*/
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
            Expression::Loop { .. } => self.codegen_basic_loop(expr),
            Expression::Binary(lhs, op, rhs) => match codegen_binary(self, op, lhs, rhs) {
                Ok(val) => Some(val),
                Err(_) => {
                    self.add_error(CompileError::FailedBinary {
                        op: op.clone(),
                        lhs: lhs.fmt(),
                        rhs: rhs.fmt(),
                    });
                    None
                }
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
            _ => panic!("ICE: codegen_expr {:?}", expr),
        }*/
    }

    pub fn codegen_pass(&mut self, root_module: &mut CompilerUnit) -> Option<LLVMValueRef> {
        todo!()
        /*match ast {
            AST::Constant {
                ty: Type::NamedType { name, ty: _ },
                export,
                value: _,
            }
            | AST::Global {
                ty: Type::NamedType { name, ty: _ },
                export,
                value: _,
            } => {
                fold_constant(self, ast).map(|constant| {
                    if !export {
                        self.push_in_scope(name, constant)
                    } else {
                        self.export_global(name, constant)
                    }
                });
                None
            }
            AST::File(nodes) => nodes
                .iter()
                .fold(None, |_, n| Some(self.codegen_pass(n)))
                .unwrap(),
            AST::Expr(expr) => self.codegen_expr(expr).map(|e| *e),
            AST::Import(..) => None,
            AST::Record(..) => None,
            AST::Typedef { .. } => None,
            AST::Module(name, _) => self
                .root_module
                .get_mut(name)
                .expect(format!("ICE: Module codegen but none found").as_str())
                .codegen(),
            _ => panic!("cannot codegen {:?}", ast),
        }*/
    }

    /*pub fn start_scope(&mut self) {
        self.scope_stack.push(Scope::new());
    }

    pub fn start_scope_with_name(&mut self, name: &String) {
        self.scope_stack.push(Scope::with_name(name));
    }

    pub fn get_in_scope(&self, name: &String) -> Option<&Slot> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(sym) = scope.find_symbol(name) {
                return Some(sym);
            }
        }

        None
    }

    pub fn get_function(&self) -> Option<(String, FunctionType)> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(function) = scope.function() {
                return Some(function);
            }
        }
        None
    }

    pub fn basic_block(&mut self, name: String) -> LLVMBasicBlockRef {
        for scope in self.scope_stack.iter().rev() {
            let bb = scope.bb(self, &name);
            if let Some(bb) = bb {
                return bb;
            }
        }
        panic!("Compiler broke. Scope stack is corrupted.");
    }

    pub fn push_in_scope(&mut self, name: &String, unit: Slot) {
        self.scope_stack
            .last_mut()
            .expect("Compiler broke. Scope stack is corrupted.")
            .add_symbol(name, unit);
    }



    pub fn export_global(&mut self, name: &String, unit: Slot) {
        self.scope_stack
            .first_mut()
            .expect("Compiler broke. Scope stack is corrupted.")
            .add_symbol(name, unit);
    }

    */

    pub fn propagate_generic_body(
        body: Box<Expression>,
        type_map: HashMap<&String, Type>,
    ) -> Box<Expression> {
        box match body {
            box Expression::Atom(atom) => Expression::Atom(match atom {
                AtomKind::Cast(box atom, Type::TypeRef(ty_ref)) => {
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

    /*pub fn get_params_generic(
        &mut self,
        template: &String,
        name: &String,
        types: Vec<Type>,
    ) -> Option<Vec<Type>> {
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
                            export: true,
                        };
                        declare_and_compile_function(self, &function);
                        if let FunctionType { params, .. } = ty {
                            Some(
                                params
                                    .iter()
                                    .map(|p| match p {
                                        Type::NamedType {
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
    }*/

    /*pub fn is_extern(&self, func: (String, FunctionType)) -> bool {
        let val_ref = self.get_func(func);
        if let Some(func) = val_ref {
            func.is_extern()
        } else {
            false
        }
    }

    pub fn get_var(&self, name: &String) -> Option<&Slot> {
        let val_ref = self.get_in_scope(name);
        match val_ref {
            Some(Slot::Variable {
                mutable: _,
                name: _,
                ty: _,
                value: _,
            }) => val_ref,
            Some(Slot::FunctionArgument(_arg, _)) => val_ref,
            _ => None,
        }
    }*/

    /*pub fn propagate_types_in_function(
        &mut self,
        name: &String,
        types: Vec<Type>,
    ) -> Option<FunctionType> {
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
            ty: FunctionType { ret_ty, params, .. },
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
                box Type::TypeRef(ty) => {
                    if type_map.contains_key(ty) {
                        box type_map[ty].clone()
                    } else {
                        box Type::TypeRef(ty.clone())
                    }
                }
                _ => ret_ty.clone(),
            };
            let params = params
                .iter()
                .map(|param| match param {
                    Type::TypeRef(ty) => {
                        if type_map.contains_key(ty) {
                            type_map[ty].clone()
                        } else {
                            Type::TypeRef(ty.clone())
                        }
                    }
                    Type::NamedType {
                        name,
                        ty: Some(box Type::TypeRef(ty)),
                    } => {
                        if type_map.contains_key(ty) {
                            Type::NamedType {
                                name: name.clone(),
                                ty: Some(box type_map[ty].clone()),
                            }
                        } else {
                            Type::NamedType {
                                name: name.clone(),
                                ty: Some(box Type::TypeRef(ty.clone())),
                            }
                        }
                    }
                    _ => param.clone(),
                })
                .collect::<Vec<_>>();

            let ty = FunctionType {
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
                export: false,
            };

            self.add_function(
                &name,
                Func::new(
                    ty.clone(),
                    name.clone(),
                    self.llvm_module_ref().get_or_add_function(
                        function.mangle(self).as_ref(),
                        ty.llvm_ty_in_ctx(self),
                    ),
                ),
            );
            Some(ty)
        } else {
            None
        }
    }*/

    pub fn propagate_types_in_record(&mut self, name: &String, types: Vec<Type>) {
        todo!()
        /*let rec = format!(
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
                    if let Type::NamedType { name, ty } = field {
                        if let Some(box Type::TypeRef(ty_name)) = ty {
                            if type_map.contains_key(ty_name) {
                                Type::NamedType {
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
        }*/
    }

    /*pub fn fdir(&self) -> PathBuf {
        for scope in self.scope_stack.iter().rev() {
            if let Some(fdir) = scope.fdir() {
                return fdir.clone();
            }
        }
        unreachable!();
    }

    */

    pub fn llvm_module_ref(&self) -> &Module {
        &self.llvm_module
    }

    pub fn llvm_context_ref(&self) -> &Context {
        todo!() //&self.llvm_context
    }

    pub fn typectx_ref(&self) -> &TypeContext {
        todo!()
        //&self.typectx
    }

    pub fn typectx_ref_mut(&mut self) -> &mut TypeContext {
        todo!()
        //&mut self.typectx
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
