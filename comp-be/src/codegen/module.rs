use llvm_sys_wrapper::{Builder, Context, LLVMValueRef, Module, LLVM};
use std::{collections::HashMap, fs::File, io, io::prelude::Read};

use crate::{
    codegen::{
        unit::{
            alloc_variable, codegen_function, declare_function, init_record,
            inject_function_in_scope, store_value,
        },
        CodegenUnit, Scope,
    },
    ty::{
        record::{store_named_field, Record},
        LLVMTyInCtx, TypeContext,
    },
};

use parser::{
    ast::{AtomKind, Boolean, Expression, IntSize, PrimitiveType, AST},
    lexer::Lexer,
    parser::Parser,
};

#[allow(dead_code)]
pub struct AatbeModule {
    llvm_context: Context,
    llvm_module: Module,
    llvm_builder: Builder,
    name: String,
    scope_stack: Vec<Scope>,
    imported: HashMap<String, AST>,
    current_function: Option<String>,
    typectx: TypeContext,
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
            scope_stack: Vec::new(),
            current_function: None,
            typectx: TypeContext::new(),
        }
    }

    pub fn parse_import(&mut self, module: &String) -> io::Result<AST> {
        let mut f = File::open(format!("{}.aat", module))?;
        let mut code = String::new();

        f.read_to_string(&mut code)?;

        let mut lexer = Lexer::new(code.as_str());
        lexer.lex();

        let mut parser = Parser::new(lexer.tt());
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
            AST::Record(name, types) => {
                let rec = Record::new(self, name, types);
                self.typectx.push_type(name, rec);
                self.typectx.get_type(name).unwrap().set_body(self, types);
            }
            AST::File(nodes) => nodes
                .iter()
                .fold(None, |_, n| Some(self.decl_pass(n)))
                .unwrap(),
            AST::Expr(expr) => self.decl_expr(expr),
            AST::Import(module) => {
                let ast = self
                    .parse_import(module)
                    .expect("Something is completely broken");
                self.decl_pass(&ast);
                self.imported
                    .insert(module.clone(), ast)
                    .expect_none(format!("Module {} already imported", module).as_ref());
            }
            _ => panic!("cannot decl {:?}", ast),
        }
    }

    pub fn get_interior_pointer(&self, parts: Vec<String>) -> LLVMValueRef {
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
                                    ty: box PrimitiveType::TypeRef(rec),
                                },
                            value: _,
                        } => rec.clone(),
                        _ => unreachable!(),
                    };

                    self.typectx_ref()
                        .get_type(&rec_type)
                        .expect("ICE get_access variable without type")
                        .read_field(self, rec.into(), rec_ref, tail.to_vec())
                }
            }
        } else {
            unreachable!()
        }
    }

    pub fn codegen_atom(&mut self, atom: &AtomKind) -> Option<LLVMValueRef> {
        match atom {
            AtomKind::NamedValue { name: _, val } => self.codegen_expr(&*val),
            AtomKind::Access(path) => Some(self.llvm_builder_ref().build_load_with_name(
                self.get_interior_pointer(path.to_vec()),
                path.join(".").as_str(),
            )),
            AtomKind::Ident(name) => {
                let var_ref = self.get_var(name);

                match var_ref {
                    None => panic!("Cannot find variable {}", name),
                    Some(var) => Some(var.load_var(self.llvm_builder_ref())),
                }
            }
            AtomKind::StringLiteral(string) => {
                Some(self.llvm_builder.build_global_string_ptr(string.as_str()))
            }
            AtomKind::Integer(val, PrimitiveType::Int(IntSize::Bits32)) => {
                Some(self.llvm_context.SInt32(*val))
            }
            AtomKind::Bool(Boolean::True) => Some(self.llvm_context.SInt1(1)),
            AtomKind::Bool(Boolean::False) => Some(self.llvm_context.SInt1(0)),
            AtomKind::Expr(expr) => self.codegen_expr(expr),
            AtomKind::Unit => None,
            AtomKind::Unary(op, val) if op == &String::from("!") => {
                let value = self
                    .codegen_atom(val)
                    .expect(format!("ICE Cannot negate {:?}", val).as_str());
                Some(self.llvm_builder.build_not(value))
            }
            AtomKind::Parenthesized(expr) => self.codegen_expr(expr),
            AtomKind::RecordInit { record, values } => {
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
                        store_named_field(
                            self,
                            rec,
                            record,
                            self.typectx_ref()
                                .get_type(record)
                                .expect(format!("ICE could not find record {}", record).as_str()),
                            name,
                            val_ref,
                        );
                    }
                    _ => panic!("ICE codegen_atom {:?}", atom),
                });

                Some(self.llvm_builder_ref().build_load(rec))
            }
            _ => panic!("ICE codegen_atom {:?}", atom),
        }
    }

    pub fn codegen_expr(&mut self, expr: &Expression) -> Option<LLVMValueRef> {
        match expr {
            Expression::Assign { lval, value } => match value {
                box Expression::Atom(AtomKind::RecordInit { record, values }) => Some(init_record(
                    self,
                    lval,
                    &AtomKind::RecordInit {
                        record: record.clone(),
                        values: values.to_vec(),
                    },
                )),
                _ => Some(store_value(self, lval, value)),
            },
            Expression::Decl {
                ty: PrimitiveType::NamedType { name, ty: _ },
                value: _,
                exterior_bind: _,
            } => {
                alloc_variable(self, expr);

                Some(self.get_var(name).expect("Compiler crapped out.").into())
            }
            Expression::If {
                cond_expr,
                then_expr,
                else_expr,
            } => {
                else_expr
                    .as_ref()
                    .expect_none("Else conditions not supported");

                let func = self
                    .get_func(self.current_function.as_ref().expect("Compiler borked ifs"))
                    .expect("Must be in a function for if statement");

                let then_bb = func.append_basic_block(String::default());
                let end_bb = func.append_basic_block(String::default());

                let cond = self.codegen_expr(cond_expr);

                self.llvm_builder
                    .build_cond_br(cond.unwrap(), then_bb, end_bb);

                self.llvm_builder.position_at_end(then_bb);
                self.codegen_expr(then_expr);
                self.llvm_builder.build_br(end_bb);
                self.llvm_builder.position_at_end(end_bb);

                None
            }
            Expression::Call { name, args } => {
                let mut name = name.clone();

                args.iter().for_each(|arg| match arg {
                    AtomKind::SymbolLiteral(symbol) => name.push_str(&format!("__{}", symbol)),
                    _ => {}
                });

                let mut args = args
                    .iter()
                    .filter_map(|arg| match arg {
                        AtomKind::SymbolLiteral(_) => None,
                        _ => self.codegen_atom(arg),
                    })
                    .collect::<Vec<LLVMValueRef>>();
                Some(
                    self.llvm_builder.build_call(
                        self.get_func(&name)
                            .expect(format!("Call to undefined function {}", name).as_str())
                            .into(),
                        &mut args,
                    ),
                )
            }
            Expression::Binary(lhs, op, rhs) => {
                let lh = self
                    .codegen_expr(lhs)
                    .expect("Binary op lhs must contain a value");
                let rh = self
                    .codegen_expr(rhs)
                    .expect("Binary op rhs must contain a value");
                Some(self.codegen_binary_op(op, lh, rh))
            }
            Expression::Function {
                name,
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
                        codegen_function(self, expr);
                        self.current_function = Some(name.clone());
                        self.start_scope_with_name(name);
                        inject_function_in_scope(self, expr);
                        let ret = self.codegen_expr(
                            &body
                                .as_ref()
                                .expect("ICE Function with no body but not external"),
                        );

                        // TODO: Typechecks
                        match has_return_type(ty) {
                            true => self.llvm_builder.build_ret(ret.expect(
                                "Compiler broke, function returns broke. Everything's on fire",
                            )),
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
            AST::File(nodes) => nodes
                .iter()
                .fold(None, |_, n| Some(self.codegen_pass(n)))
                .unwrap(),
            AST::Expr(expr) => self.codegen_expr(expr),
            AST::Import(module) => {
                let ast = self
                    .get_imported_ast(module)
                    .expect("Cannot find already declared imported module? wat")
                    .clone();
                self.codegen_pass(&ast);
                None
            }
            AST::Record(_, _) => None,
            _ => panic!("cannot codegen {:?}", ast),
        }
    }

    pub fn codegen_binary_op(&self, op: &String, x: LLVMValueRef, y: LLVMValueRef) -> LLVMValueRef {
        match op.as_str() {
            "+" => self.llvm_builder.build_add(x, y),
            "-" => self.llvm_builder.build_sub(x, y),
            "*" => self.llvm_builder.build_mul(x, y),
            "/" => self.llvm_builder.build_sdiv(x, y),
            "%" => self.llvm_builder.build_srem(x, y),
            "==" => self.llvm_builder.build_icmp_eq(x, y),
            "!=" => self.llvm_builder.build_icmp_ne(x, y),
            "<" => self.llvm_builder.build_icmp_slt(x, y),
            ">" => self.llvm_builder.build_icmp_sgt(x, y),
            "<=" => self.llvm_builder.build_icmp_sle(x, y),
            ">=" => self.llvm_builder.build_icmp_sge(x, y),
            _ => panic!("Cannot binary op {:?}", op),
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
            Some(CodegenUnit::Function(_)) => val_ref,
            _ => None,
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
            Some(CodegenUnit::FunctionArgument(_arg)) => val_ref,
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
