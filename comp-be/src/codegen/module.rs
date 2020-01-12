use llvm_sys_wrapper::{Builder, Context, LLVMValueRef, Module, LLVM};
use std::{collections::HashMap, fs::File, io, io::prelude::Read};

use crate::codegen::{
    unit::{
        alloc_variable, codegen_function, declare_function, inject_function_in_scope, store_value,
    },
    CodegenUnit, Scope,
};

use parser::{
    ast::{AtomKind, Boolean, Expression, IntType, PrimitiveType, UIntType, AST},
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

    pub fn codegen_atom(&mut self, atom: &AtomKind) -> Option<LLVMValueRef> {
        match atom {
            AtomKind::StringLiteral(string) => {
                Some(self.llvm_builder.build_global_string_ptr(string.as_str()))
            }
            AtomKind::Integer(val) => Some(self.llvm_context.SInt64(*val)),
            AtomKind::Bool(Boolean::True) => Some(self.llvm_context.SInt1(1)),
            AtomKind::Bool(Boolean::False) => Some(self.llvm_context.SInt1(0)),
            AtomKind::Expr(expr) => self.codegen_expr(expr),
            _ => panic!("ICE codegen_atom {:?}", atom),
        }
    }

    pub fn codegen_expr(&mut self, expr: &Expression) -> Option<LLVMValueRef> {
        match expr {
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
                let mut args = args
                    .iter()
                    .filter_map(|arg| self.codegen_atom(arg))
                    .collect::<Vec<LLVMValueRef>>();
                Some(
                    self.llvm_builder.build_call(
                        self.get_func(name)
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
            /*AST::Ref(name) => {
                let var_ref = self.get_var(name);

                match var_ref {
                    None => panic!("Cannot find variable {}", name),
                    Some(var) => Some(var.load_var(self.llvm_builder_ref())),
                }
            }
            AST::True => Some(self.llvm_context.SInt1(1)),
            AST::False => Some(self.llvm_context.SInt1(0)),
            AST::IntLiteral(ty, val) => Some(self.codegen_const_int(ty, *val)),
            AST::Function {
                name: _,
                ty: _,
                attributes: _,
            } => None,
            AST::Assign(decl, expr) => match decl {
                box AST::Function {
                    name,
                    ty,
                    attributes: _,
                } => {
                    codegen_function(self, decl);
                    self.current_function = Some(name.clone());
                    self.start_scope_with_name(name);
                    inject_function_in_scope(self, decl);
                    let ret = self.codegen_pass(expr);

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
                box AST::Ref(name) => Some(store_value(self, name, expr)),
                _ => panic!("Cannot assign to {:?}", decl),
            },
            AST::Decl(_, _, name, _) => {
                alloc_variable(self, ast);

                Some(self.get_var(name).expect("Compiler crapped out.").into())
            }
            AST::Empty => None,*/
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
            ">=" => self.llvm_builder.build_icmp_sle(x, y),
            ">=" => self.llvm_builder.build_icmp_sge(x, y),
            _ => panic!("Cannot binary op {:?}", op),
        }
    }

    pub fn codegen_const_int(&self, ty: &PrimitiveType, val: u64) -> LLVMValueRef {
        match ty {
            PrimitiveType::Int(IntType::I8) => self.llvm_context.SInt8(val),
            PrimitiveType::Int(IntType::I16) => self.llvm_context.SInt16(val),
            PrimitiveType::Int(IntType::I32) => self.llvm_context.SInt32(val),
            PrimitiveType::Int(IntType::I64) => self.llvm_context.SInt64(val),
            PrimitiveType::UInt(UIntType::U8) => self.llvm_context.UInt8(val),
            PrimitiveType::UInt(UIntType::U16) => self.llvm_context.UInt16(val),
            PrimitiveType::UInt(UIntType::U32) => self.llvm_context.UInt32(val),
            PrimitiveType::UInt(UIntType::U64) => self.llvm_context.UInt64(val),
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

    pub fn push_ref_in_scope(&mut self, name: &String, unit: CodegenUnit) {
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
