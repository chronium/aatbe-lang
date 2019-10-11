use llvm_sys_wrapper::*;
use std::ops::Deref;

#[derive(Debug)]
pub enum AstNode<'a> {
    Call(LLVMValueRef, &'a mut [LLVMValueRef]),
    GlobalStrPtr(&'a str),
    Ref(LLVMTypeRef),
    Assign(Box<AstNode<'a>>, Box<AstNode<'a>>),
    I32Const(i32),
    I64Const(i64),
    U32Const(u32),
    U64Const(u64),
    /*Ref(String, LLVMTypeRef),
    Assign(String, LLVMValueRef, Box<AstNode<'a>>)*/
    Add(Box<AstNode<'a>>, Box<AstNode<'a>>),
    Sub(Box<AstNode<'a>>, Box<AstNode<'a>>),
    Mul(Box<AstNode<'a>>, Box<AstNode<'a>>),
    Div(Box<AstNode<'a>>, Box<AstNode<'a>>),
    Load(LLVMValueRef),
    LoadRef(Box<AstNode<'a>>),
}

impl<'a> AstNode<'a> {
    pub fn emit(&mut self, builder: &Builder) -> LLVMValueRef {
        match self {
            Self::Call(func, args) => builder.build_call(*func, *args),
            Self::GlobalStrPtr(val) => builder.build_global_string_ptr(val),
            Self::Ref(val_type) => builder.build_alloca(*val_type),
            Self::Assign(var, val) => {
                let var_ref = var.emit(&builder);
                builder.build_store(val.emit(&builder), var_ref);
                var_ref
            }
            Self::I32Const(val) => LLVM::Const::SInt32(*val as u64),
            Self::I64Const(val) => LLVM::Const::SInt64(*val as u64),
            Self::U32Const(val) => LLVM::Const::UInt32(*val as u64),
            Self::U64Const(val) => LLVM::Const::UInt64(*val as u64),
            Self::Add(a, b) => builder.build_add(a.emit(&builder), b.emit(&builder)),
            Self::Sub(a, b) => builder.build_sub(a.emit(&builder), b.emit(&builder)),
            Self::Mul(a, b) => builder.build_mul(a.emit(&builder), b.emit(&builder)),
            Self::Div(a, b) => builder.build_sdiv(a.emit(&builder), b.emit(&builder)),
            Self::Load(val) => builder.build_load(*val),
            Self::LoadRef(val) => builder.build_load(val.emit(&builder)),
        }
    }
}

struct Func {
    function: Function,
}

impl Deref for Func {
    type Target = Function;

    fn deref(&self) -> &Function {
        &self.function
    }
}

impl Func {
    fn new(module: &Module, name: String, func_type: LLVMTypeRef) -> Self {
        Self {
            function: module.add_function(name.as_ref(), func_type),
        }
    }

    fn append_block(&self, name: String) -> LLVMBasicBlockRef {
        self.append_basic_block(name.as_ref())
    }

    fn append_and_position(&self, name: String, builder: &Builder) -> LLVMBasicBlockRef {
        let block = self.append_block(name);
        builder.position_at_end(block);
        block
    }
}

peg::parser! {
    grammar lang_parser() for str {
        use self::AstNode;

        rule _() = quiet!{[' ' | '\n' | '\t']+}

        rule number<'a>() -> AstNode<'a>
            = n:$(['0'..='9']+) { AstNode::I64Const(n.parse().unwrap()) }

        rule ident() -> &'input str
            = $(quiet!{['a'..='z' | 'A'..='Z']['a'..='z' | 'A'..='Z' | '0'..='9']*})
            / expected!("identifier")

        pub rule expr<'a>() -> AstNode<'a> = precedence! {
            x:(@) _ "+" _ y:@ { AstNode::Add(Box::new(x), Box::new(y)) }
            x:(@) _ "-" _ y:@ { AstNode::Sub(Box::new(x), Box::new(y)) }
            --
            x:(@) _ "*" _ y:@ { AstNode::Mul(Box::new(x), Box::new(y)) }
            x:(@) _ "/" _ y:@ { AstNode::Div(Box::new(x), Box::new(y)) }
            --
            n:number() { n }
        }

        pub rule assign<'a>() -> AstNode<'a>
            = ident() _ "=" _ expr:expr() {
                AstNode::Assign(Box::new(AstNode::Ref(LLVM::Type::Int64())), Box::new(expr))
            }
        pub rule file<'a>() -> AstNode<'a>
            = expr()
            / var:assign() {? Ok(AstNode::LoadRef(Box::new(var))) }
            / expected!("Expected either expression or variable assignment")
    }
}

fn main() {
    LLVM::initialize();

    let context = Context::global_context();

    let builder = context.create_builder();
    'repl: loop {
        use std::io::{stdin, stdout, Write};
        print!(">>> ");
        let mut input = String::new();
        let _ = stdout().flush();
        stdin().read_line(&mut input).unwrap();

        match &*input.trim() {
            ":quit" => break 'repl,
            code => {
                let module = context.create_module(&String::default());

                let main = Func::new(&module, "main".to_string(), fn_type!(context.Int64Type()));
                main.append_and_position("entry".to_string(), &builder);

                let mut parsed = match lang_parser::file(&*code) {
                    Ok(val) => val,
                    Err(e) => {
                        continue;
                    }
                };

                builder.build_ret(parsed.emit(&builder));

                // verify & dump
                match module.verify() {
                    Ok(_) => {
                        let interpreter = module.create_jit_engine().unwrap();
                        let named_function = module.named_function("main");
                        let mut params = [];
                        let run_result =
                            interpreter.run_function(named_function.as_ref(), &mut params);
                        println!("> {}", run_result.to_int());
                    }
                    Err(msg) => panic!("Error: {}", msg),
                }
            }
        }
    }
}
