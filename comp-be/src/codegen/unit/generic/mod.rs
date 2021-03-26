use std::collections::{HashMap, HashSet};

use parser::ast::{Expression, IdentPath, PrimitiveType, AST};

mod gen;

#[derive(Debug)]
pub struct Processor {
    pub(super) function_calls: HashMap<IdentPath, HashSet<Vec<PrimitiveType>>>,
    extracted_tree: Option<AST>,
    pub generated_tree: Option<AST>,
}

impl Processor {
    pub fn new() -> Self {
        Self {
            function_calls: HashMap::new(),
            extracted_tree: None,
            generated_tree: None,
        }
    }

    pub fn extract_function_calls(&mut self, ast: &AST) -> &mut Self {
        match ast {
            AST::Module(..) => {
                // TODO: Module
            }
            AST::File(body) => body.iter().for_each(|ast| {
                self.extract_function_calls(ast);
            }),
            AST::Expr(expr) => self.from_expression(expr),
            _ => todo!("{:?}", ast),
        };

        self
    }

    fn from_expression(&mut self, expr: &Expression) {
        match expr {
            Expression::Function {
                body: Some(box body),
                ..
            } => self.from_expression(body),
            Expression::Call { name, types, .. } if types.len() > 0 => {
                self.insert_function_call(name, types.clone())
            }
            Expression::Block(body) => body.iter().for_each(|e| self.from_expression(e)),
            _ => {}
        }
    }

    pub fn extract_tree(&mut self, ast: &AST) -> &mut Self {
        println!("{:#?}", ast);
        self.extracted_tree = extract_ast(ast);
        println!("{:#?}", self.extracted_tree);
        self
    }

    pub fn generate_tree(&mut self) -> &mut Self {
        let ast = self.extracted_tree.as_ref().unwrap();
        self.generated_tree = gen::gen(ast, &self);
        println!("{:#?}", self.generated_tree);
        self
    }

    fn insert_function_call(&mut self, name: &IdentPath, types: Vec<PrimitiveType>) {
        if !self.function_calls.contains_key(name) {
            self.function_calls.insert(name.clone(), HashSet::new());
        }

        self.function_calls.get_mut(name).unwrap().insert(types);
    }
}

fn extract_ast(ast: &AST) -> Option<AST> {
    match ast {
        AST::Module(..) => {
            // TODO: Module
            None
        }
        AST::File(body) => Some(AST::File(
            body.iter().filter_map(extract_ast).collect::<Vec<_>>(),
        )),
        AST::Expr(expr) => extract_expr(expr).and_then(|expr| Some(AST::Expr(expr))),
        _ => todo!("{:?}", ast),
    }
}

fn extract_expr(expr: &Expression) -> Option<Expression> {
    match expr {
        Expression::Function { type_names, .. } if type_names.len() > 0 => Some(expr.clone()),
        _ => None,
    }
}
