use super::operations::{BinaryOp, UnaryOp};
use super::primitive_type::PrimitiveType;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum VarType {
  Const,
  Mutable,
  Immutable,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum AST {
  True,
  False,
  Empty,
  IntLiteral(PrimitiveType, u64),
  CharLiteral(char),
  StringLiteral(String),
  Unary(UnaryOp, Box<AST>),
  Binary(BinaryOp, Box<AST>, Box<AST>),
  Parenthesized(Box<AST>),
  If {
    condition: Box<AST>,
    then_block: Box<AST>,
  },
  IfElse {
    if_expr: Box<AST>,
    else_expr: Box<AST>,
  },
  Block(Vec<AST>),
  Decl(VarType, PrimitiveType, String, Box<Option<AST>>),
  Assign(Box<AST>, Box<AST>),
  Index {
    lhs: Box<AST>,
    indexer: Box<AST>,
  },
  Ref(String),
  Tuple(Vec<AST>),
  AddrOf(Box<AST>),
  Function {
    name: String,
    ty: Box<PrimitiveType>,
    attributes: Vec<String>,
  },
  Call {
    name: String,
    arg: Box<AST>,
  },
  File(Vec<AST>),
  Import(String),
}