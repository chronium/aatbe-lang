#[derive(Debug, Eq, PartialEq, Clone)]
pub enum AST {
  True,
  False,
  File(Vec<AST>),
  Type(PrimitiveType),
  Error,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum PrimitiveType {
  Unit,
}
