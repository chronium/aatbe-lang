#[derive(Debug, Eq, PartialEq, Clone)]
pub enum AST {
  File(Vec<AST>),
  Type(PrimitiveType),
  Error,
  Atom(AtomKind),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum PrimitiveType {
  Unit,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum AtomKind {
  Bool(Boolean),
  Integer(u64),
  Unit,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Boolean {
  True,
  False,
}
