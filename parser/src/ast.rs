#[derive(Debug, Eq, PartialEq, Clone)]
pub enum AST {
  File(Vec<AST>),
  Type(PrimitiveType),
  Error,
  Expr(Expression),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expression {
  Atom(AtomKind),
  Function {
    name: String,
    ty: PrimitiveType,
    body: Option<Box<Expression>>,
    attributes: Vec<String>,
  },
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum PrimitiveType {
  Unit,
  Function {
    ext: bool,
    ret_ty: Box<PrimitiveType>,
  },
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
