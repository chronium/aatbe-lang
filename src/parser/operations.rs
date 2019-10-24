#[derive(Debug, Eq, PartialEq)]
pub enum UnaryOp {
  Negative,
  Negate,
}

#[derive(Debug, Eq, PartialEq)]
pub enum BinaryOp {
  Add,
  Subtract,
  Multiply,
  Divide,
  Modulo,
  Equals,
  NotEquals,
}
