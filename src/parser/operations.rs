#[derive(Debug)]
pub enum UnaryOp {
  Negative,
  Negate,
}

#[derive(Debug)]
pub enum BinaryOp {
  Add,
  Subtract,
  Multiply,
  Divide,
  Modulo,
  Equals,
  NotEquals,
}
