use std::iter::Peekable;

mod ast;
mod lexer;

use ast::AST;
use lexer::token::Token;

pub struct Parser<Iter>
where
  Iter: Iterator<Item = Token>,
{
  tt: Peekable<Iter>,
  pt: Option<AST>,
}

impl<Iter> Parser<Iter>
where
  Iter: Iterator<Item = Token>,
{
  fn new(tt: Iter) -> Self {
    Self {
      tt: tt.peekable(),
      pt: None,
    }
  }
}

#[cfg(test)]
mod parser_tests {}
