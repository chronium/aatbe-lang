use crate::{
    ast::Expression,
    parser::{ParseError, ParseResult, Parser},
    token::Keyword,
};

impl Parser {
    pub fn parse_if_else(&mut self) -> ParseResult<Expression> {
        kw!(If, self);

        let cond_expr =
            box capture!(self, parse_expression).ok_or(ParseError::ExpectedCondition)?;
        let then_expr =
            box capture!(self, parse_expression).ok_or(ParseError::ExpectedThenExpression)?;
        let mut else_expr = None;

        let has_else = kw!(bool Else, self);

        if has_else {
            else_expr =
                Some(box capture!(self, parse_expression).ok_or(ParseError::ExpectedExpression)?);
        }

        Ok(Expression::If {
            cond_expr,
            then_expr,
            else_expr,
        })
    }
}
