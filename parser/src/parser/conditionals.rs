use crate::{
    ast::{Expression, LoopType},
    parser::{ParseError, ParseResult, Parser},
    token::Keyword,
};

impl Parser {
    pub fn parse_if_else(&mut self) -> ParseResult<Expression> {
        kw!(If, self);

        let cond_expr =
            box capture!(self, parse_expression).ok_or(ParseError::ExpectedCondition)?;
        kw!(bool Then, self);
        let then_expr =
            box capture!(self, parse_expression).ok_or(ParseError::ExpectedThenExpression)?;
        let mut else_expr = None;

        let mut chain = vec![];

        let mut has_else = false;
        while kw!(bool Else, self) && {
            has_else = !kw!(bool If, self);
            !has_else
        } {
            let cond_expr =
                capture!(self, parse_expression).ok_or(ParseError::ExpectedCondition)?;
            kw!(bool Then, self);
            let then_expr =
                capture!(self, parse_expression).ok_or(ParseError::ExpectedThenExpression)?;

            chain.push((cond_expr, then_expr));
        }

        if has_else {
            else_expr =
                Some(box capture!(self, parse_expression).ok_or(ParseError::ExpectedExpression)?);
        }

        Ok(Expression::If {
            cond_expr,
            then_expr,
            elseif_exprs: chain,
            else_expr,
        })
    }

    pub fn parse_while_until(&mut self) -> ParseResult<Expression> {
        let loop_type = if kw!(bool While, self) {
            LoopType::While
        } else if kw!(bool Until, self) {
            LoopType::Until
        } else {
            return Err(ParseError::Continue);
        };

        let cond_expr =
            box capture!(self, parse_expression).ok_or(ParseError::ExpectedCondition)?;
        let body =
            box capture!(self, parse_expression).ok_or(ParseError::ExpectedThenExpression)?;

        Ok(Expression::Loop {
            loop_type,
            cond_expr,
            body,
        })
    }
}
