use crate::{
    ast::{AtomKind, BindType, Expression, IdentPath, LValue},
    parser::{ParseError, ParseResult, Parser},
    token::{Keyword, Symbol, Token},
};

fn prec(symbol: Symbol) -> u32 {
    match symbol {
        Symbol::Or => 10,
        Symbol::And => 15,
        Symbol::Pipe => 20,
        Symbol::Xor => 25,
        Symbol::Ampersand => 30,
        Symbol::Equal | Symbol::NotEqual => 40,
        Symbol::Lower | Symbol::LowerEqual | Symbol::Greater | Symbol::GreaterEqual => 45,
        Symbol::Plus | Symbol::Minus => 50,
        Symbol::Star | Symbol::Slash | Symbol::Modulo => 60,
        _ => panic!("Not an op {:?}", symbol),
    }
}

fn binds_tighter(left: Option<&Token>, right: u32) -> bool {
    left.map_or(None, |op| op.op())
        .map_or(false, |op| prec(op) > right)
}

impl Parser {
    fn parse_rhs(&mut self, lhs: Expression) -> ParseResult<Expression> {
        let op = sym!(op self);
        match op {
            None => Err(ParseError::ExpectedOperator),
            Some(op) => {
                let rhs = self.parse_expr(prec(op))?;
                Ok(Expression::Binary(box lhs, op.into(), box rhs))
            }
        }
    }

    fn parse_expr(&mut self, rbp: u32) -> ParseResult<Expression> {
        let mut left = Expression::Atom(capture!(res parse_unary, self)?);
        if self.nl() {
            return Ok(left);
        }

        while binds_tighter(self.peek(), rbp) {
            left = self.parse_rhs(left)?;
        }

        Ok(left)
    }

    fn parse_funcall(&mut self) -> ParseResult<Expression> {
        let name = ident!(required self);

        let types = if sym!(bool LBracket, self) {
            let types = self.parse_type_list()?;
            sym!(required RBracket, self);
            types
        } else {
            vec![]
        };

        if self.nl()
            || sym!(bool Comma, self)
            || !(self.sep() || self.peek_symbol(Symbol::Unit).unwrap_or(false))
        {
            return Err(ParseError::Continue);
        }
        let mut args = vec![];

        match capture!(self, parse_expression) {
            Some(expr) => args.push(expr),
            None => {}
        };

        loop {
            if !sym!(bool Comma, self) {
                break;
            }
            match capture!(self, parse_expression) {
                Some(expr) => args.push(expr),
                None => break,
            }
        }

        if args.len() < 1 {
            Err(ParseError::ExpectedExpression)
        } else {
            Ok(Expression::Call {
                name: IdentPath::Local(name),
                types,
                args,
            })
        }
    }

    fn parse_decl(&mut self) -> ParseResult<Expression> {
        let val = kw!(bool Val, self);
        let var = kw!(bool Var, self);

        if !(val || var) {
            return Err(ParseError::ExpectedExpression);
        }

        let ty = capture!(res parse_named_type, self)?;

        let value = match sym!(bool Assign, self) {
            false => None,
            true => {
                Some(box capture!(self, parse_expression).ok_or(ParseError::ExpectedExpression)?)
            }
        };

        Ok(Expression::Decl {
            ty,
            value,
            exterior_bind: match var {
                true => BindType::Mutable,
                false => BindType::Immutable,
            },
        })
    }

    fn parse_lval_ident(&mut self) -> Option<LValue> {
        ident!(raw self).map(|i| {
            i.map(|id| match id.split_accessor() {
                Some(parts) if parts.len() == 1 => LValue::Ident(parts[0].clone()),
                Some(parts) => LValue::Accessor(parts),
                None => unreachable!(),
            })
            .unwrap()
        })
    }

    fn parse_lval_deref(&mut self) -> Option<LValue> {
        sym!(Star, self);

        match capture!(res parse_lvalue, self) {
            Err(_) => None,
            Ok(lval) => Some(LValue::Deref(box lval)),
        }
    }

    fn parse_lvalue(&mut self) -> ParseResult<LValue> {
        let lval =
            capture!(self, parse_lval_ident, parse_lval_deref).ok_or(ParseError::ExpectedLValue);

        if sym!(bool LBracket, self) && lval.is_ok() {
            let index =
                box capture!(self, parse_expression).ok_or(ParseError::ExpectedExpression)?;
            sym!(required RBracket, self);
            Ok(LValue::Index(box lval.unwrap(), index))
        } else {
            lval
        }
    }

    fn parse_assign(&mut self) -> ParseResult<Expression> {
        let lval = capture!(res parse_lvalue, self)?;
        sym!(required Assign, self);

        let value = box capture!(self, parse_expression).ok_or(ParseError::ExpectedExpression)?;

        Ok(Expression::Assign { lval, value })
    }

    fn parse_named_value(&mut self) -> ParseResult<AtomKind> {
        let name = ident!(required self);
        sym!(required Colon, self);
        let val = box capture!(expect parse_expression, err ExpectedExpression, self);
        Ok(AtomKind::NamedValue { name, val })
    }

    fn parse_named_value_list(&mut self) -> ParseResult<Vec<AtomKind>> {
        let mut params = vec![];
        params.push(capture!(res parse_named_value, self).or(Err(ParseError::ExpectedType))?);
        loop {
            if !sym!(bool Comma, self) {
                break;
            }

            params.push(capture!(res parse_named_value, self).or(Err(ParseError::ExpectedType))?);
        }
        Ok(params)
    }

    fn parse_record_init(&mut self) -> ParseResult<Expression> {
        let record = ident!(res self)?;

        let types = if sym!(bool LBracket, self) {
            let types = self.parse_type_list()?;
            sym!(required RBracket, self);
            types
        } else {
            vec![]
        };

        sym!(required LCurly, self);
        let values = self
            .parse_named_value_list()
            .expect(format!("Expected a named argument list at {}", record).as_str());
        sym!(required RCurly, self);

        Ok(Expression::RecordInit {
            record,
            types,
            values,
        })
    }

    fn parse_ret(&mut self) -> ParseResult<Expression> {
        kw!(Ret, self);

        Ok(Expression::Ret(
            box capture!(expect parse_expression, err ExpectedExpression, self),
        ))
    }

    pub fn parse_expression(&mut self) -> Option<Expression> {
        match self.peek_symbol(Symbol::LCurly) {
            Some(false) => capture!(res parse_record_init, self)
                .or_else(|_| capture!(res parse_funcall, self))
                .or_else(|_| capture!(res parse_assign, self))
                .or_else(|_| capture!(res parse_decl, self))
                .or_else(|_| capture!(res parse_if_else, self))
                .or_else(|_| capture!(res parse_ret, self))
                .or_else(|_| capture!(res parse_while_until, self))
                .or_else(|_| self.parse_expr(0))
                .ok(),
            Some(true) => {
                self.next();
                let mut block = vec![];
                loop {
                    match self.peek_symbol(Symbol::RCurly) {
                        Some(true) => {
                            self.next();
                            break;
                        }
                        Some(false) => match capture!(self, parse_expression) {
                            Some(expr) => block.push(expr),
                            None => panic!("Broken Expression {:?}", self.peek()),
                        },
                        None => panic!("Expected RCurly"),
                    }
                }
                Some(Expression::Block(block))
            }
            None => panic!("Unexpected EOF"),
        }
    }
}
