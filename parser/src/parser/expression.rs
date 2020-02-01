use crate::{
    ast::{AtomKind, BindType, Boolean, Expression, IntSize, LValue, PrimitiveType},
    parser::{ParseError, ParseResult, Parser},
    token,
    token::{Keyword, Symbol, Token},
};

fn prec(symbol: Symbol) -> u32 {
    match symbol {
        Symbol::LogicalOr => 10,
        Symbol::LogicalAnd => 15,
        Symbol::Or => 20,
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
    fn parse_boolean(&mut self) -> Option<AtomKind> {
        let token = self.next();

        if let Some(tok) = token {
            match tok.bl() {
                Some(token::Boolean::True) => Some(AtomKind::Bool(Boolean::True)),
                Some(token::Boolean::False) => Some(AtomKind::Bool(Boolean::False)),
                _ => None,
            }
        } else {
            None
        }
    }

    fn parse_number(&mut self) -> Option<AtomKind> {
        let token = self.next();
        if let Some(tok) = token {
            if let Some(val) = tok.int() {
                return Some(AtomKind::Integer(val, PrimitiveType::Int(IntSize::Bits32)));
            }
        }

        None
    }

    fn parse_string_lit(&mut self) -> Option<AtomKind> {
        let token = self.next();
        if let Some(tok) = token {
            if let Some(val) = tok.st() {
                return Some(AtomKind::StringLiteral(val));
            }
        }
        None
    }

    fn parse_char_lit(&mut self) -> Option<AtomKind> {
        let token = self.next();
        if let Some(tok) = token {
            if let Some(val) = tok.ch() {
                return Some(AtomKind::CharLiteral(val));
            }
        }
        None
    }

    fn parse_unit(&mut self) -> Option<AtomKind> {
        let token = self.next();
        if let Some(tok) = token {
            if let Some(Symbol::Unit) = tok.sym() {
                return Some(AtomKind::Unit);
            }
        }

        None
    }

    fn parse_atomized_expression(&mut self) -> Option<AtomKind> {
        let dlr = sym!(bool Dollar, self);
        if !dlr {
            return None;
        }
        match capture!(self, parse_expression) {
            None => None,
            Some(e) => Some(AtomKind::Expr(box e)),
        }
    }

    fn parse_ident(&mut self) -> Option<AtomKind> {
        if capture!(res parse_funcall, self)
            .or_else(|_| capture!(res parse_assign, self))
            .or_else(|_| capture!(res parse_record_init, self))
            .is_ok()
        {
            None
        } else {
            ident!(res raw self)
                .map(|i| {
                    i.map(|id| match id.split_accessor() {
                        Some(parts) if parts.len() == 1 => AtomKind::Ident(parts[0].clone()),
                        Some(parts) => AtomKind::Access(parts),
                        None => unreachable!(),
                    })
                    .unwrap()
                })
                .ok()
        }
    }

    fn parse_symbol_literal(&mut self) -> Option<AtomKind> {
        sym!(Colon, self);
        let symbol = ident!(opt self);
        Some(AtomKind::SymbolLiteral(symbol))
    }

    fn parse_atom(&mut self) -> ParseResult<AtomKind> {
        match capture!(
            self,
            parse_boolean,
            parse_number,
            parse_unit,
            parse_string_lit,
            parse_char_lit,
            parse_atomized_expression,
            parse_symbol_literal,
            parse_ident
        ) {
            None => Err(ParseError::ExpectedAtom),
            Some(atom) => Ok(atom),
        }
    }

    fn parse_unary(&mut self) -> ParseResult<AtomKind> {
        let res = if sym!(bool Minus, self) {
            Ok(AtomKind::Unary(
                String::from("-"),
                box capture!(res parse_unary, self)?,
            ))
        } else if sym!(bool Not, self) {
            Ok(AtomKind::Unary(
                String::from("!"),
                box capture!(res parse_unary, self)?,
            ))
        } else if sym!(bool LParen, self) {
            let expr =
                box capture!(self, parse_expression).ok_or(ParseError::ExpectedExpression)?;
            sym!(required RParen, self);
            Ok(AtomKind::Parenthesized(expr))
        } else if sym!(bool Star, self) {
            Ok(AtomKind::Deref(box capture!(res parse_unary, self)?))
        } else {
            capture!(res parse_atom, self)
        };

        let index = if sym!(bool LBracket, self) && res.is_ok() {
            let index =
                box capture!(self, parse_expression).ok_or(ParseError::ExpectedExpression)?;
            sym!(required RBracket, self);
            Ok(AtomKind::Index(box res.unwrap(), index))
        } else {
            res
        };

        if kw!(bool As, self) && index.is_ok() {
            let ty = capture!(res parse_type, self)?;

            Ok(AtomKind::Cast(box index.unwrap(), ty))
        } else {
            index
        }
    }

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

        while binds_tighter(self.peek(), rbp) {
            left = self.parse_rhs(left)?;
        }

        Ok(left)
    }

    fn parse_funcall(&mut self) -> ParseResult<Expression> {
        let name = ident!(required self);
        let mut args = vec![];

        match capture!(res parse_unary, self) {
            Ok(expr) => args.push(expr),
            Err(e) => return Err(e),
        };

        loop {
            if !sym!(bool Comma, self) {
                break;
            }
            match capture!(res parse_unary, self) {
                Ok(expr) => args.push(expr),
                Err(_) => break,
            }
        }

        if args.len() < 1 {
            Err(ParseError::NotEnoughArguments(name))
        } else {
            Ok(Expression::Call { name, args })
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

    fn parse_named_value_list(&mut self, terminator: Symbol) -> ParseResult<Vec<AtomKind>> {
        let mut params = vec![];
        loop {
            match self.peek_symbol(terminator) {
                Some(true) => break,
                Some(false) => {
                    if params.len() > 0 {
                        sym!(required Comma, self);
                    }
                    let ty =
                        capture!(res parse_named_value, self).or(Err(ParseError::ExpectedType))?;
                    params.push(ty);
                }
                None => return Err(ParseError::UnexpectedEOF),
            }
        }
        self.next();
        Ok(params)
    }

    fn parse_record_init(&mut self) -> ParseResult<Expression> {
        let record = ident!(res self)?;
        sym!(required LCurly, self);
        let values = self
            .parse_named_value_list(Symbol::RCurly)
            .expect(format!("Expected a named argument list at {}", record).as_str());
        Ok(Expression::RecordInit { record, values })
    }

    pub fn parse_expression(&mut self) -> Option<Expression> {
        match self.peek_symbol(Symbol::LCurly) {
            Some(false) => {
                let prev_ind = self.index;
                match self.parse_expr(0) {
                    Err(_) => {
                        self.index = prev_ind;

                        capture!(res parse_funcall, self)
                            .or_else(|_| capture!(res parse_record_init, self))
                            .or_else(|_| capture!(res parse_assign, self))
                            .or_else(|_| capture!(res parse_decl, self))
                            .or_else(|_| capture!(res parse_if_else, self))
                            .ok()
                    }
                    Ok(val) => Some(val),
                }
            }
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
