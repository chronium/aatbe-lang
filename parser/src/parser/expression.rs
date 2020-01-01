use crate::{
    ast::{AtomKind, Boolean, Expression},
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
                return Some(AtomKind::Integer(val));
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

    fn parse_atom(&mut self) -> ParseResult<AtomKind> {
        match capture!(
            self,
            parse_boolean,
            parse_number,
            parse_unit,
            parse_string_lit,
            parse_atomized_expression
        ) {
            None => Err(ParseError::ExpectedAtom),
            Some(atom) => Ok(atom),
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
        let mut left = Expression::Atom(capture!(res parse_atom, self)?);

        while binds_tighter(self.peek(), rbp) {
            left = self.parse_rhs(left)?;
        }

        Ok(left)
    }

    fn parse_funcall(&mut self) -> ParseResult<Expression> {
        let name = ident!(required self);
        let mut args = vec![];

        loop {
            match capture!(res parse_atom, self) {
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
            ext_mut: var,
        })
    }

    fn parse_assign(&mut self) -> ParseResult<Expression> {
        let name = ident!(required self);
        sym!(required Assign, self);

        let value = box capture!(self, parse_expression).ok_or(ParseError::ExpectedExpression)?;

        Ok(Expression::Assign { name, value })
    }

    pub fn parse_expression(&mut self) -> Option<Expression> {
        match self.peek_symbol(Symbol::LCurly) {
            Some(false) => {
                let prev_ind = self.index;
                match self.parse_expr(0) {
                    Err(_) => {
                        self.index = prev_ind;
                        capture!(res parse_decl, self)
                            .or_else(|_| capture!(res parse_funcall, self))
                            .or_else(|_| capture!(res parse_assign, self))
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
                            None => panic!("Broken Expression"),
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
