use crate::{
    ast::{AtomKind, Boolean, Expression},
    parser::{ParseError, ParseResult, Parser},
    token,
    token::{Symbol, Token},
};

fn prec(sym: Symbol) -> u32 {
    match sym {
        Symbol::Plus => 10,
        Symbol::Minus => 10,
        Symbol::Star => 20,
        Symbol::Slash => 20,
        Symbol::LCurly => 0,
        _ => panic!("No precedence on {:?}", sym),
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
        let arrow = sym!(bool Dollar, self);
        if !arrow {
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
        let mut left = Expression::Atom(self.parse_atom()?);

        while binds_tighter(self.peek(), rbp) {
            left = self.parse_rhs(left)?;
        }

        Ok(left)
    }

    fn parse_funcall(&mut self) -> ParseResult<Expression> {
        let name = self.peek_ident().ok_or(ParseError::ExpectedIdent)?;
        self.next();
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

    pub fn parse_expression(&mut self) -> Option<Expression> {
        match self.peek_symbol(Symbol::LCurly) {
            Some(false) => self
                .parse_expr(0)
                .or(capture!(res parse_funcall, self))
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
