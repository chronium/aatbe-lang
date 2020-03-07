use crate::{
    ast::{AtomKind, Boolean, FloatSize, IntSize, PrimitiveType},
    parser::{ParseError, ParseResult, Parser},
    token,
    token::{Keyword, Symbol, Type},
};

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
        fn parse_number_type(parser: &mut Parser) -> Option<PrimitiveType> {
            let prev_ind = parser.index;
            let tok = parser.next();

            if let Some(tok) = tok {
                match tok.ty() {
                    Some(Type::I8) => Some(PrimitiveType::Int(IntSize::Bits8)),
                    Some(Type::I16) => Some(PrimitiveType::Int(IntSize::Bits16)),
                    Some(Type::I32) => Some(PrimitiveType::Int(IntSize::Bits32)),
                    Some(Type::I64) => Some(PrimitiveType::Int(IntSize::Bits64)),
                    Some(Type::U8) => Some(PrimitiveType::UInt(IntSize::Bits8)),
                    Some(Type::U16) => Some(PrimitiveType::UInt(IntSize::Bits16)),
                    Some(Type::U32) => Some(PrimitiveType::UInt(IntSize::Bits32)),
                    Some(Type::U64) => Some(PrimitiveType::UInt(IntSize::Bits64)),
                    Some(Type::F32) => Some(PrimitiveType::Float(FloatSize::Bits32)),
                    Some(Type::F64) => Some(PrimitiveType::Float(FloatSize::Bits64)),
                    _ => {
                        parser.index = prev_ind;
                        None
                    }
                }
            } else {
                parser.index = prev_ind;
                None
            }
        }

        let token = self.next();
        if let Some(tok) = token {
            if let Some(val) = tok.int() {
                return Some(match parse_number_type(self) {
                    Some(ty @ (PrimitiveType::Int(_) | PrimitiveType::UInt(_))) => {
                        AtomKind::Integer(val, ty)
                    }
                    Some(ty @ PrimitiveType::Float(_)) => AtomKind::Floating(val as f64, ty),
                    None => AtomKind::Integer(val, PrimitiveType::Int(IntSize::Bits32)),
                    _ => unreachable!(),
                });
            }
        }

        None
    }

    fn parse_float(&mut self) -> Option<AtomKind> {
        fn parse_float_type(parser: &mut Parser) -> Option<PrimitiveType> {
            let prev_ind = parser.index;
            let tok = parser.next();

            if let Some(tok) = tok {
                match tok.ty() {
                    Some(Type::F32) => Some(PrimitiveType::Float(FloatSize::Bits32)),
                    Some(Type::F64) => Some(PrimitiveType::Float(FloatSize::Bits64)),
                    _ => {
                        parser.index = prev_ind;
                        None
                    }
                }
            } else {
                parser.index = prev_ind;
                None
            }
        }

        let token = self.next();
        if let Some(tok) = token {
            if let Some(val) = tok.float() {
                let ty = parse_float_type(self);
                return Some(AtomKind::Floating(
                    val,
                    ty.unwrap_or(PrimitiveType::Float(FloatSize::Bits32)),
                ));
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
        if !sym!(bool Dollar, self) {
            return None;
        }
        match capture!(self, parse_expression) {
            None => None,
            Some(e) => Some(AtomKind::Expr(box e)),
        }
    }

    fn parse_ident(&mut self) -> Option<AtomKind> {
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

    fn parse_symbol_literal(&mut self) -> Option<AtomKind> {
        sym!(Colon, self);
        let symbol = ident!(opt self);
        Some(AtomKind::SymbolLiteral(symbol))
    }

    fn parse_array(&mut self) -> Option<AtomKind> {
        sym!(LBracket, self);
        if sym!(bool RBracket, self) {
            return Some(AtomKind::Array(vec![]));
        }

        let mut values = vec![];
        match capture!(self, parse_expression) {
            None => return None,
            Some(e) => values.push(e),
        };

        loop {
            if !sym!(bool Comma, self) {
                break;
            }

            match capture!(self, parse_expression) {
                None => return None,
                Some(e) => values.push(e),
            };
        }
        sym!(RBracket, self);

        Some(AtomKind::Array(values))
    }

    fn parse_atom(&mut self) -> ParseResult<AtomKind> {
        match capture!(
            self,
            parse_ident,
            parse_boolean,
            parse_number,
            parse_float,
            parse_unit,
            parse_string_lit,
            parse_char_lit,
            parse_atomized_expression,
            parse_symbol_literal,
            parse_array
        ) {
            None => Err(ParseError::ExpectedAtom),
            Some(atom) => Ok(atom),
        }
    }

    pub fn parse_unary(&mut self) -> ParseResult<AtomKind> {
        let amp = sym!(bool Ampersand, self);
        let res = if sym!(bool Minus, self) {
            if self.sep() {
                Err(ParseError::Continue)
            } else {
                Ok(AtomKind::Unary(
                    String::from("-"),
                    box capture!(res parse_unary, self)?,
                ))
            }
        } else if sym!(bool Not, self) {
            if self.sep() {
                Err(ParseError::Continue)
            } else {
                Ok(AtomKind::Unary(
                    String::from("!"),
                    box capture!(res parse_unary, self)?,
                ))
            }
        } else if sym!(bool LParen, self) {
            let expr =
                box capture!(self, parse_expression).ok_or(ParseError::ExpectedExpression)?;
            sym!(required RParen, self);
            Ok(AtomKind::Parenthesized(expr))
        } else if sym!(bool Star, self) {
            if self.sep() {
                Err(ParseError::Continue)
            } else {
                Ok(AtomKind::Deref(box capture!(res parse_unary, self)?))
            }
        } else {
            capture!(res parse_atom, self)
        }
        .and_then(|res| {
            if amp {
                Ok(AtomKind::Ref(box res))
            } else {
                Ok(res)
            }
        });

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
}
