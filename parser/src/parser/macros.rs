#[macro_export]
macro_rules! peek {
    ($tt: expr, $ind: expr) => {{
        if ($ind) >= $tt.len() {
            None
        } else {
            Some(&$tt[$ind])
        }
    }};
}

// Used to clone a token reference for an Option for parser errors. May not be needed.
#[macro_export]
macro_rules! deref_opt {
    ($opt: expr) => {
        $opt.map(|t| t.clone())
    };
}

#[macro_export]
macro_rules! capture {
    ($self:ident, $opt:ident) => {{
      let prev_ind = $self.index;
      match $self.$opt() {
        None => {
          $self.index = prev_ind;
          None
        }
        capt => capt,
      }
    }};
    (res $opt:ident, $self:ident) => {{
      let prev_ind = $self.index;
      match $self.$opt() {
        Err(e) => {
          $self.index = prev_ind;
          Err(e)
        }
        capt => capt,
      }
    }};
    ($self:ident, $opt:ident, $($opts:ident),*) => {{
      match capture!($self, $opt) {
        None => capture!($self, $($opts),*),
        Some(ast) => Some(ast),
      }
    }};
    ($self:ident err $err:ident, $opt:ident, $($opts:ident),*) => {{
      match capture!(res $self, $opt) {
        Err(_) => capture!(res $self, $($opts),*),
        Ok(ast) => Ok(ast),
      }
    }};
    (expect $opt:ident, err $err:ident, $self:ident) => {{
      match capture!($self, $opt) {
        None => return Err(ParseError::$err),
        Some(res) => res,
      }
    }};
    (box $opt:ident, err $err:ident, $self:ident) => {{
      match capture!($self, $opt) {
        None => return Err(ParseError::$err),
        Some(res) => Some(box res),
      }
    }};
    (result $opt:ident, err $err:ident, $self:ident) => {{
      match capture!($self, $opt) {
        None => Err(ParseError::$err),
        Some(res) => Ok(box res),
      }
    }};
    (required $opt:ident, $self:ident) => {{
      match capture!($self, $opt) {
        None => return None,
        Some(res) => res,
      }
    }};
  }

#[macro_export]
macro_rules! kw {
    ($kw:ident, $self:ident) => {{
        let prev_ind = $self.index;
        let token = $self.next();
        if let Some(tok) = token {
            match tok.kw() {
                Some(kw) if kw == Keyword::$kw => {}
                _ => {
                    $self.index = prev_ind;
                    return Err(ParseError::UnexpectedToken);
                }
            }
        }
    }};
    (bool $kw:ident, $self:ident) => {{
        let prev_ind = $self.index;
        let token = $self.next();
        if let Some(tok) = token {
            match tok.kw() {
                Some(kw) if kw == Keyword::$kw => true,
                _ => {
                    $self.index = prev_ind;
                    false
                }
            }
        } else {
            $self.index = prev_ind;
            false
        }
    }};
}

#[macro_export]
macro_rules! ident {
    (required $self:ident) => {{
        let prev_ind = $self.index;
        let token = $self.next();
        if let Some(tok) = token {
            match tok.ident() {
                Some(id) => id,
                _ => {
                    $self.index = prev_ind;
                    return Err(ParseError::ExpectedIdent);
                }
            }
        } else {
            $self.index = prev_ind;
            return Err(ParseError::ExpectedIdent);
        }
    }};
    (opt $self:ident) => {{
        let prev_ind = $self.index;
        let token = $self.next();
        if let Some(tok) = token {
            match tok.ident() {
                Some(id) => id,
                _ => {
                    $self.index = prev_ind;
                    return None;
                }
            }
        } else {
            $self.index = prev_ind;
            return None;
        }
    }};
    (res $self:ident) => {{
        let prev_ind = $self.index;
        let token = $self.next();
        if let Some(tok) = token {
            match tok.ident() {
                Some(id) => Ok(id),
                _ => {
                    $self.index = prev_ind;
                    Err(ParseError::ExpectedIdent)
                }
            }
        } else {
            $self.index = prev_ind;
            Err(ParseError::ExpectedIdent)
        }
    }};
    ($self:ident) => {{
        let prev_ind = $self.index;
        let token = $self.next();
        if let Some(tok) = token {
            tok.ident()
        } else {
            $self.index = prev_ind;
            None
        }
    }};
}

#[macro_export]
macro_rules! sym {
    (required $sym:ident, $self:ident) => {{
        let prev_ind = $self.index;
        let token = $self.next();
        if let Some(tok) = token {
            match tok.sym() {
                Some(kw) if kw == Symbol::$sym => {}
                _ => {
                    $self.index = prev_ind;
                    return Err(ParseError::UnexpectedToken);
                }
            }
        }
    }};
    ($sym:ident, $self:ident) => {{
        let prev_ind = $self.index;
        let token = $self.next();
        if let Some(tok) = token {
            match tok.sym() {
                Some(kw) if kw == Symbol::$sym => {}
                _ => {
                    $self.index = prev_ind;
                    return None;
                }
            }
        }
    }};
    (op $self:ident) => {{
        let prev_ind = $self.index;
        let token = $self.next();
        if let Some(tok) = token {
            match tok.op() {
                Some(kw) => Some(kw),
                _ => {
                    $self.index = prev_ind;
                    None
                }
            }
        } else {
            $self.index = prev_ind;
            None
        }
    }};
    (bool $sym:ident, $self:ident) => {{
        let prev_ind = $self.index;
        let token = $self.next();
        if let Some(tok) = token {
            match tok.sym() {
                Some(sym) if sym == Symbol::$sym => true,
                _ => {
                    $self.index = prev_ind;
                    false
                }
            }
        } else {
            $self.index = prev_ind;
            false
        }
    }};
}
