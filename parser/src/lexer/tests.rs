#[cfg(test)]
mod lexer_tests {
    use crate::lexer::{
        token::{Boolean, Keyword, Type},
        Lexer, Symbol, TokenKind,
    };
    #[test]
    fn end_of_file() {
        let mut lexer = Lexer::new("");
        let mut tokens = lexer.lex().into_iter();

        assert_eq!(tokens.next().unwrap().kind, TokenKind::EOF);
    }

    macro_rules! sep {
        ($tokens: ident) => {
            assert_eq!($tokens.next().unwrap().kind, TokenKind::SEP);
        };
    }

    #[test]
    fn symbols() {
        let mut lexer = Lexer::new(
            " @ ( ) -> { } () = + - * / & $ , : ! == != > >= < <= | || && ^ % . .. ... [ ]",
        );
        let mut tokens = lexer.lex().into_iter();

        let expected = vec![
            Symbol::At,
            Symbol::LParen,
            Symbol::RParen,
            Symbol::Arrow,
            Symbol::LCurly,
            Symbol::RCurly,
            Symbol::Unit,
            Symbol::Assign,
            Symbol::Plus,
            Symbol::Minus,
            Symbol::Star,
            Symbol::Slash,
            Symbol::Ampersand,
            Symbol::Dollar,
            Symbol::Comma,
            Symbol::Colon,
            Symbol::Not,
            Symbol::Equal,
            Symbol::NotEqual,
            Symbol::Greater,
            Symbol::GreaterEqual,
            Symbol::Lower,
            Symbol::LowerEqual,
            Symbol::Pipe,
            Symbol::Or,
            Symbol::And,
            Symbol::Xor,
            Symbol::Modulo,
            Symbol::Dot,
            Symbol::DoDot,
            Symbol::GoDot,
            Symbol::LBracket,
            Symbol::RBracket,
        ]
        .into_iter()
        .map(|t| Some(t));

        for t in expected {
            sep!(tokens);
            assert_eq!(tokens.next().unwrap().sym(), t);
        }
    }

    #[test]
    fn single_line_comment() {
        let mut lexer = Lexer::new("//Test");
        let mut tokens = lexer.lex().into_iter();

        assert_eq!(
            tokens.next().unwrap().kind,
            TokenKind::Comment(String::from("Test"))
        );
    }

    #[test]
    fn single_line_comment_with_whitespace() {
        let mut lexer = Lexer::new("                 //Test");
        let mut tokens = lexer.lex().into_iter();

        sep!(tokens);
        assert_eq!(
            tokens.next().unwrap().kind,
            TokenKind::Comment(String::from("Test"))
        );
    }

    #[test]
    fn single_number_literal() {
        let mut lexer = Lexer::new("18_446_744_073_709_551_614");
        let mut tokens = lexer.lex().into_iter();

        assert_eq!(
            tokens.next().unwrap().kind,
            TokenKind::IntLiteral(18_446_744_073_709_551_614u64),
        )
    }

    #[test]
    fn single_number_literal_zero() {
        let mut lexer = Lexer::new("0123456789");
        let mut tokens = lexer.lex().into_iter();

        assert_eq!(
            tokens.next().unwrap().kind,
            TokenKind::IntLiteral(0123456789),
        )
    }

    #[test]
    fn single_number_literal_hex() {
        let mut lexer = Lexer::new("0xdeadbeef");
        let mut tokens = lexer.lex().into_iter();

        assert_eq!(
            tokens.next().unwrap().kind,
            TokenKind::IntLiteral(0xdeadbeef),
        )
    }

    #[test]
    fn single_number_literal_hex_separator() {
        let mut lexer = Lexer::new("0xdead_beef");
        let mut tokens = lexer.lex().into_iter();

        assert_eq!(
            tokens.next().unwrap().kind,
            TokenKind::IntLiteral(0xdeadbeef),
        )
    }

    #[test]
    fn string_literal_no_escape() {
        let mut lexer = Lexer::new("\"Hello World\n\"");
        let mut tokens = lexer.lex().into_iter();

        assert_eq!(
            tokens.next().unwrap().kind,
            TokenKind::StringLiteral(String::from("Hello World\n")),
        )
    }

    #[test]
    fn keyword_identifier() {
        let mut lexer = Lexer::new(
            "fn extern var val if else use true false main record.test bool rec global ret while until type is public module",
        );
        let mut tokens = lexer.lex().into_iter();

        assert_eq!(tokens.next().unwrap().kw(), Some(Keyword::Fn));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().kw(), Some(Keyword::Extern));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().kw(), Some(Keyword::Var));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().kw(), Some(Keyword::Val));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().kw(), Some(Keyword::If));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().kw(), Some(Keyword::Else));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().kw(), Some(Keyword::Use));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().bl(), Some(Boolean::True));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().bl(), Some(Boolean::False));
        sep!(tokens);
        assert_eq!(
            tokens.next().unwrap().kind,
            TokenKind::Identifier(String::from("main")),
        );
        sep!(tokens);
        assert_eq!(
            tokens.next().unwrap().kind,
            TokenKind::Identifier(String::from("record.test")),
        );
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().kw(), Some(Keyword::Bool));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().kw(), Some(Keyword::Record));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().kw(), Some(Keyword::Global));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().kw(), Some(Keyword::Ret));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().kw(), Some(Keyword::While));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().kw(), Some(Keyword::Until));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().kw(), Some(Keyword::Type));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().kw(), Some(Keyword::Is));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().kw(), Some(Keyword::Public));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().kw(), Some(Keyword::Module));
    }

    #[test]
    fn type_tests() {
        let mut lexer = Lexer::new("str i8 i16 i32 i64 u8 u16 u32 u64 f32 f64");
        let mut tokens = lexer.lex().into_iter();

        assert_eq!(tokens.next().unwrap().ty(), Some(Type::Str));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().ty(), Some(Type::I8));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().ty(), Some(Type::I16));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().ty(), Some(Type::I32));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().ty(), Some(Type::I64));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().ty(), Some(Type::U8));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().ty(), Some(Type::U16));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().ty(), Some(Type::U32));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().ty(), Some(Type::U64));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().ty(), Some(Type::F32));
        sep!(tokens);
        assert_eq!(tokens.next().unwrap().ty(), Some(Type::F64));
    }
}
