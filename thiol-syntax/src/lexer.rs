use logos::Logos;

use crate::{FileId, FileLocation, Loc};

#[derive(Debug, Clone, PartialEq, Logos)]
pub enum TokenKind {
    //
    // Keywords
    //
    #[token("const")]
    Const,
    #[token("type")]
    Type,
    #[token("record")]
    Record,

    #[token("function")]
    Function,

    #[token("program")]
    Program,

    #[token("var")]
    Var,
    #[token("begin")]
    Begin,
    #[token("end")]
    End,

    #[token("returns")]
    Returns,

    #[token("input")]
    Input,
    #[token("output")]
    Output,

    #[regex(r"(\p{XID_Start}|_)(\p{XID_Continue}|')*", |lex| lex.slice().to_string())]
    Identifier(String),

    #[regex(r"[0-9][0-9_]*", |lex| parse_integer_literal(lex.slice()))]
    Integer(u128),

    #[regex(r"[0-9][0-9_]*\.[0-9_]*", |lex| parse_float_literal(lex.slice()))]
    Float(f64),

    //
    // operators
    //
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("=")]
    Equals,
    #[token("<>")]
    #[token("/=")]
    NotEquals,

    #[token("mod")]
    Mod,

    #[token(">")]
    GreaterThan,
    #[token(">=")]
    GreaterThanEqual,
    #[token("<")]
    LessThan,
    #[token("<=")]
    LessThanEqual,

    #[token(":=")]
    Becomes,

    //
    // Punctuation and stuff
    //
    #[token("(")]
    ParenOpen,
    #[token(")")]
    ParenClose,

    #[token("[")]
    BracketOpen,
    #[token("]")]
    BracketClose,

    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token(";")]
    SemiColon,

    #[token(".")]
    Dot,

    // Misc
    #[regex(r"//.*\n", logos::skip)]
    Comment,
    #[regex(r#"\s+"#, logos::skip)]
    Whitespace,
    #[error]
    Error,

    // Items
    Root,
    BinaryExpr,
    PrefixExpr,
}

fn parse_integer_literal(s: &str) -> Option<u128> {
    s.chars()
        .filter(|c| *c != '_')
        .collect::<String>()
        .parse()
        .ok()
}

fn parse_float_literal(s: &str) -> Option<f64> {
    s.chars()
        .filter(|c| *c != '_')
        .collect::<String>()
        .parse()
        .ok()
}

pub type Token = Loc<TokenKind>;

pub fn tokenise(file_id: FileId, input: &'_ str) -> impl Iterator<Item = Token> + '_ {
    TokenKind::lexer(input)
        .spanned()
        .map(move |(kind, span)| Token {
            value: kind,
            loc: FileLocation {
                file: file_id,
                start: span.start,
                end: span.end,
            },
        })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check(input: &str, kind: TokenKind) {
        let mut lexer = tokenise(0, input);
        let tok = lexer.next().unwrap();
        assert_eq!(tok.value, kind);
    }

    #[test]
    fn lex_identifiers() {
        check("hello", TokenKind::Identifier("hello".into()));
        check("_test", TokenKind::Identifier("_test".into()));
        check("x'", TokenKind::Identifier("x'".into()));
        check("klöße42", TokenKind::Identifier("klöße42".into()));
        check(
            "TEST_CONSTANT",
            TokenKind::Identifier("TEST_CONSTANT".into()),
        );
    }

    #[test]
    fn lex_numbers() {
        check("0", TokenKind::Integer(0));
        check("1_234", TokenKind::Integer(1_234));

        check("1.", TokenKind::Float(1.0));
        #[allow(clippy::approx_constant)]
        check("3.1415", TokenKind::Float(3.1415));
    }
}
