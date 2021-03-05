use logos::Logos;

use crate::{ast::VecSize, FileId, FileLocation, Loc};

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

    #[token("array")]
    Array,

    #[token("of")]
    Of,
    #[token("in")]
    In,
    #[token("is")]
    Is,
    #[token("from")]
    From,
    #[token("to")]
    To,
    #[token("as")]
    As,
    #[token("downto")]
    DownTo,

    #[token("Point")]
    Point,
    #[token("Vector")]
    Vector,
    #[token("Colour")]
    Colour,

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

    #[token("return")]
    Return,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,

    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("else")]
    Else,
    #[token("elseif")]
    ElseIf,

    #[token("for")]
    For,
    #[token("do")]
    Do,

    #[token("returns")]
    Returns,

    #[token("input")]
    Input,
    #[token("output")]
    Output,

    // primitive types
    #[token("bool")]
    TyBool,
    #[token("int")]
    TyInt,
    #[token("uint")]
    TyUInt,
    #[token("float")]
    TyFloat,
    #[token("double")]
    TyDouble,

    #[token("bool2", |_| VecSize::VS2)]
    #[token("bool3", |_| VecSize::VS3)]
    #[token("bool4", |_| VecSize::VS4)]
    TyBoolVec(VecSize),

    #[token("int2", |_| VecSize::VS2)]
    #[token("int3", |_| VecSize::VS3)]
    #[token("int4", |_| VecSize::VS4)]
    TyIntVec(VecSize),

    #[token("uint2", |_| VecSize::VS2)]
    #[token("uint3", |_| VecSize::VS3)]
    #[token("uint4", |_| VecSize::VS4)]
    TyUIntVec(VecSize),

    #[token("float2", |_| VecSize::VS2)]
    #[token("float3", |_| VecSize::VS3)]
    #[token("float4", |_| VecSize::VS4)]
    TyFloatVec(VecSize),

    #[token("double2", |_| VecSize::VS2)]
    #[token("double3", |_| VecSize::VS3)]
    #[token("double4", |_| VecSize::VS4)]
    TyDoubleVec(VecSize),

    #[token("float2x2", |_| { (VecSize::VS2, VecSize::VS2) })]
    #[token("float2x3", |_| { (VecSize::VS2, VecSize::VS3) })]
    #[token("float2x4", |_| { (VecSize::VS2, VecSize::VS4) })]
    #[token("float3x2", |_| { (VecSize::VS3, VecSize::VS2) })]
    #[token("float3x3", |_| { (VecSize::VS3, VecSize::VS3) })]
    #[token("float3x4", |_| { (VecSize::VS3, VecSize::VS4) })]
    #[token("float4x2", |_| { (VecSize::VS4, VecSize::VS2) })]
    #[token("float4x3", |_| { (VecSize::VS4, VecSize::VS3) })]
    #[token("float4x4", |_| { (VecSize::VS4, VecSize::VS4) })]
    TyFloatMat((VecSize, VecSize)),

    #[token("double2x2", |_| { (VecSize::VS2, VecSize::VS2) })]
    #[token("double2x3", |_| { (VecSize::VS2, VecSize::VS3) })]
    #[token("double2x4", |_| { (VecSize::VS2, VecSize::VS4) })]
    #[token("double3x2", |_| { (VecSize::VS3, VecSize::VS2) })]
    #[token("double3x3", |_| { (VecSize::VS3, VecSize::VS3) })]
    #[token("double3x4", |_| { (VecSize::VS3, VecSize::VS4) })]
    #[token("double4x2", |_| { (VecSize::VS4, VecSize::VS2) })]
    #[token("double4x3", |_| { (VecSize::VS4, VecSize::VS3) })]
    #[token("double4x4", |_| { (VecSize::VS4, VecSize::VS4) })]
    TyDoubleMat((VecSize, VecSize)),

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
