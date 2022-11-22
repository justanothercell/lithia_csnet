use std::fmt::{Debug, Display, Formatter};
use crate::source::Span;

#[derive(Clone, PartialEq)]
pub(crate) struct Token {
    pub(crate) tt: TokenType,
    pub(crate) loc: Span
}

impl Debug for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Token {{ tt: {:?}, loc: {}..{} }}", self.tt, self.loc.start, self.loc.end)
    }
}


#[derive(Debug, Clone, PartialEq)]
pub(crate) enum TokenType {
    Particle(char),
    Ident(String),
    Literal(Literal)
}

impl TokenType {
    pub(crate) fn at(self, loc: Span) -> Token{
        Token { tt: self, loc }
    }
}

#[derive(Clone, PartialEq)]
pub(crate) enum Literal {
    String(String),
    Char(char),
    Integer(u64),
    Float(f64),
}

impl Debug for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Literal::String(s) => format!("String(\"{s}\")"),
            Literal::Char(c) => format!("Char('{c}')"),
            Literal::Integer(i) => format!("Integer({i})"),
            Literal::Float(f) => format!("Float({f})"),
        })
    }
}