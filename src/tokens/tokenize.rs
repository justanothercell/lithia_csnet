use crate::source::{ParseError, Source, SourceIter, Span};
use crate::tokens::tokens::{Literal, Token, TokenType};

pub(crate) fn tokenize(source: Source) -> Result<Vec<Token>, ParseError>{
    let mut iter = SourceIter::new(source);
    let mut tokens = vec![];
    while iter.left() > 0 {
        match iter.this()? {
            '"' => {
                let (string, span) = collect_until(&mut iter, true, true,
                                                   |c| c != '"')?;
                tokens.push(TokenType::Literal(Literal::String(string)).at(span));
            }
            c if c.is_whitespace() => {
                // pass
            }
            c if c.is_ascii_alphabetic() || c == '_' => {
                let (ident, span) = collect_until(&mut iter, false, false,
                                                  |c| c.is_ascii_alphanumeric() || c == '_')?;
                tokens.push(TokenType::Ident(ident).at(span));
            }
            c if c.is_ascii_digit() => {

            }
            c => tokens.push(TokenType::Particle(c).at(iter.here().span()))
        }
        iter.next();
    }
    Ok(tokens)
}

fn collect_until(iter: &mut SourceIter, skip_first: bool, consume_break: bool, cond: fn(char) -> bool) -> Result<(String, Span), ParseError>{
    let start = iter.here();
    let mut result = String::new();
    if skip_first {
        iter.next();
    }
    while cond(iter.this()?) {
        result.push(iter.this()?);
        iter.next();
    }
    if consume_break {
        iter.next();
    }
    iter.index -= 1;
    Ok((result, Span::from_points(start, iter.here())))
}