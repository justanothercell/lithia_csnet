use crate::ast::code_printer::CodePrinter;
use crate::ast::pattern_builder::build_patterns;
use crate::source::{ParseError};
use crate::tokens::tok_iter::TokIter;
use crate::tokens::tokens::{Token};
use crate::ast::patterns::{ConsumablePattern};

pub(crate) fn parse_tokens(tokens: Vec<Token>) -> Result<(), ParseError>{
    let mut iter = TokIter::new(tokens);
    println!("{:?}", iter);
    let patterns = build_patterns();
    let (functions, _span) = patterns.module_content.consume(&mut iter)?;

    for func in functions {
        println!("{}", func.print())
    }

    return Ok(());
}
