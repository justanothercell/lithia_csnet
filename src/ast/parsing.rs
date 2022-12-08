use std::collections::HashMap;
use crate::ast::ast::{Ident, Module};
use crate::ast::pattern_builder::build_patterns;
use crate::source::{ParseError};
use crate::tokens::tok_iter::TokIter;
use crate::tokens::tokens::{Token};
use crate::ast::patterns::{ConsumablePattern};

pub(crate) fn parse_tokens(tokens: Vec<Token>, mod_name: &str) -> Result<Module, ParseError>{
    let mut iter = TokIter::new(tokens);
    let mut start = iter.this()?.loc;
    let patterns = build_patterns();
    let (functions, _span) = patterns.module_content.consume(&mut iter)?;
    let end = iter.nearest_point()?;
    start.extend(end.end());
    let main = Module {
        name: Ident(mod_name.to_string(), start.clone()),
        sub_modules: Default::default(),
        functions: {
            let mut map = HashMap::new();
            for func in functions {
                map.insert(func.name.0.clone(), func);
            }
            map
        },
        loc: start,
    };
    return Ok(main);
}
