use crate::ast::ast::{Block, Expr, Expression, FullType, Func, Ident, Item, Statement, Stmt, Type, TypeT};
use crate::ast::code_printer::CodePrinter;
use crate::ast::consumers::{BranchIfElse, GetIdentConsumer, ListConsumer, ConditionalConsumer, PatternConsumer, TokenConsumer, Trail, GetLiteralConsumer, RefLoopPatternConsumer};
use crate::source::{ParseError, Span};
use crate::tokens::tok_iter::TokIter;
use crate::tokens::tokens::{Token, TokenType};
use crate::ast::patterns::{Pattern};

pub(crate) fn parse_tokens(tokens: Vec<Token>) -> Result<(), ParseError>{
    let mut iter = TokIter::new(tokens);
    println!("{:?}", iter);
    let item_pattern = Pattern::named("item", (ListConsumer::non_empty(
        Pattern::single(GetIdentConsumer,|(ident,), _| ident),
        Pattern::inline((
                         TokenConsumer(TokenType::Particle(':')),
                         TokenConsumer(TokenType::Particle(':'))
                     ),|_, _| ()),
        Trail::Never
    ),), |(path,), loc| Item(path, loc));

    let generics_pattern = Pattern::named("generics", (
        TokenConsumer(TokenType::Particle('<')),
        ListConsumer::maybe_empty_pred(
            ConditionalConsumer(Pattern::single(GetIdentConsumer, |(i,), _| i),
                Pattern::single(RefLoopPatternConsumer::<FullType>::create(), |(i,), _| i),
            |i, _|i),
            ConditionalConsumer(Pattern::single(TokenConsumer(TokenType::Particle(',')), |(i,), _| i),
                                Pattern::single(TokenConsumer(TokenType::Particle(',')), |(_,), _| ()),
                                |i, _|i),
            Trail::Optional
        ),
        TokenConsumer(TokenType::Particle('>')),
    ), |(_, generics, _), _| generics);
    let optional_generics_pattern = Pattern::named("optional generics", (
        ConditionalConsumer(Pattern::single(TokenConsumer(TokenType::Particle('<')), |_,_|()),
                            generics_pattern.clone(),
                            |g, _| g.unwrap_or(vec![])),
    ), |(g,), _| g
    );

    let type_pattern = Pattern::named("single type", (
        PatternConsumer(item_pattern.clone()),
        PatternConsumer(optional_generics_pattern.clone())
    ), |(path, generics), loc| Type { generics, base_type: path, loc });
    let l = ListConsumer::maybe_empty_pred(
        ConditionalConsumer(
        Pattern::single(GetIdentConsumer, |(i,), _| i),
        Pattern::single(GetIdentConsumer, |(i,), _| i),
        |i, _| i),
        ConditionalConsumer(
            Pattern::single(TokenConsumer(TokenType::Particle(',')),|_, _| ()),
            Pattern::single(TokenConsumer(TokenType::Particle(',')),|_, _| ()),
            |i, _|i),
        Trail::Never
    );
    let b = BranchIfElse(Pattern::single(TokenConsumer(TokenType::Particle('(')), |_, _|()),
                         Pattern::single(type_pattern.clone(), |(t,), loc|TypeT::Single(t))
                         Pattern::single(type_pattern.clone(), |(t,), loc|TypeT::Single(t))
    );
    let full_type_pattern = Pattern::named("type", (
        PatternConsumer(type_pattern.clone())
    ), |(ty,), loc| FullType(ty, loc));
    generics_pattern.consumers.1.0.1.consumers.0.finalize(Box::new(PatternConsumer(full_type_pattern.clone())));
    let ident_type_pair_pattern = Pattern::named("ident type pair", (
        GetIdentConsumer,
        TokenConsumer(TokenType::Particle(':')),
        PatternConsumer(full_type_pattern.clone()),
    ),|(ident, _, ty), _| (ident, ty));

    let fn_def_args_pattern = Pattern::named("function args", (
        ListConsumer::maybe_empty_pred(ConditionalConsumer(
            Pattern::single(GetIdentConsumer, |(i,), _| i),
            ident_type_pair_pattern.clone(),
            |i, _| i
        ),
        ConditionalConsumer(
            Pattern::single(TokenConsumer(TokenType::Particle(',')),|_, _| ()),
            Pattern::single(TokenConsumer(TokenType::Particle(',')),|_, _| ()),
            |i, _|i),
        Trail::Never
    ),), |(args,), _| args);

    let arrow_pattern = Pattern::inline((
        TokenConsumer(TokenType::Particle('-')),
        TokenConsumer(TokenType::Particle('>'))
    ), |_, _|());

    let expr_pattern = Pattern::named("expression", (
            GetLiteralConsumer,
        ), |(lit,), loc| Expression(Expr::Literal(lit), loc)
    );
    let args_pattern = Pattern::named("args", (ListConsumer::maybe_empty_pred(
        ConditionalConsumer(Pattern::single(PatternConsumer(expr_pattern.clone()), |(i,), _| i),
                            Pattern::single(PatternConsumer(expr_pattern.clone()), |(i,), _| i),
                            |i, _|i),
        ConditionalConsumer(Pattern::single(TokenConsumer(TokenType::Particle(',')), |(i,), _| i),
                            Pattern::single(TokenConsumer(TokenType::Particle(',')), |(i,), _| i),
                            |i, _|i),
        Trail::Never),
    ), |(args, ), _| args);
    let fn_call_pattern = Pattern::named("function call",(
            PatternConsumer(item_pattern.clone()),
            TokenConsumer(TokenType::Particle('(')),
            PatternConsumer(args_pattern.clone()),
            TokenConsumer(TokenType::Particle(')')),
        ), |(ident, _, args, _), _| Stmt::FuncCall(ident, args)
    );
    let var_creation_pattern = Pattern::named("variable declaration",(
        TokenConsumer(TokenType::Ident("let".to_string())),
        ConditionalConsumer(Pattern::single(TokenConsumer(TokenType::Ident("mut".to_string())), |_, _|()),
                            Pattern::single(TokenConsumer(TokenType::Ident("mut".to_string())), |_, _|()),
            |m, _| m.is_some()
        ),
        PatternConsumer(item_pattern.clone()),
        ConditionalConsumer(Pattern::single(TokenConsumer(TokenType::Particle(':')), |_, _|()),
            Pattern::named("variable type", (
                TokenConsumer(TokenType::Particle(':')),
                PatternConsumer(full_type_pattern.clone())
            ), |(_, ty), _| ty),
                            |ty, _| ty
        ),
        TokenConsumer(TokenType::Particle('=')),
        PatternConsumer(expr_pattern.clone())
    ), |(_, mutable, ident, ty, _, value), _|
        Stmt::VarCreate(ident, mutable, ty, value)
    );

    let statement_pattern = Pattern::named("statement",(
            BranchIfElse(Pattern::single(TokenConsumer(TokenType::Ident("let".to_string())), |_, _|()),
                         var_creation_pattern.clone(),
                         fn_call_pattern.clone(),
            ),
            TokenConsumer(TokenType::Particle(';'))
        ), |(stmt, _), loc| Statement(stmt, loc)
    );

    let block_pattern = Pattern::named("block", (
            ListConsumer::maybe_empty_pred(ConditionalConsumer(
                Pattern::single(GetIdentConsumer, |_, _|()),
                statement_pattern.clone(), |i, _|i),
                                           ConditionalConsumer(
                                               Pattern::dummy(),
                                               Pattern::dummy(), |i, _|i),
                                           Trail::Always),
        ), |(stmts,), loc| Block(stmts, loc)
    );

    let fn_pattern = Pattern::named("function", (
        TokenConsumer(TokenType::Ident("fn".to_string())),
        GetIdentConsumer,
        TokenConsumer(TokenType::Particle('(')),
        PatternConsumer(fn_def_args_pattern.clone()),
        TokenConsumer(TokenType::Particle(')')),
        ConditionalConsumer(arrow_pattern.clone(),
            Pattern::named("return type",(
                PatternConsumer(arrow_pattern.clone()),
                PatternConsumer(full_type_pattern)
            ), |(_, ty), _| ty ),
            |opt_ty, loc| opt_ty.unwrap_or(FullType(TypeT::empty(), loc))),
        TokenConsumer(TokenType::Particle('{')),
        PatternConsumer(block_pattern.clone()),
        TokenConsumer(TokenType::Particle('}')),
    ), |(_, ident, _, args, _, ret, _, body, _), loc| Func {
        name: ident,
        args,
        ret,
        body,
        loc
    });
    println!("{}", fn_pattern.consume(&mut iter)?.print());
    println!("{}", fn_pattern.consume(&mut iter)?.print());
    let main = fn_pattern.consume(&mut iter)?;
    println!("{}", main.print());
    //println!("\n{:#?}", main);
    Ok(())
}
