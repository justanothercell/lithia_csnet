use crate::ast::ast::{Block, Expr, Expression, FullType, Func, Item, Module, Op, Operator, Statement, Stmt, Type, TypeT};
use crate::ast::consumers::{BranchIfElse, ConditionalConsumer, CustomConsumer, GetIdentConsumer, GetLiteralConsumer, GetParticleConsumer, ListConsumer, MatchConsumer, PatternConsumer, RefLoopPatternConsumer, TokenConsumer, Trail};
use crate::ast::patterns::{MapConsumer, Pat, Pattern};
use crate::source::{ParseET, Span};
use crate::tokens::tokens::TokenType;

pub(crate) struct Patterns {
    pub(crate) module_content: Pat<(Vec<Func>, Span)>,
    pub(crate) function: Pat<Func>
}

pub(crate) fn build_patterns() -> Patterns{
    let item_pattern = Pattern::named("item", (ListConsumer::non_empty(
        Pattern::single(GetIdentConsumer,|(ident,), _| ident),
        Pattern::inline((
                            TokenConsumer(TokenType::Particle(':')),
                            TokenConsumer(TokenType::Particle(':'))
                        ),|_, _| ()),
        Trail::Never
    ),), |(path,), loc| Item(path, loc));
    let (ref_loop_type_for_generics, ref_loop_type_for_generics_finalizer) = RefLoopPatternConsumer::<FullType>::create();
    let generics_pattern = Pattern::named("generics", (
        TokenConsumer(TokenType::Particle('<')),
        ListConsumer::maybe_empty_pred(
            ConditionalConsumer(GetIdentConsumer.pat(),
                                Pattern::single(ref_loop_type_for_generics, |(i,), _| i)),
            ConditionalConsumer(Pattern::single(TokenConsumer(TokenType::Particle(',')), |(i,), _| i),
                                Pattern::single(TokenConsumer(TokenType::Particle(',')), |(_,), _| ())),
            Trail::Optional
        ),
        TokenConsumer(TokenType::Particle('>')),
    ), |(_, generics, _), _| generics);
    let optional_generics_pattern = Pattern::named("optional generics", (
        ConditionalConsumer(Pattern::single(TokenConsumer(TokenType::Particle('<')), |_,_|()),
                            generics_pattern.clone()),
    ), |(g,), _| g.unwrap_or(vec![])
    );

    let type_pattern = Pattern::named("single type", (
       item_pattern.clone().con(),
       optional_generics_pattern.clone().con()
    ), |(path, generics), loc| Type { generics, base_type: path, loc });
    let type_predicate = BranchIfElse(TokenConsumer(TokenType::Particle('(')).pat(),
                                                      TokenConsumer(TokenType::Particle('(')).pat(),
                                                      GetIdentConsumer.pat().check_ok()).pat();
    let (ref_loop_type_for_tuple, ref_loop_type_for_tuple_finalizer) = RefLoopPatternConsumer::<FullType>::create();
    let tuple_type_pattern = Pattern::inline((TokenConsumer(TokenType::Particle('(')),
                                              ListConsumer::maybe_empty_pred(
                                                  ConditionalConsumer(
                                                     type_predicate.clone().con().pat(),
                                                      ref_loop_type_for_tuple.pat(),
                                                  ),
                                                  ConditionalConsumer(
                                                      TokenConsumer(TokenType::Particle(',')).pat(),
                                                      TokenConsumer(TokenType::Particle(',')).pat()),
                                                  Trail::Never),
                                              TokenConsumer(TokenType::Particle(')'))
                                             ), |(_, t, _ ), loc|TypeT::Tuple(t));
    let full_type_pattern = Pattern::named("type", (
        BranchIfElse(TokenConsumer(TokenType::Particle('(')).pat(),
                     tuple_type_pattern.clone(),
                     Pattern::single(PatternConsumer(type_pattern.clone()), |(t,), loc|TypeT::Single(t))
        ),
    ), |(ty,), loc| FullType(ty, loc));
    ref_loop_type_for_generics_finalizer.finalize(full_type_pattern.clone());
    ref_loop_type_for_tuple_finalizer.finalize(full_type_pattern.clone());
    let ident_type_pair_pattern = Pattern::named("ident type pair", (
        GetIdentConsumer,
        TokenConsumer(TokenType::Particle(':')),
       full_type_pattern.clone().con(),
    ),|(ident, _, ty), _| (ident, ty));

    let fn_def_args_pattern = Pattern::named("function args", (
        ListConsumer::maybe_empty_pred(ConditionalConsumer(
            GetIdentConsumer.pat(),
            ident_type_pair_pattern.clone()
        ),
                                       ConditionalConsumer(
                                           TokenConsumer(TokenType::Particle(',')).pat(),
                                           TokenConsumer(TokenType::Particle(',')).pat()),
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
    let expr_predicate = CustomConsumer(|iter|{
        let tok = iter.this()?;
        iter.next();
        match tok.tt {
            TokenType::Particle('(') => Ok(()),
            TokenType::Particle('~') => Ok(()),
            TokenType::Particle('-') => Ok(()),
            TokenType::Particle('+') => Ok(()),
            TokenType::Ident(_) => Ok(()),
            TokenType::Literal(_) => Ok(()),
            _ => Err(ParseET::ParsingError(format!("expected Expression, found {:?}", tok.tt)).at(tok.loc))
        }
    }).pat();
    let args_pattern = Pattern::named("args", (ListConsumer::maybe_empty_pred(
        ConditionalConsumer(PatternConsumer(expr_predicate.clone()).pat(),
                            expr_pattern.clone()),
        ConditionalConsumer(TokenConsumer(TokenType::Particle(',')).pat(),
                            TokenConsumer(TokenType::Particle(',')).pat()),
        Trail::Never),
    ), |(args, ), _| args);
    let fn_call_pattern = Pattern::named("function call",(
       item_pattern.clone().con(),
        TokenConsumer(TokenType::Particle('(')),
       args_pattern.clone().con(),
        TokenConsumer(TokenType::Particle(')')),
    ), |(ident, _, args, _), _| Stmt::FuncCall(ident, args)
    );
    let var_creation_pattern = Pattern::named("variable declaration",(
        TokenConsumer(TokenType::Ident("let".to_string())),
        ConditionalConsumer(TokenConsumer(TokenType::Ident("mut".to_string())).pat(),
                            TokenConsumer(TokenType::Ident("mut".to_string())).pat()
        ),
       item_pattern.clone().con(),
        ConditionalConsumer(TokenConsumer(TokenType::Particle(':')).pat(),
                            Pattern::named("variable type", (
                                TokenConsumer(TokenType::Particle(':')),
                               full_type_pattern.clone().con()
                            ), |(_, ty), _| ty)
        ),
        TokenConsumer(TokenType::Particle('=')),
       expr_pattern.clone().con()
    ), |(_, mutable, ident, ty, _, value), _|
                                                  Stmt::VarCreate(ident, mutable.is_some(), ty, value)
    );

    let var_assign_pattern = Pattern::named("variable assignment",(
       item_pattern.clone().con(),
        ListConsumer::non_empty(GetParticleConsumer.pat(),
                                Pattern::dummy(),
                                Trail::Always)
            .mapper_failable(|(mut c,), loc|{
                let equals = c.pop().unwrap();
                if equals != '=' {
                    return Err(ParseET::ParsingError(format!("Expected '=', found '{equals}'")).at(loc))
                }
                if c.len() > 0 {
                    Ok(Some(Operator(Op::from_chars(c, Some(loc.clone()))?, loc)))
                } else { Ok(None)}
            }),
       expr_pattern.clone().con()
    ), |(ident, op, value), _loc| Stmt::VarAssign(ident, op,value)
    );

    let statement_pattern = Pattern::named("statement",(
        BranchIfElse(Pattern::single(TokenConsumer(TokenType::Ident("let".to_string())), |_, _|()),
                     var_creation_pattern.clone(),
                     Pattern::single(BranchIfElse(Pattern::inline((
                                                                      GetIdentConsumer,
                                                                      BranchIfElse(
                                                                          TokenConsumer(TokenType::Particle(':')).pat(),
                                                                          TokenConsumer(TokenType::Particle(':')).pat(),
                                                                          TokenConsumer(TokenType::Particle('(')).pat(),
                                                                      )
                                                                  ), |_, _|()),
                                                  fn_call_pattern.clone(),
                                                  var_assign_pattern.clone()
                     ),|(stmt,), loc| stmt),
        ),
        TokenConsumer(TokenType::Particle(';'))
    ), |(stmt, _), loc| Statement(stmt, loc)
    );

    let block_pattern = Pattern::named("block", (
        ListConsumer::maybe_empty_pred(ConditionalConsumer(
            GetIdentConsumer.pat(),
            statement_pattern.clone()),
                                       ConditionalConsumer(
                                           Pattern::dummy(),
                                           Pattern::dummy()),
                                       Trail::Always),
    ), |(stmts,), loc| Block(stmts, loc)
    );

    let fn_pattern = Pattern::named("function", (
        TokenConsumer(TokenType::Ident("fn".to_string())),
        GetIdentConsumer,
        TokenConsumer(TokenType::Particle('(')),
        fn_def_args_pattern.clone().con(),
        TokenConsumer(TokenType::Particle(')')),
        ConditionalConsumer(arrow_pattern.clone(),
                            Pattern::named("return type",(
                               arrow_pattern.clone().con(),
                               full_type_pattern.con()
                            ), |(_, ty), _| ty )).mapper(|(ty,), loc|ty.unwrap_or(FullType(TypeT::empty(), loc))),
        TokenConsumer(TokenType::Particle('{')),
        block_pattern.clone().con(),
        TokenConsumer(TokenType::Particle('}')),
    ), |(_, ident, _, args, _, ret, _, body, _), loc| Func {
        name: ident,
        args,
        ret,
        body,
        loc
    });
    let fn_predicate = Pattern::single(TokenConsumer(TokenType::Ident(String::from("fn"))), |i, _|i);

    // only for full module, not module content
    let mod_content_pred = MatchConsumer(vec![
        (fn_predicate.clone().check_ok(), Pattern::dummy())
    ]
    ).pat();

    enum ModuleObject{
        Function(Func)
    }

    let mod_content_pattern = Pattern::named("module content",
                                             (
                                                 ListConsumer::maybe_empty_pred(
                                                 ConditionalConsumer(Pattern::dummy(), MatchConsumer(vec![
                                                     (fn_predicate.clone().check_ok(), fn_pattern.clone().mapper(|(func,), _| ModuleObject::Function(func)))
                                                    ]
                                                 ).pat()),
                                                 Pattern::dummy().maybe(),
                                                 Trail::Never),
    ), |(objects,), loc| {
            let mut functions = vec![];
            for object in objects {
                match object {
                    ModuleObject::Function(func) => functions.push(func)
                }
            }
            (functions, loc)
        });

    Patterns {
        module_content: mod_content_pattern,
        function: fn_pattern,
    }
}