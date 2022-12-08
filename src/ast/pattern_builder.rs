use crate::ast::ast::{AstLiteral, Block, Expr, Expression, FullType, Func, Item, Op, Operator, OpGroup, Statement, Stmt, Type, TypeT};
use crate::ast::consumers::{BranchIfElse, ConditionalConsumer, CustomConsumer, GetGluedParticleConsumer, GetIdentConsumer, GetLiteralConsumer, GetParticleConsumer, ListConsumer, MatchConsumer, ParticleConsumer, PatternConsumer, RefLoopPatternConsumer, TokenConsumer, Trail};
use crate::ast::patterns::{MapConsumer, Pat, Pattern};
use crate::source::{ParseError, ParseET, Span};
use crate::tokens::tokens::{Literal, TokenType};

pub(crate) struct Patterns {
    pub(crate) module_content: Pat<(Vec<Func>, Span)>,
    pub(crate) function: Pat<Func>
}

pub(crate) fn build_patterns() -> Patterns{
    let item_pattern = Pattern::named("item", (ListConsumer::non_empty(
        Pattern::single(GetIdentConsumer,|(ident,), _| ident),
        Pattern::inline((
                            ParticleConsumer(':'),
                            TokenConsumer::particle(':', true)  // ::
                        ),|_, _| ()),
        Trail::Never
    ),), |(path,), loc| Item(path, loc));

    // === TYPES ===
    let (ref_loop_type_for_generics, ref_loop_type_for_generics_finalizer) = RefLoopPatternConsumer::<FullType>::create();
    let generics_pattern = Pattern::named("generics", (
        ParticleConsumer('<'),
        ListConsumer::maybe_empty_pred(
            ConditionalConsumer(GetIdentConsumer.pat(),
                                Pattern::single(ref_loop_type_for_generics, |(i,), _| i)),
            ConditionalConsumer(Pattern::single(ParticleConsumer(','), |(i,), _| i),
                                Pattern::single(ParticleConsumer(','), |(_,), _| ())),
            Trail::Optional
        ),
        ParticleConsumer('>'),
    ), |(_, generics, _), _| generics);
    let optional_generics_pattern = Pattern::named("optional generics", (
        ConditionalConsumer(Pattern::single(ParticleConsumer('<'), |_,_|()),
                            generics_pattern.clone()),
    ), |(g,), _| g.unwrap_or(vec![])
    );
    let type_pattern = Pattern::named("single type", (
       item_pattern.clone().con(),
       optional_generics_pattern.clone().con()
    ), |(path, generics), loc| Type { generics, base_type: path, loc });
    let type_predicate = BranchIfElse(ParticleConsumer('(').pat(),
                                                      ParticleConsumer('(').pat(),
                                                      GetIdentConsumer.pat().ok()).pat();
    let (ref_loop_type_for_tuple, ref_loop_type_for_tuple_finalizer) = RefLoopPatternConsumer::<FullType>::create();
    let tuple_type_pattern = Pattern::inline((ParticleConsumer('('),
                                              ListConsumer::maybe_empty_pred(
                                                  ConditionalConsumer(
                                                     type_predicate.clone().con().pat(),
                                                      ref_loop_type_for_tuple.pat(),
                                                  ),
                                                  ConditionalConsumer(
                                                      ParticleConsumer(',').pat(),
                                                      ParticleConsumer(',').pat()),
                                                  Trail::Never),
                                              ParticleConsumer(')')
                                             ), |(_, t, _ ), _loc|TypeT::Tuple(t));
    let full_type_pattern = Pattern::named("type", (
        BranchIfElse(ParticleConsumer('(').pat(),
                     tuple_type_pattern.clone(),
                     Pattern::single(PatternConsumer(type_pattern.clone()), |(t,), _loc|TypeT::Single(t))
        ),
    ), |(ty,), loc| FullType(ty, loc));

    // === FUNCTION ===
    ref_loop_type_for_generics_finalizer.finalize(full_type_pattern.clone());
    ref_loop_type_for_tuple_finalizer.finalize(full_type_pattern.clone());
    let ident_type_pair_pattern = Pattern::named("ident type pair", (
        GetIdentConsumer,
        ParticleConsumer(':'),
       full_type_pattern.clone().con(),
    ),|(ident, _, ty), _| (ident, ty));

    let fn_def_args_pattern = Pattern::named("function args", (
        ListConsumer::maybe_empty_pred(ConditionalConsumer(
            GetIdentConsumer.pat(),
            ident_type_pair_pattern.clone()
        ),
                                       ConditionalConsumer(
                                           ParticleConsumer(',').pat(),
                                           ParticleConsumer(',').pat()),
                                       Trail::Never
        ),), |(args,), _| args);

    let arrow_pattern = Pattern::inline((
                                            ParticleConsumer('-'),
                                            ParticleConsumer('>')
                                        ), |_, _|());
    // === EXPRESSION ===
    #[derive(Debug, Clone)]
    enum ExprPart{
        Expr(Expression),
        Op(Operator),
        MultiOps(Vec<Op>, Span)
    }
    impl ExprPart {
        fn loc(&self) -> Span{
            match self {
                ExprPart::Expr(expr) => expr.1.clone(),
                ExprPart::Op(op) => op.1.clone(),
                ExprPart::MultiOps(_, loc) => loc.clone()
            }
        }
    }
    let expr_predicate = CustomConsumer(|iter|{
        let tok = iter.this()?;
        iter.next();
        match tok.tt {
            TokenType::Particle(')', _) => Ok(()),
            TokenType::Particle(';', _) => Ok(()),
            TokenType::Particle(',', _) => Ok(()),
            _ => Err(ParseET::ParsingError(format!("found {:?}", tok.tt)).at(tok.loc))
        }
    }).pat().fail();
    let (ref_loop_fn_call,ref_loop_fn_call_finalizer) = RefLoopPatternConsumer::<Expr>::create();
    let expr_pattern = Pattern::named("expression", (
        ListConsumer::non_empty_pred(
            ConditionalConsumer(expr_predicate.clone(),
            MatchConsumer(vec![
                (GetLiteralConsumer.pat().ok(), GetLiteralConsumer.pat()
                    .mapper(|(lit,), loc| ExprPart::Expr(Expression(Expr::Literal(lit), loc)))),
                (Pattern::inline((GetIdentConsumer, ParticleConsumer('(')), |_, _|()), ref_loop_fn_call.pat()
                    .mapper(|(ident,), loc| ExprPart::Expr(Expression(ident, loc)))),
                (GetIdentConsumer.pat().ok(), GetIdentConsumer.pat()
                    .mapper(|(ident,), loc| ExprPart::Expr(Expression(Expr::Variable(ident), loc)))),
                (GetParticleConsumer.pat().ok(), Pattern::inline((
                    GetParticleConsumer,
                    ListConsumer::maybe_empty(GetGluedParticleConsumer.pat(), Pattern::dummy(), Trail::Always)
                ), |(p, mut pp, ), _loc| { pp.insert(0, p); pp })
                    .mapper_failable(|(c, ), loc| Ok(ExprPart::MultiOps(Op::from_chars_multi(c, Some(loc.clone()))?, loc)))),
            ]).pat()),
            Pattern::dummy().maybe(),
            Trail::Always
        ),
    ), |(parts,), _loc| parts).mapper_failable(|(mut parts,), loc| {
        // === flatten MultiPops ===
        let mut flattened = vec![];
        for part in parts.into_iter() {
            if let ExprPart::MultiOps(ops, loc) = part {
                for op in ops {
                    flattened.push(ExprPart::Op(Operator(op, loc.clone())))
                }
            } else {
                flattened.push(part)
            }
        }
        parts = flattened;
        // === unary ops ===
        let mut combined_parts = vec![];
        let mut num_ops = 1; // start at 1 to allow unary ops at the beginning
        for part in parts {
            if let ExprPart::Expr(mut expr) = part {
                while num_ops > 1 {
                    if let ExprPart::Op(op) = combined_parts.pop().unwrap() {
                        let loc = op.1.clone();
                        expr = Expression(Expr::UnaryOp(op, Box::new(expr)), loc)
                    } else { unreachable!("this should have been an op but was an expression") }
                    num_ops -= 1;
                }
                combined_parts.push(ExprPart::Expr(expr));
                num_ops = 0;
            } else {
                combined_parts.push(part);
                num_ops += 1;
            }
        }
        parts = combined_parts;
        // === binary ops ===
        macro_rules! combine_parts {
            ($op_group: ident) => {
                let mut parts_iter = parts.into_iter();
                let mut combined_parts = vec![];
                while let Some(part) = parts_iter.next() {
                    match part {
                        ExprPart::Op(op) if op.0.group() == OpGroup::$op_group => {
                            let (l, r) = (combined_parts.pop(), parts_iter.next());
                            if let (Some(ExprPart::Expr(left)), Some(ExprPart::Expr(right))) = (l.clone(), r.clone()){
                                let mut span = left.1.clone();
                                span.extend(right.1.end());
                                combined_parts.push(ExprPart::Expr(Expression(Expr::BinaryOp(op, Box::new(left), Box::new(right)), span)))
                            } else {
                                return Err(ParseET::ParsingError(format!("operator {:?} expects expression on both sides", op.0)).at(op.1))
                            }
                        },
                        p => combined_parts.push(p)
                    }
                }
                parts = combined_parts;
            };
        }
        combine_parts!(Dot);
        combine_parts!(Bin);
        combine_parts!(Dash);
        combine_parts!(Bool);

        if parts.len() != 1 {
            return Err(ParseET::ParsingError(format!("could not resolve expression completely, expected 1 resolved part but found {}", parts.len())).at(loc))
        }
        if let ExprPart::Expr(expr) = parts.pop().unwrap() {
            return Ok(expr);
        }
        return Err(ParseET::ParsingError(format!("could not resolve expression completely")).at(loc))
    }
    );
    let args_pattern = Pattern::named("args", (ListConsumer::maybe_empty_pred(
        ConditionalConsumer(PatternConsumer(expr_predicate.clone()).pat(),
                            expr_pattern.clone()),
        ParticleConsumer(',').pat().maybe(),
        Trail::Never),
    ), |(args, ), _| args);
    let fn_call_pattern = Pattern::named("function call",(
       item_pattern.clone().con(),
        ParticleConsumer('('),
       args_pattern.clone().con(),
        ParticleConsumer(')'),
    ), |(ident, _, args, _), _| Expr::FuncCall(ident, args)
    );
    ref_loop_fn_call_finalizer.finalize(fn_call_pattern.clone());

    // === STATEMENT ===
    let var_creation_pattern = Pattern::named("variable declaration",(
        TokenConsumer::ident("let"),
        TokenConsumer::ident("mut").pat().maybe(),
        item_pattern.clone().con(),
        ConditionalConsumer(ParticleConsumer(':').pat(),
                            Pattern::named("variable type", (
                                ParticleConsumer(':'),
                               full_type_pattern.clone().con()
                            ), |(_, ty), _| ty)
        ),
        ParticleConsumer('='),
        expr_pattern.clone().con()
    ), |(_, mutable, ident, ty, _, value), _|
                                                  Stmt::VarCreate(ident, mutable.is_some(), ty, value)
    );

    let var_assign_pattern = Pattern::named("variable assignment",(
        item_pattern.clone().con(),
        BranchIfElse(ParticleConsumer('=').pat().fail(),
                     Pattern::named("variable assignment",(
                         GetParticleConsumer,
                         ListConsumer::non_empty_pred(ConditionalConsumer(ParticleConsumer('=').pat().fail(), GetGluedParticleConsumer.pat()),
                                                    Pattern::dummy().maybe(),
                                                    Trail::Always),
                     TokenConsumer::particle('=', true)  // equals should be right behind
                     ),|(c, mut cc, _), _loc| {
                        cc.insert(0, c);
                        cc
                     }).mapper_failable(|(cc,), loc|{
                        Ok(Some(Operator(Op::from_chars(cc, Some(loc.clone()))?, loc)))
                    }),
                     ParticleConsumer('=').pat().mapper(|_, _| None)
        ),
        expr_pattern.clone().con()
    ), |(ident, op, value), _loc| Stmt::VarAssign(ident, op,value)
    );

    let statement_pattern = Pattern::named("statement",(
        MatchConsumer(vec![
            (TokenConsumer::ident("let").pat(), var_creation_pattern.clone()),
            (Pattern::inline((
                                 GetIdentConsumer,
                                 BranchIfElse(
                                     ParticleConsumer(':').pat(),
                                     ParticleConsumer(':').pat(),
                                     ParticleConsumer('(').pat(),
                                 )
                             ), |_, _|()), fn_call_pattern.clone().mapper(|(func,), loc|Stmt::Expression(Expression(func, loc)))),
            (Pattern::dummy(), var_assign_pattern.clone())
        ]),
        ParticleConsumer(';')
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
        TokenConsumer::ident("fn"),
        GetIdentConsumer,
        ParticleConsumer('('),
        fn_def_args_pattern.clone().con(),
        ParticleConsumer(')'),
        ConditionalConsumer(arrow_pattern.clone(),
                            Pattern::named("return type",(
                               arrow_pattern.clone().con(),
                               full_type_pattern.con()
                            ), |(_, ty), _| ty )).mapper(|(ty,), loc|ty.unwrap_or(FullType(TypeT::empty(), loc))),
        ParticleConsumer('{'),
        block_pattern.clone().con(),
        ParticleConsumer('}'),
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
        (fn_predicate.clone().ok(), Pattern::dummy())
    ]
    ).pat();

    #[derive(Debug, Clone)]
    enum ModuleObject{
        Function(Func)
    }
    let mod_content_pattern = Pattern::named("module content",
                                             (
                                                 ListConsumer::maybe_empty_pred(
                                                 ConditionalConsumer(Pattern::dummy(), MatchConsumer(vec![
                                                     (fn_predicate.clone().ok(), fn_pattern.clone().mapper(|(func,), _| ModuleObject::Function(func)))
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