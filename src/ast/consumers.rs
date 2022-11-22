use crate::ast::ast::{AstLiteral, Ident};
use crate::ast::patterns::{Consumer, ConsumerTuple, Pattern};
use crate::source::{ParseError, ParseET, Span};
use crate::tokens::tok_iter::TokIter;
use crate::tokens::tokens::TokenType;

#[derive(Clone)]
pub(crate) struct TokenConsumer(pub(crate) TokenType);

impl Consumer for TokenConsumer {
    type Output = ();
    fn consume(&self, iter: &mut TokIter) -> Result<Self::Output, ParseError> {
        if iter.this()?.tt == self.0 {
            iter.next();
            Ok(())
        }
        else {
            let tok = iter.this()?;
            Err(ParseET::ParsingError(format!("expected {:?}, found {:?}", self.0, tok.tt)).at(tok.loc))
        }
    }
}

#[derive(Clone)]
pub(crate) struct GetIdentConsumer;

impl Consumer for GetIdentConsumer {
    type Output = Ident;
    fn consume(&self, iter: &mut TokIter) -> Result<Self::Output, ParseError> {
        let tok = iter.this()?;
        if let TokenType::Ident(ident) = tok.tt {
            iter.next();
            Ok(Ident(ident, tok.loc))
        }
        else {
            let tok = iter.this()?;
            Err(ParseET::ParsingError(format!("expected Ident, found {:?}", tok.tt)).at(tok.loc))
        }
    }
}

#[derive(Clone)]
pub(crate) struct GetLiteralConsumer;

impl Consumer for GetLiteralConsumer {
    type Output = AstLiteral;
    fn consume(&self, iter: &mut TokIter) -> Result<Self::Output, ParseError> {
        let tok = iter.this()?;
        if let TokenType::Literal(literal) = tok.tt {
            iter.next();
            Ok(AstLiteral(literal, tok.loc))
        }
        else {
            let tok = iter.this()?;
            Err(ParseET::ParsingError(format!("expected Literal, found {:?}", tok.tt)).at(tok.loc))
        }
    }
}

#[derive(Clone)]
pub(crate) struct ListConsumer<
    ItemPred: ConsumerTuple + Clone, ItemPredOut: Clone,
    Item: ConsumerTuple + Clone, ItemOut: Clone, 
    DeliPred: ConsumerTuple + Clone, DeliPredOut: Clone,
    Deli: ConsumerTuple + Clone, DeliOut: Clone>
(
    pub(crate) ConditionalConsumer<ItemPred, ItemPredOut, Item, ItemOut, Option<ItemOut>>,
    pub(crate) ConditionalConsumer<DeliPred, DeliPredOut, Deli, DeliOut, Option<DeliOut>>,
    Trail, bool
);

#[derive(PartialEq, Clone)]
pub(crate) enum Trail{
    Always,
    Never,
    Optional
}

impl<Item: ConsumerTuple + Clone, ItemOut: Clone,
    Deli: ConsumerTuple + Clone, DeliOut: Clone>
ListConsumer<Item, ItemOut, Item, ItemOut, Deli, DeliOut, Deli, DeliOut> {
    pub(crate) fn non_empty(item_pat: Pattern<Item, ItemOut>, deli_pat: Pattern<Deli, DeliOut>, trail: Trail) -> Self {
        Self(ConditionalConsumer(item_pat.clone(),
                                 item_pat,
                                 |i, _| i),
             ConditionalConsumer(deli_pat.clone(),
                                 deli_pat,
                                 |i, _| i), trail, false)
    }
    pub(crate) fn maybe_empty(item_pat: Pattern<Item, ItemOut>, deli_pat: Pattern<Deli, DeliOut>, trail: Trail) -> Self {
        Self(ConditionalConsumer(item_pat.clone(),
                                 item_pat,
                                 |i, _| i),
             ConditionalConsumer(deli_pat.clone(),
                                 deli_pat,
                                 |i, _| i), trail, true)
    }
}

impl<ItemPred: ConsumerTuple + Clone, ItemPredOut: Clone,
        Item: ConsumerTuple + Clone, ItemOut: Clone,
        DeliPred: ConsumerTuple + Clone, DeliPredOut: Clone,
        Deli: ConsumerTuple + Clone, DeliOut: Clone>
    ListConsumer<ItemPred, ItemPredOut, Item, ItemOut, DeliPred, DeliPredOut, Deli, DeliOut> {
    pub(crate) fn non_empty_pred(item_cond: ConditionalConsumer<ItemPred, ItemPredOut, Item, ItemOut, Option<ItemOut>>,
                                 deli_cond: ConditionalConsumer<DeliPred, DeliPredOut, Deli, DeliOut, Option<DeliOut>>,
                                 trail: Trail) -> Self{
        Self(item_cond, deli_cond, trail, false)
    }
    pub(crate) fn maybe_empty_pred(item_cond: ConditionalConsumer<ItemPred, ItemPredOut, Item, ItemOut, Option<ItemOut>>,
                                  deli_cond: ConditionalConsumer<DeliPred, DeliPredOut, Deli, DeliOut, Option<DeliOut>>,
                                  trail: Trail) -> Self{
        Self(item_cond, deli_cond, trail, true)
    }
}

impl<ItemPred: ConsumerTuple + Clone, ItemPredOut: Clone,
    Item: ConsumerTuple + Clone, ItemOut: Clone,
    DeliPred: ConsumerTuple + Clone, DeliPredOut: Clone,
    Deli: ConsumerTuple + Clone, DeliOut: Clone>
Consumer for ListConsumer<ItemPred, ItemPredOut, Item, ItemOut, DeliPred, DeliPredOut, Deli, DeliOut> {
    type Output = Vec<ItemOut>;
    fn consume(&self, iter: &mut TokIter) -> Result<Self::Output, ParseError> {
        let mut out = vec![];
        loop {
            // item
            match self.0.consume(iter)? {
                Some(v) => out.push(v),
                None if self.2 == Trail::Never && !self.3 => return Err(ParseET::ParsingError(format!("unexpected end of listing: expecting last element to be an item")).at(iter.this()?.loc)),
                None => break
            }
            // deli
            match self.1.consume(iter)? {
                Some(_) => (),
                None if self.2 == Trail::Always => return Err(ParseET::ParsingError(format!("unexpected end of listing: expecting last element to be a deli")).at(iter.this()?.loc)),
                None => break
            }
        }
        Ok(out)
    }
}

#[derive(Clone)]
pub(crate) struct PatternConsumer<Item: ConsumerTuple + Clone, Out: Clone>(pub(crate) Pattern<Item, Out>);

impl<Item: ConsumerTuple + Clone, Out: Clone> Consumer for PatternConsumer<Item, Out> {
    type Output = Out;
    fn consume(&self, iter: &mut TokIter) -> Result<Self::Output, ParseError> {
        self.0.consume(iter)
    }
}

#[derive(Clone)]
pub(crate) struct ConditionalConsumer<
    Predicate: ConsumerTuple + Clone, PredicateOut: Clone,
    Item: ConsumerTuple + Clone, Out: Clone,
    Mapped: Clone>
(pub(crate) Pattern<Predicate, PredicateOut>,
 pub(crate) Pattern<Item, Out>,
 pub(crate) fn(Option<Out>, Span) -> Mapped);

impl<Predicate: ConsumerTuple + Clone, PredicateOut: Clone,
    Item: ConsumerTuple + Clone, Out: Clone,
    Mapped: Clone>
Consumer for ConditionalConsumer<Predicate, PredicateOut, Item, Out, Mapped> {
    type Output = Mapped;
    fn consume(&self, iter: &mut TokIter) -> Result<Self::Output, ParseError> {
        Ok((self.2)(if let Ok(_) = self.0.consume(&mut iter.clone()){
            Some(self.1.consume(iter)?)
        } else {
            None
        }, iter.this()?.loc))
    }
}

#[derive(Clone)]
pub(crate) struct BranchIfElse<
    Predicate: ConsumerTuple + Clone, 
    PredicateOut: Clone, 
    IfBranch: ConsumerTuple + Clone,
    ElseBranch: ConsumerTuple + Clone,
    BranchesOut: Clone>
(pub(crate) Pattern<Predicate, PredicateOut>, Pattern<IfBranch, BranchesOut>, Pattern<ElseBranch, BranchesOut>);

impl <Predicate: ConsumerTuple + Clone, 
    PredicateOut: Clone,
    IfBranch: ConsumerTuple + Clone,
    ElseBranch: ConsumerTuple + Clone,
    BranchesOut: Clone> 
Consumer for BranchIfElse<Predicate, PredicateOut, IfBranch, ElseBranch, BranchesOut>{
    type Output = BranchesOut;

    fn consume(&self, iter: &mut TokIter) -> Result<Self::Output, ParseError> {
        if let Ok(_) = self.0.consume(&mut iter.clone()){
            self.1.consume(iter)
        } else {
            self.2.consume(iter)
        }
    }
}