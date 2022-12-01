use std::cell::{Cell, Ref, RefCell};
use std::rc::Rc;
use crate::ast::ast::{AstLiteral, Ident};
use crate::ast::patterns::{Consumer, ConsumerTuple, Pattern};
use crate::source::{ParseError, ParseET, Span};
use crate::tokens::tok_iter::TokIter;
use crate::tokens::tokens::TokenType;

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

pub(crate) struct GetParticleConsumer;

impl Consumer for GetParticleConsumer {
    type Output = (char, Span);
    fn consume(&self, iter: &mut TokIter) -> Result<Self::Output, ParseError> {
        let tok = iter.this()?;
        if let TokenType::Particle(c) = tok.tt {
            iter.next();
            Ok((c, tok.loc))
        }
        else {
            let tok = iter.this()?;
            Err(ParseET::ParsingError(format!("expected Particle, found {:?}", tok.tt)).at(tok.loc))
        }
    }
}

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

pub(crate) struct ListConsumer<
    ItemPred: ConsumerTuple, ItemPredOut,
    Item: ConsumerTuple, ItemOut,
    DeliPred: ConsumerTuple, DeliPredOut,
    Deli: ConsumerTuple, DeliOut>
(
    pub(crate) ConditionalConsumer<ItemPred, ItemPredOut, Item, ItemOut>,
    pub(crate) ConditionalConsumer<DeliPred, DeliPredOut, Deli, DeliOut>,
    Trail, bool
);

#[derive(PartialEq, Clone)]
pub(crate) enum Trail{
    Always,
    Never,
    Optional
}

impl<Item: ConsumerTuple, ItemOut,
    Deli: ConsumerTuple, DeliOut>
ListConsumer<Item, ItemOut, Item, ItemOut, Deli, DeliOut, Deli, DeliOut> {
    pub(crate) fn non_empty(item_pat: Rc<Pattern<Item, ItemOut>>, deli_pat: Rc<Pattern<Deli, DeliOut>>, trail: Trail) -> Self {
        Self(ConditionalConsumer(item_pat.clone(),
                                 item_pat),
             ConditionalConsumer(deli_pat.clone(),
                                 deli_pat), trail, false)
    }
    pub(crate) fn maybe_empty(item_pat: Rc<Pattern<Item, ItemOut>>, deli_pat: Rc<Pattern<Deli, DeliOut>>, trail: Trail) -> Self {
        Self(ConditionalConsumer(item_pat.clone(),
                                 item_pat),
             ConditionalConsumer(deli_pat.clone(),
                                 deli_pat), trail, true)
    }
}

impl<ItemPred: ConsumerTuple, ItemPredOut,
        Item: ConsumerTuple, ItemOut,
        DeliPred: ConsumerTuple, DeliPredOut,
        Deli: ConsumerTuple, DeliOut>
    ListConsumer<ItemPred, ItemPredOut, Item, ItemOut, DeliPred, DeliPredOut, Deli, DeliOut> {
    pub(crate) fn non_empty_pred(item_cond: ConditionalConsumer<ItemPred, ItemPredOut, Item, ItemOut>,
                                 deli_cond: ConditionalConsumer<DeliPred, DeliPredOut, Deli, DeliOut>,
                                 trail: Trail) -> Self{
        Self(item_cond, deli_cond, trail, false)
    }
    pub(crate) fn maybe_empty_pred(item_cond: ConditionalConsumer<ItemPred, ItemPredOut, Item, ItemOut>,
                                  deli_cond: ConditionalConsumer<DeliPred, DeliPredOut, Deli, DeliOut>,
                                  trail: Trail) -> Self{
        Self(item_cond, deli_cond, trail, true)
    }
}

impl<ItemPred: ConsumerTuple, ItemPredOut,
    Item: ConsumerTuple, ItemOut,
    DeliPred: ConsumerTuple, DeliPredOut,
    Deli: ConsumerTuple, DeliOut>
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

pub(crate) struct PatternConsumer<Item: ConsumerTuple, Out>(pub(crate) Rc<Pattern<Item, Out>>);

impl<Item: ConsumerTuple, Out> Consumer for PatternConsumer<Item, Out> {
    type Output = Out;
    fn consume(&self, iter: &mut TokIter) -> Result<Self::Output, ParseError> {
        self.0.consume(iter)
    }
}

pub(crate) struct RefLoopPatternConsumer<Out>(pub(crate) RefCell<Option<Box<dyn Consumer<Output=Out>>>>);

impl<Out> RefLoopPatternConsumer<Out> {
    pub(crate) fn create() -> Self {
        Self(RefCell::new(None))
    }

    pub(crate) fn finalize(&self, pattern_consumer: Box<dyn Consumer<Output=Out>>){
        self.0.swap(&RefCell::new(Some(pattern_consumer)));
    }
}

impl<Out> Consumer for RefLoopPatternConsumer<Out> {
    type Output = Out;
    fn consume(&self, iter: &mut TokIter) -> Result<Self::Output, ParseError> {
        let pat = self.0.borrow();
        if pat.is_some() {
            pat.as_ref().unwrap().consume(iter)
        } else {
            panic!("Pattern with reference loop was not finalized!")
        }
    }
}

pub(crate) struct ConditionalConsumer<
    Predicate: ConsumerTuple, PredicateOut,
    Item: ConsumerTuple, Out>
(pub(crate) Rc<Pattern<Predicate, PredicateOut>>,
 pub(crate) Rc<Pattern<Item, Out>>);

impl<Predicate: ConsumerTuple, PredicateOut,
    Item: ConsumerTuple, Out>
Consumer for ConditionalConsumer<Predicate, PredicateOut, Item, Out> {
    type Output = Option<Out>;
    fn consume(&self, iter: &mut TokIter) -> Result<Self::Output, ParseError> {
        Ok(if let Ok(_) = self.0.consume(&mut iter.clone()){
            Some(self.1.consume(iter)?)
        } else {
            None
        })
    }
}

pub(crate) struct BranchIfElse<
    Predicate: ConsumerTuple,
    PredicateOut,
    IfBranch: ConsumerTuple,
    ElseBranch: ConsumerTuple,
    BranchesOut>
(pub(crate) Rc<Pattern<Predicate, PredicateOut>>,
 pub(crate) Rc<Pattern<IfBranch, BranchesOut>>,
 pub(crate) Rc<Pattern<ElseBranch, BranchesOut>>);

impl <Predicate: ConsumerTuple,
    PredicateOut,
    IfBranch: ConsumerTuple,
    ElseBranch: ConsumerTuple,
    BranchesOut>
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

pub(crate) struct CustomConsumer<Out> (pub(crate) fn(&mut TokIter) -> Result<Out, ParseError>);

impl <Out> Consumer for CustomConsumer<Out>{
    type Output = Out;
    fn consume(&self, iter: &mut TokIter) -> Result<Self::Output, ParseError> {
        (self.0)(iter)
    }
}

pub(crate) struct Unwrapper<T: ConsumerTuple, Out>(pub(crate) Rc<Pattern<T, Result<Out, ParseError>>>);

impl<T: ConsumerTuple, Out> Consumer for Unwrapper<T, Out> {
    type Output = Out;
    fn consume(&self, iter: &mut TokIter) -> Result<Self::Output, ParseError> {
        self.0.consume(iter)?
    }
}