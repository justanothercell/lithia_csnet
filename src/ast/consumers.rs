use std::cell::{RefCell};
use std::rc::Rc;
use crate::ast::ast::{AstLiteral, Ident};
use crate::ast::patterns::{ConsumablePattern, Consumer, Pat};
use crate::source::{ParseError, ParseET};
use crate::tokens::tok_iter::TokIter;
use crate::tokens::tokens::{glued, TokenType};

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

impl TokenConsumer {
    pub(crate) fn particle(p: char, glued: glued) -> Self {
        Self(TokenType::Particle(p, glued))
    }

    pub(crate) fn ident(ident: &str) -> Self {
        Self(TokenType::Ident(ident.to_string()))
    }
}

pub(crate) struct ParticleConsumer(pub(crate) char);

impl Consumer for ParticleConsumer {
    type Output = ();

    fn consume(&self, iter: &mut TokIter) -> Result<Self::Output, ParseError> {
        if let TokenType::Particle(p, _) = iter.this()?.tt {
            if p == self.0 {
                iter.next();
                Ok(())
            } else {
                Err(ParseET::ParsingError(format!("expected '{}', found '{p}'", self.0)).at(iter.this()?.loc))
            }
        }
        else {
            let tok = iter.this()?;
            Err(ParseET::ParsingError(format!("expected particle '{}', found {:?}", self.0, tok.tt)).at(tok.loc))
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
    type Output = char;
    fn consume(&self, iter: &mut TokIter) -> Result<Self::Output, ParseError> {
        let tok = iter.this()?;
        if let TokenType::Particle(c, _) = tok.tt {
            iter.next();
            Ok(c)
        }
        else {
            let tok = iter.this()?;
            Err(ParseET::ParsingError(format!("expected Particle, found {:?}", tok.tt)).at(tok.loc))
        }
    }
}

pub(crate) struct GetGluedParticleConsumer;

impl Consumer for GetGluedParticleConsumer {
    type Output = char;
    fn consume(&self, iter: &mut TokIter) -> Result<Self::Output, ParseError> {
        let tok = iter.this()?;
        if let TokenType::Particle(c, true) = tok.tt {
            iter.next();
            Ok(c)
        }
        else {
            let tok = iter.this()?;
            Err(ParseET::ParsingError(format!("expected glued Particle, found {:?}", tok.tt)).at(tok.loc))
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

pub(crate) struct ListConsumer<ItemPredOut, ItemOut, DeliPredOut, DeliOut>
(
    pub(crate) ConditionalConsumer<ItemPredOut, ItemOut>,
    pub(crate) ConditionalConsumer<DeliPredOut, DeliOut>,
    Trail, bool
);

#[derive(PartialEq, Clone)]
pub(crate) enum Trail{
    Always,
    Never,
    Optional
}

impl<ItemOut, DeliOut> ListConsumer<ItemOut, ItemOut, DeliOut, DeliOut> {
    pub(crate) fn non_empty(item_pat: Pat<ItemOut>, deli_pat: Pat<DeliOut>, trail: Trail) -> Self {
        Self(ConditionalConsumer(item_pat.clone(),
                                 item_pat),
             ConditionalConsumer(deli_pat.clone(),
                                 deli_pat), trail, false)
    }
    pub(crate) fn maybe_empty(item_pat: Pat<ItemOut>, deli_pat: Pat<DeliOut>, trail: Trail) -> Self {
        Self(ConditionalConsumer(item_pat.clone(),
                                 item_pat),
             ConditionalConsumer(deli_pat.clone(),
                                 deli_pat), trail, true)
    }
}

impl<ItemPredOut, ItemOut, DeliPredOut, DeliOut> ListConsumer<ItemPredOut, ItemOut, DeliPredOut, DeliOut> {
    pub(crate) fn non_empty_pred(item_cond: ConditionalConsumer<ItemPredOut, ItemOut>,
                                 deli_cond: ConditionalConsumer<DeliPredOut, DeliOut>,
                                 trail: Trail) -> Self{
        Self(item_cond, deli_cond, trail, false)
    }
    pub(crate) fn maybe_empty_pred(item_cond: ConditionalConsumer<ItemPredOut, ItemOut>,
                                  deli_cond: ConditionalConsumer<DeliPredOut, DeliOut>,
                                  trail: Trail) -> Self{
        Self(item_cond, deli_cond, trail, true)
    }
}

impl<ItemPredOut, ItemOut, DeliPredOut, DeliOut> Consumer for ListConsumer<ItemPredOut, ItemOut, DeliPredOut, DeliOut> {
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

pub(crate) struct PatternConsumer<Out>(pub(crate) Pat<Out>);

impl<Out> PatternConsumer<Out> {
    pub(crate) fn new(pattern: Pat<Out>) -> Self{
        Self(pattern)
    }
}

impl<Out> Consumer for PatternConsumer<Out> {
    type Output = Out;
    fn consume(&self, iter: &mut TokIter) -> Result<Self::Output, ParseError> {
        self.0.consume(iter)
    }
}

pub(crate) struct RefLoopPatternConsumer<Out>(Rc<RefCell<Option<Pat<Out>>>>);

impl<Out> RefLoopPatternConsumer<Out> {
    pub(crate) fn create() -> (Self, Finalizer<Out>) {
        let cell = Rc::new(RefCell::new(None));
        (Self(cell.clone()), Finalizer(cell))
    }


}

pub(crate) struct Finalizer<Out>(Rc<RefCell<Option<Pat<Out>>>>);

impl<Out> Finalizer<Out> {
    pub(crate) fn finalize(self, pat: Pat<Out>){
        self.0.swap(&RefCell::new(Some(pat)));
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

pub(crate) struct ConditionalConsumer<PredicateOut, Out>(pub(crate) Pat<PredicateOut>, pub(crate) Pat<Out>);

impl<PredicateOut, Out> Consumer for ConditionalConsumer<PredicateOut, Out> {
    type Output = Option<Out>;
    fn consume(&self, iter: &mut TokIter) -> Result<Self::Output, ParseError> {
        Ok(if let Ok(_) = self.0.consume(&mut iter.clone()){
            Some(self.1.consume(iter)?)
        } else {
            None
        })
    }
}

pub(crate) struct BranchIfElse<PredicateOut, BranchesOut>
(pub(crate) Pat<PredicateOut>, pub(crate) Pat<BranchesOut>, pub(crate) Pat<BranchesOut>);

impl <PredicateOut, BranchesOut> Consumer for BranchIfElse<PredicateOut, BranchesOut>{
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

pub(crate) struct Unwrapper<Out>(pub(crate) Pat<Result<Out, ParseError>>);

impl<Out> Consumer for Unwrapper<Out> {
    type Output = Out;
    fn consume(&self, iter: &mut TokIter) -> Result<Self::Output, ParseError> {
        self.0.consume(iter)?
    }
}

pub(crate) struct MatchConsumer<Out>(pub(crate) Vec<(Pat<()>, Pat<Out>)>);

impl<Out> Consumer for MatchConsumer<Out> {
    type Output = Out;
    fn consume(&self, iter: &mut TokIter) -> Result<Self::Output, ParseError> {
        for (pred, pat) in &self.0 {
            if let Ok(_) = pred.consume(&mut iter.clone()){
                return pat.consume(iter)
            }
        }
        return Err(ParseET::ParsingError("could not match to any case in pattern matcher".to_string()).at(iter.this()?.loc))
    }
}