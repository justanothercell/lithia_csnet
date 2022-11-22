use crate::source::{OnParseErr, ParseError, Span};
use crate::tokens::tok_iter::TokIter;

pub(crate) trait Consumer: Clone {
    type Output: Clone;
    fn consume(&self, iter: &mut TokIter) -> Result<Self::Output, ParseError>;
}

#[derive(Clone)]
pub(crate) struct Pattern<T: ConsumerTuple + Clone, Out: Clone> {
    name: Option<String>,
    pub(crate) consumers: T,
    mapper: fn(T::Output, Span) -> Out
}

impl<T: ConsumerTuple + Clone, Out: Clone> Pattern<T, Out> {
    pub(crate) fn inline(consumers: T, mapper: fn(T::Output, Span) -> Out) -> Box<Self>{
        Box::new(Self {
            name: None,
            consumers,
            mapper
        })
    }

    pub(crate) fn named(name: &str, consumers: T, mapper: fn(T::Output, Span) -> Out) -> Box<Self>{
        Box::new(Self {
            name: Some(name.to_string()),
            consumers,
            mapper
        })
    }

    pub(crate) fn consume(&self, iter: &mut TokIter) -> Result<Out, ParseError>{
        let mut span = iter.this()?.loc;
        let mut r = self.consumers.consume(iter);
        if let Some(name) = &self.name {
            r = r.when_e(format!("parsing {}", name))
        }
        span.extend(iter.nearest_point()?.end());
        Ok((self.mapper)(r?, span))
    }
}

impl Pattern<(), ()> {
    pub(crate) fn dummy() -> Box<Self>{
        Box::new(Self {
            name: None,
            consumers: (),
            mapper: |_, _|()
        })
    }
}

impl<C: Consumer + Clone, Out: Clone> Pattern<(C,), Out> {
    pub(crate) fn single(single: C, mapper: fn(<(C, ) as ConsumerTuple>::Output, Span) -> Out) -> Box<Self> {
        Box::new(Self {
            name: None,
            consumers: (single, ),
            mapper
        })
    }
}

pub(crate) trait ConsumerTuple: Clone{
    type Output: Clone;
    fn consume(&self, iter: &mut TokIter) -> Result<Self::Output, ParseError>;
}

impl ConsumerTuple for () {
    type Output = ();
    #[inline]
    fn consume(&self, _iter: &mut TokIter) -> Result<Self::Output, ParseError> {
        Ok(())
    }
}

macro_rules! tupler {
    ($($t:ident, $n: tt;)*) => {
        impl<$($t: Consumer,)*> ConsumerTuple for ($($t,)*) {
            type Output = ($($t::Output,)*);
            #[inline]
            fn consume(&self, iter: &mut TokIter) -> Result<Self::Output, ParseError> {
                Ok(($(self.$n.consume(iter)?,)*))
            }
        }
    };
}

tupler!(T0, 0;);
tupler!(T0, 0; T1, 1;);
tupler!(T0, 0; T1, 1; T2, 2;);
tupler!(T0, 0; T1, 1; T2, 2; T3, 3;);
tupler!(T0, 0; T1, 1; T2, 2; T3, 3; T4, 4;);
tupler!(T0, 0; T1, 1; T2, 2; T3, 3; T4, 4; T5, 5;);
tupler!(T0, 0; T1, 1; T2, 2; T3, 3; T4, 4; T5, 5; T6, 6;);
tupler!(T0, 0; T1, 1; T2, 2; T3, 3; T4, 4; T5, 5; T6, 6; T7, 7;);
tupler!(T0, 0; T1, 1; T2, 2; T3, 3; T4, 4; T5, 5; T6, 6; T7, 7; T8, 8;);
tupler!(T0, 0; T1, 1; T2, 2; T3, 3; T4, 4; T5, 5; T6, 6; T7, 7; T8, 8; T9, 9;);
tupler!(T0, 0; T1, 1; T2, 2; T3, 3; T4, 4; T5, 5; T6, 6; T7, 7; T8, 8; T9, 9; T10, 10;);
tupler!(T0, 0; T1, 1; T2, 2; T3, 3; T4, 4; T5, 5; T6, 6; T7, 7; T8, 8; T9, 9; T10, 10; T11, 11;);
tupler!(T0, 0; T1, 1; T2, 2; T3, 3; T4, 4; T5, 5; T6, 6; T7, 7; T8, 8; T9, 9; T10, 10; T11, 11; T12, 12;);
tupler!(T0, 0; T1, 1; T2, 2; T3, 3; T4, 4; T5, 5; T6, 6; T7, 7; T8, 8; T9, 9; T10, 10; T11, 11; T12, 12; T13, 13;);
tupler!(T0, 0; T1, 1; T2, 2; T3, 3; T4, 4; T5, 5; T6, 6; T7, 7; T8, 8; T9, 9; T10, 10; T11, 11; T12, 12; T13, 13; T14, 14;);
tupler!(T0, 0; T1, 1; T2, 2; T3, 3; T4, 4; T5, 5; T6, 6; T7, 7; T8, 8; T9, 9; T10, 10; T11, 11; T12, 12; T13, 13; T14, 14; T15, 15;);