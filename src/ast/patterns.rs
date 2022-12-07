use std::rc::Rc;
use crate::ast::consumers::{ConditionalConsumer, PatternConsumer, Unwrapper};
use crate::source::{OnParseErr, ParseError, ParseET, Span};
use crate::tokens::tok_iter::TokIter;

pub(crate) struct Pat<Out>(Rc<Box<dyn ConsumablePattern<Output=Out>>>);

impl<Out> Pat<Out>{
    pub(crate) fn clone(&self) -> Self{
        Pat(self.0.clone())
    }

    pub(crate) fn con(self) -> PatternConsumer<Out> {
        PatternConsumer(self)
    }

    pub(crate) fn maybe(self) -> ConditionalConsumer<Out, Out> {
        ConditionalConsumer(self.clone(), self)
    }

    pub(crate) fn ok(self) -> Pat<()>
        where Self: 'static {
        Pattern::single(self.con(),  |_, _|())
    }

    pub(crate) fn fail(self) -> Pat<()>
        where Self: 'static {
        self.maybe().mapper_failable(|(option,), loc| if option.is_some() {
            Err(ParseET::ParsingError(String::from("expected condition to fail, got success")).at(loc))
        } else { Ok(())}).pat()
    }

    pub(crate) fn mapper<Mapped: 'static>(self, mapper: fn((Out,), Span) -> Mapped) -> Pat<Mapped>
        where Self: 'static {
        Pattern::single(self.con(), mapper)
    }
    pub(crate) fn mapper_failable<Mapped: 'static>(self, mapper: fn((Out,), Span) -> Result<Mapped, ParseError>) -> Pat<Mapped>
        where Self: 'static {
        Unwrapper(Pattern::single(self.con(), mapper)).pat()
    }
}

pub(crate) trait Consumer {
    type Output;
    fn consume(&self, iter: &mut TokIter) -> Result<Self::Output, ParseError>;
}

pub(crate) trait MapConsumer<Out>: Sized + Consumer {
    fn mapper<Mapped: 'static>(self, mapper: fn((Self::Output,), Span) -> Mapped) -> PatternConsumer<Mapped>
        where Self: 'static {
        PatternConsumer(Pattern::single(self, mapper))
    }
    fn mapper_failable<Mapped: 'static>(self, mapper: fn((Self::Output,), Span) -> Result<Mapped, ParseError>) -> Unwrapper<Mapped>
        where Self: 'static {
        Unwrapper(Pattern::single(self, mapper))
    }
    fn pat(self) -> Pat<Self::Output>
        where Self: 'static {
        Pattern::single(self, |(out,), _|out)
    }
}

impl<T: Consumer<Output=Out>, Out> MapConsumer<Out> for T {

}

pub(crate) struct Pattern<T: ConsumerTuple, Out> {
    name: Option<String>,
    pub(crate) consumers: T,
    mapper: fn(T::Output, Span) -> Out
}

impl<T: ConsumerTuple + 'static, Out: 'static> Pattern<T, Out> {
    pub(crate) fn inline(consumers: T, mapper: fn(T::Output, Span) -> Out) -> Pat<Out>{
        Pat(Rc::new(Box::new(Self {
            name: None,
            consumers,
            mapper
        })))
    }

    pub(crate) fn named(name: &str, consumers: T, mapper: fn(T::Output, Span) -> Out) -> Pat<Out>{
        Pat(Rc::new(Box::new(Self {
            name: Some(name.to_string()),
            consumers,
            mapper
        })))
    }
}

impl<C: Consumer + 'static, Out: 'static> Pattern<(C,), Out> {
    pub(crate) fn single(single: C, mapper: fn(<(C, ) as ConsumerTuple>::Output, Span) -> Out) -> Pat<Out> {
        Pat(Rc::new(Box::new(Self {
            name: None,
            consumers: (single, ),
            mapper
        })))
    }
}

impl Pattern<(), ()> {
    pub(crate) fn dummy() -> Pat<()>{
        Pat(Rc::new(Box::new(Self {
            name: None,
            consumers: (),
            mapper: |_, _|()
        })))
    }
}

pub(crate) trait ConsumablePattern{
    type Output;
    fn consume(&self, iter: &mut TokIter) -> Result<Self::Output, ParseError>;
}

impl<Out> ConsumablePattern for Pat<Out> {
    type Output = Out;

    fn consume(&self, iter: &mut TokIter) -> Result<Self::Output, ParseError> {
        self.0.consume(iter)
    }
}

impl<T: ConsumerTuple, Out> ConsumablePattern for Pattern<T, Out> {
    type Output = Out;

    fn consume(&self, iter: &mut TokIter) -> Result<Out, ParseError>{
        let mut span = iter.this()?.loc;
        let mut r = self.consumers.consume(iter);
        if let Some(name) = &self.name {
            r = r.e_when(format!("parsing {}", name))
        }
        span.extend(iter.nearest_point()?.end());
        Ok((self.mapper)(r?, span))
    }
}

pub(crate) trait ConsumerTuple{
    type Output;
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