use crate::source::Span;
use crate::tokens::tokens::Literal;

#[derive(Debug, Clone)]
pub(crate) enum  Expr {
    Literal(AstLiteral)
}

#[derive(Debug, Clone)]
pub(crate) struct Expression(pub(crate) Expr, pub(crate) Span);

#[derive(Debug, Clone)]
pub(crate) struct AstLiteral(pub(crate) Literal, pub(crate) Span);

#[derive(Debug, Clone)]
pub(crate) struct Ident(pub(crate) String, pub(crate) Span);

#[derive(Debug, Clone)]
pub(crate) struct Item(pub(crate) Vec<Ident>, pub(crate) Span);

#[derive(Debug, Clone)]
pub(crate) struct FullType(pub(crate) TypeT, pub(crate) Span);

#[derive(Debug, Clone)]
pub(crate) enum TypeT {
    Single(Type),
    Tuple(Vec<TypeT>)
}

impl TypeT {
    pub(crate) fn empty() -> Self{
        TypeT::Tuple(vec![])
    }
    pub(crate) fn is_empty(&self) -> bool {
        if let TypeT::Tuple(ty) = self {
            ty.len() == 0
        } else {
            false
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Type {
    pub(crate) generics: Vec<FullType>,
    pub(crate) base_type: Item,
    pub(crate) loc: Span
}

#[derive(Debug, Clone)]
pub(crate) struct Func {
    pub(crate) name: Ident,
    pub(crate) args: Vec<(Ident, FullType)>,
    pub(crate) ret: FullType,
    pub(crate) body: Block,
    pub(crate) loc: Span
}

#[derive(Debug, Clone)]
pub(crate) enum Stmt {
    FuncCall(Item, Vec<Expression>),
    VarCreate(Item, Self::mutable, Option<FullType>, Expression)
}

impl Stmt {
    type mutable = bool;
}

#[derive(Debug, Clone)]
pub(crate) struct Statement(pub(crate) Stmt, pub(crate) Span);

#[derive(Debug, Clone)]
pub(crate) struct Block(pub(crate) Vec<Statement>, pub(crate) Span);