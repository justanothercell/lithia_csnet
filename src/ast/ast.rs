use std::collections::HashMap;
use crate::ast::code_printer::CodePrinter;
use crate::source::{OnParseErr, ParseError, ParseET, Span};
use crate::tokens::tokens::Literal;

pub(crate) trait AstLike {}
impl AstLike for dyn Ast {}
impl AstLike for dyn AstFragment {}
pub(crate) trait Ast: CodePrinter {}
pub(crate) trait AstFragment {}
macro_rules! ast {
    ($enum_or_struct: ident $ty: ident $($tt: tt)*) => {
        #[derive(Debug, Clone)]
        pub(crate) $enum_or_struct $ty $($tt)*
        impl AstFragment for $ty {}
    };
}

macro_rules! ast_fragment {
    ($enum_or_struct: ident $ty: ident $($tt: tt)*) => {
        #[derive(Debug, Clone)]
        pub(crate) $enum_or_struct $ty $($tt)*
        impl AstFragment for $ty {}
    };
}


ast!{enum Expr {
    Literal(AstLiteral),
    Variable(Ident),
    FuncCall(Item, Vec<Expression>),
    BinaryOp(Operator, Box<Expression>, Box<Expression>),
    UnaryOp(Operator, Box<Expression>)
}}

ast!{struct Operator(pub(crate) Op, pub(crate) Span);}

ast_fragment!{enum Op {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Not,
    LShift,
    RShift,
}}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum OpGroup {
    Dot,
    Dash,
    Bin,
    Bool,
}

impl Op {
    pub(crate) fn from_chars(chars: Vec<char>, loc: Option<Span>) -> Result<Op, ParseError>{
        Ok(match chars.iter().collect::<String>().as_str() {
            "+" => Op::Add,
            "-" => Op::Sub,
            "*" => Op::Mul,
            "/" => Op::Div,
            "&&" => Op::And,
            "||" => Op::Or,
            "!" => Op::Not,
            "<<" => Op::LShift,
            ">>" => Op::RShift,
            op => {
                let et = ParseET::ParsingError(format!("operator '{op}' not recognized"));
                return Err(if let Some(span) = loc {
                    et.at(span)
                } else {
                    et.error()
                }).e_when(String::from("parsing operator"))
            }
        })
    }

    pub(crate) fn from_chars_multi(mut chars: Vec<char>, loc: Option<Span>) -> Result<Vec<Op>, ParseError>{
        let original = chars.iter().collect::<String>();
        let mut ops = vec![];
        while chars.len() > 0 {
            let mut secondary = vec![];
            while chars.len() > 0 {
                if let Ok(op) = Op::from_chars(chars.clone(), None) {
                    ops.push(op);
                    break
                } else {}
                secondary.insert(0, chars.pop().unwrap())
            }
            if chars.len() == 0 {
                let et = ParseET::ParsingError(format!("operator '{original}' not recognized", ));
                return Err(if let Some(span) = loc {
                    et.at(span)
                } else {
                    et.error()
                }).e_when(String::from("parsing (multi) operator"))
            }
            chars = secondary;
        }
        Ok(ops)
    }

    pub(crate) fn group(&self) -> OpGroup {
        match self {
            Op::Add => OpGroup::Dash,
            Op::Sub => OpGroup::Dash,
            Op::Mul => OpGroup::Dot,
            Op::Div => OpGroup::Dot,
            Op::And => OpGroup::Bool,
            Op::Or => OpGroup::Bool,
            Op::Not => OpGroup::Bool,
            Op::LShift => OpGroup::Bin,
            Op::RShift => OpGroup::Bin,
        }
    }
}



ast!{struct Expression(pub(crate) Expr, pub(crate) Span);}

ast!{struct AstLiteral(pub(crate) Literal, pub(crate) Span);}

ast!{struct Ident(pub(crate) String, pub(crate) Span);}

ast!{struct Item(pub(crate) Vec<Ident>, pub(crate) Span);}

ast!{struct FullType(pub(crate) TypeT, pub(crate) Span);}

ast!{enum TypeT {
    Single(Type),
    Tuple(Vec<FullType>)
}}

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

ast!{struct Type {
    pub(crate) generics: Vec<FullType>,
    pub(crate) base_type: Item,
    pub(crate) loc: Span
}}

ast!{struct Func {
    pub(crate) name: Ident,
    pub(crate) args: Vec<(Ident, FullType)>,
    pub(crate) ret: FullType,
    pub(crate) body: Block,
    pub(crate) loc: Span
}}

ast!{enum Stmt {
    Expression(Expression),
    VarCreate(Item, Self::mutable, Option<FullType>, Expression),
    VarAssign(Item, Option<Operator>, Expression)
}}

impl Stmt {
    type mutable = bool;
}

ast!{struct Statement(pub(crate) Stmt, pub(crate) Span);}

ast_fragment!{struct Block(pub(crate) Vec<Statement>, pub(crate) Span);}

ast!{struct Module{
    pub(crate) name: Ident,
    pub(crate) sub_modules: Map<Module>,
    pub(crate) functions: Map<Func>,
    pub(crate) loc: Span
}}

pub(crate) type Map<T> = HashMap<String, T>;