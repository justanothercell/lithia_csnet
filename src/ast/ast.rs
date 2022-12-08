use std::collections::HashMap;
use crate::source::{OnParseErr, ParseError, ParseET, Span};
use crate::tokens::tokens::Literal;

#[derive(Debug, Clone)]
pub(crate) enum Expr {
    Literal(AstLiteral),
    Variable(Ident),
    FuncCall(Item, Vec<Expression>),
    BinaryOp(Operator, Box<Expression>, Box<Expression>),
    UnaryOp(Operator, Box<Expression>)
}

#[derive(Debug, Clone)]
pub(crate) struct Operator(pub(crate) Op, pub(crate) Span);

#[derive(Debug, Clone)]
pub(crate) enum Op {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Not,
    LShift,
    RShift,
}

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
    Tuple(Vec<FullType>)
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
    Expression(Expression),
    VarCreate(Item, Self::mutable, Option<FullType>, Expression),
    VarAssign(Item, Option<Operator>, Expression)
}

impl Stmt {
    type mutable = bool;
}

#[derive(Debug, Clone)]
pub(crate) struct Statement(pub(crate) Stmt, pub(crate) Span);

#[derive(Debug, Clone)]
pub(crate) struct Block(pub(crate) Vec<Statement>, pub(crate) Span);

#[derive(Debug, Clone)]
pub(crate) struct Module{
    pub(crate) name: Ident,
    pub(crate) sub_modules: Map<Module>,
    pub(crate) functions: Map<Func>,
    pub(crate) loc: Span
}

pub(crate) type Map<T> = HashMap<String, T>;