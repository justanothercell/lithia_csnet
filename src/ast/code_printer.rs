use crate::ast::ast::{AstLiteral, Expr, Expression, FullType, Func, Ident, Item, Op, Operator, Statement, Stmt, Type, TypeT};
use crate::tokens::tokens::{Literal, NumLit};

pub(crate) trait CodePrinter{
    fn print(&self) -> String;
}

trait VecPrinter{
    fn print_items(&self) -> Vec<String>;
}

impl<T: CodePrinter> VecPrinter for Vec<T> {
    fn print_items(&self) -> Vec<String> {
        self.iter().map(|t| t.print()).collect()
    }
}

impl CodePrinter for TypeT {
    fn print(&self) -> String {
        match self {
            TypeT::Single(ty) => ty.print(),
            TypeT::Tuple(types) => format!("({})", types.print_items().join(", "))
        }
    }
}

impl CodePrinter for FullType {
    fn print(&self) -> String {
        self.0.print()
    }
}

impl CodePrinter for Type {
    fn print(&self) -> String {
        if self.generics.len() == 0{
            format!("{}", self.base_type.print())
        }
        else {
            format!("{}<{}>", self.base_type.print(), self.generics.print_items().join(", "))
        }
    }
}

impl CodePrinter for Ident {
    fn print(&self) -> String {
        self.0.clone()
    }
}

impl CodePrinter for Item {
    fn print(&self) -> String {
        self.0.print_items().join("::")
    }
}

impl CodePrinter for Literal {
    fn print(&self) -> String {
        match self {
            Literal::String(s) => format!("{s:?}"),
            Literal::Char(c) => format!("{c:?}"),
            Literal::Number(NumLit::Integer(i), ty) => format!("{i}{}", ty.as_ref().map_or(String::new(), |t| format!("{t}"))),
            Literal::Number(NumLit::Float(f), ty) => format!("{f}{}", ty.as_ref().map_or(String::new(), |t| format!("{t}"))),
            Literal::Bool(b) => format!("{b}"),
        }
    }
}

impl CodePrinter for AstLiteral {
    fn print(&self) -> String {
        self.0.print()
    }
}

impl CodePrinter for Expression {
    fn print(&self) -> String {
        match &self.0 {
            Expr::Literal(lit) => lit.print(),
            Expr::Variable(var) => var.print(),
            Expr::UnaryOp(op, box expr) => format!("{}{}", op.print(), expr.print()),
            Expr::BinaryOp(op, box left, box right) => format!("{} {} {}", left.print(), op.print(), right.print())
        }
    }
}

impl CodePrinter for Operator {
    fn print(&self) -> String {
        match self.0 {
            Op::Add => "+",
            Op::Sub => "-",
            Op::Mul => "*",
            Op::Div => "/",
            Op::LShift => "<<",
            Op::RShift => ">>"
        }.to_string()
    }
}

impl CodePrinter for Statement {
    fn print(&self) -> String {
        match &self.0 {
            Stmt::FuncCall(ident, args) => format!("{}({});", ident.print(), args.print_items().join(", ")),
            Stmt::VarCreate(ident, mutable, ty, expr) =>
                format!("let {}{}{} = {};",
                        if *mutable { "mut "} else {""}.to_string(),
                        ident.print(),
                        ty.as_ref().map(|t|format!(": {}", t.print())).unwrap_or("".to_string()),
                        expr.print()
                ),
            Stmt::VarAssign(ident, Some(op), expr) => format!("{} {}= {};", ident.print(), op.print(), expr.print()),
            Stmt::VarAssign(ident, None, expr) => format!("{} = {};", ident.print(), expr.print())
        }
    }
}

impl CodePrinter for Func {
    fn print(&self) -> String {
        format!("fn {}({}){} {{{}}}",
                self.name.print(),
                self.args.iter().map(|(ident, ty)| format!("{}: {}", ident.print(), ty.0.print())).collect::<Vec<String>>().join(", "),
                if self.ret.0.is_empty() {
                    String::new()
                } else {
                    format!(" -> {}", self.ret.print())
                },
                if self.body.0.is_empty() {
                    String::new()
                } else {
                    format!("\n    {}\n", self.body.0.print_items().join("\n    "))
                }
        )
    }
}