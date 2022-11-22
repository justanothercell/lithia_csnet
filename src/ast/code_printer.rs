use crate::ast::ast::{AstLiteral, Expr, Expression, FullType, Func, Ident, Item, Statement, Stmt, Type, TypeT};
use crate::tokens::tokens::Literal;

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
            Literal::String(s) => format!("\"{}\"", s),
            Literal::Char(c) => format!("'{}'", c),
            Literal::Integer(i) => format!("{}", i),
            Literal::Float(f) => format!("{}", f)
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
            Expr::Literal(lit) => lit.print()
        }
    }
}

impl CodePrinter for Statement {
    fn print(&self) -> String {
        match &self.0 {
            Stmt::FuncCall(ident, args) => format!("{}({});", ident.print(), args.print_items().join(", "))
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