extern crate plex;

pub mod base {
    use crate::lang::lexer::base::Span;

    #[derive(Debug)]
    pub struct Program {
        pub stmts: Vec<Expr>,
    }

    #[derive(Debug)]
    pub struct Expr {
        pub span: Span,
        pub node: Expr_,
    }

    #[derive(Debug)]
    pub enum Expr_ {
        Add(Box<Expr>, Box<Expr>),
        Sub(Box<Expr>, Box<Expr>),
        Mul(Box<Expr>, Box<Expr>),
        Div(Box<Expr>, Box<Expr>),
        Var(String),
        Assign(String, Box<Expr>),
        Print(Box<Expr>),
        Literal(i64),
    }
}
