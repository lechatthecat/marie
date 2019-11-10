extern crate pest;
#[macro_use]
extern crate pest_derive;
use pest::Parser;

#[derive(Parser)]
#[grammar = "parser/lexer.pest"]
pub struct parser;

mod lexer {
    #[derive(Debug, PartialEq, Clone, Copy)]
    pub enum Token {
        End,
        Error,
        Text,
        Print,
        Integer,
        OneEqual,
        TwoEqual,
        Let,
        Plus,
        Minus,
        Times,
        Divide,
        LParen,
        RParen,
        Semi,
        Whitespace,
        Comment,
    }
}

mod ast {
    #[derive(Debug)]
    pub enum AstNode {
        Add(Box<AstNode>, Box<AstNode>),
        Sub(Box<AstNode>, Box<AstNode>),
        Mul(Box<AstNode>, Box<AstNode>),
        Div(Box<AstNode>, Box<AstNode>),
        Assign(String, Box<AstNode>),
        Print(Box<AstNode>),
        Ident(String),
        Str(String),
    }
}

fn main() {
    use std::collections::HashMap;
    use std::iter::Iterator;

    let s = "let test5 = 5;let test = 5+5; //aaaaaaaaaauhiih dfgtdt";
    let parse = parser::parse(Rule::program, s);
    println!("{:?}", parse);

    // pub fn interp<'a>(p: &'a Program) {
    //     let mut env = HashMap::new();
    //     for expr in &p.stmts {
    //         interp_expr(&mut env, expr);
    //     }
    // }
    // fn interp_expr<'a>(env: &mut HashMap<&'a str, i64>, expr: &'a Expr) -> i64 {
    //     use crate::ast::Expr_::*;
    //     match expr.node {
    //         Add(ref a, ref b) => interp_expr(env, a) + interp_expr(env, b),
    //         Sub(ref a, ref b) => interp_expr(env, a) - interp_expr(env, b),
    //         Mul(ref a, ref b) => interp_expr(env, a) * interp_expr(env, b),
    //         Div(ref a, ref b) => interp_expr(env, a) / interp_expr(env, b),
    //         Assign(ref var, ref b) => {
    //             let val = interp_expr(env, b);
    //             env.insert(var, val);
    //             val
    //         }
    //         Var(ref var) => *env.get(&var[..]).unwrap(),
    //         Literal(lit) => lit,
    //         Print(ref e) => {
    //             let val = interp_expr(env, e);
    //             println!("{}", val);
    //             val
    //         }
    //     }
    // }
}
