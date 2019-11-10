extern crate logos;

mod lexer {
    use logos::Logos;
    
    #[derive(Logos, Debug, PartialEq, Copy, Clone)]
    pub enum Token {
        #[end]
        End,

        #[error]
        Error,
        
        #[regex = "[a-zA-Z0-9_]+"]
        Text,

        #[token = "print"]
        Print,

        #[regex = "[0-9]+"]
        Integer,

        #[token = "="]
        OneEqual,

        #[token = "=="]
        TwoEqual,

        #[token = "let"]
        Let,
        
        #[token = "+"]
        Plus,

        #[token = "-"]
        Minus,

        #[token = "*"]
        Star,

        #[token = "/"]
        Slash,

        #[token = "("]
        LParen,

        #[token = ")"]
        RParen,

        #[token = ";"]
        Semi,

        #[regex = "[ \t\r\n]"]
        Whitespace,

        #[regex = "//[^\n]*"]
        Comment,

        #[regex = "/[*](~(.*[*]/.*))[*]/"]
        LongComment,
    }
}

mod ast {
    use logos::Logos;

    #[derive(Debug)]
    pub struct Program {
        pub stmts: Vec<Expr>,
    }

    #[derive(Debug)]
    pub struct Expr {
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

mod parser {
    use crate::ast::*;
    use crate::lexer::Token::*;
    use crate::lexer::*;

    use logos::Logos;



}

fn take<T>(mut vec: Vec<T>, index: usize) -> Option<T> {
    if vec.get(index).is_none() {
        None
    } else {
        Some(vec.swap_remove(index))
    }
}

fn main() {
    use logos::Logos;
    use logos::Lexer;
    use crate::lexer::Token;
    use crate::lexer::Token::*;
    use crate::parser::*;
    use crate::ast::*;
    use std::collections::HashMap;
    use std::iter::Iterator;

    pub fn interp<'a>(p: &'a Program) {
        let mut env = HashMap::new();
        for expr in &p.stmts {
            interp_expr(&mut env, expr);
        }
    }
    fn interp_expr<'a>(env: &mut HashMap<&'a str, i64>, expr: &'a Expr) -> i64 {
        use crate::ast::Expr_::*;
        match expr.node {
            Add(ref a, ref b) => interp_expr(env, a) + interp_expr(env, b),
            Sub(ref a, ref b) => interp_expr(env, a) - interp_expr(env, b),
            Mul(ref a, ref b) => interp_expr(env, a) * interp_expr(env, b),
            Div(ref a, ref b) => interp_expr(env, a) / interp_expr(env, b),
            Assign(ref var, ref b) => {
                let val = interp_expr(env, b);
                env.insert(var, val);
                val
            }
            Var(ref var) => *env.get(&var[..]).unwrap(),
            Literal(lit) => lit,
            Print(ref e) => {
                let val = interp_expr(env, e);
                println!("{}", val);
                val
            }
        }
    }

    fn get_expr(lexer : &Lexer<Token, &str>){
        match lexer.token {
            Token::Whitespace | Token::Comment | Token::LongComment => {},
            Token::Semi=> {
                // tokenStrs.clear();
                // tokens.clear();
                eprintln!("tok: {:?}, code: {}, range: {:?}", lexer.token, lexer.slice(), lexer.range());
                //return None;
            },
            Token::Plus=> {
                eprintln!("tok: {:?}, code: {}, range: {:?}", lexer.token, lexer.slice(), lexer.range());
                // let previousNum = &tokenStrs[tokenStrs.len()-1];
                // lexer.advance();
                // let currentNum  = lexer.slice().to_string();
                //eprintln!("{} + {} = ", previousInt, currentInt);
                //return None;
            },
            Token::End => {
                //eprintln!("tok: {:?}, code: {}, range: {:?}", lexer.token, lexer.slice(), lexer.range());
                //return None;
            },
            _ => {
                eprintln!("tok: {:?}, code: {}, range: {:?}", lexer.token, lexer.slice(), lexer.range());
                //return Some(lexer);
            }
        }
    }

    //lexer::morph();
    let mut tokens : Vec<Token> = Vec::new();
    let mut tokenStrs : Vec<String> = Vec::new();
    let mut lexer = Token::lexer("let test5 = 5;let test = 5+5; //aaaaaaaaaauhiih dfgtdt");

    loop {
        get_expr(&lexer);
        if (lexer.token == Token::End) {
            break;
        }
        lexer.advance();
    }
}
