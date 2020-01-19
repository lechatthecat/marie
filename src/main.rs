extern crate pest;
#[macro_use]
extern crate pest_derive;
use pest::Parser;
use pest::error::Error;
use pest::iterators::Pair;
use pest::prec_climber::{Assoc, Operator, PrecClimber};
use std::ffi::CString;

#[derive(Parser)]
#[grammar = "parser/lexer.pest"]
pub struct OParser;

#[derive(PartialEq, Debug, Clone)]
pub enum AstNode {
    Add(Box<AstNode>, Box<AstNode>),
    Sub(Box<AstNode>, Box<AstNode>),
    Mul(Box<AstNode>, Box<AstNode>),
    Div(Box<AstNode>, Box<AstNode>),
    Assign(i32, String, Box<AstNode>),
    ReAssign(i32, String, Box<AstNode>),
    Function(DefaultFunction, Box<AstNode>),
    Ident(String),
    Str(CString),
    Term(Vec<AstNode>),
    Number(f64),
    Calc(CalcOp, Box<AstNode>, Box<AstNode>),
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum CalcOp {
    Plus,
    Minus,
    Times,
    Divide,
    Modulus,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum DefaultFunction {
    Print,
}

impl AstNode {
    fn calc<L, R>(op: CalcOp, lhs: L, rhs: R) -> Self
    where
        L: Into<AstNode>,
        R: Into<AstNode>,
    {
        AstNode::Calc(op.into(), Box::new(lhs.into()), Box::new(rhs.into()))
    }
}


fn get_pairs(result: Result<pest::iterators::Pairs<'_, Rule>, pest::error::Error<Rule>>)
    -> Option<pest::iterators::Pairs<'_, Rule>> {
    match result {
        Ok(pairs) => {
            return Some(pairs);
        },
        Err(e) => {
            println!("error: {:?}", e);
            return None;
        },
    }
}

pub fn parse(source: &str) -> Result<Vec<Box<AstNode>>, Error<Rule>> {
    let mut ast = vec![];

    let result = OParser::parse(Rule::program, source);
    let pairs = get_pairs(result);
    if pairs != None {
        for pair in pairs {
            // A pair can be converted to an iterator of the tokens which make it up:
            for inner_pair in pair {
                match inner_pair.as_rule() {
                    Rule::expr => {
                        for expr in inner_pair.into_inner() {
                            ast.push(Box::new(build_ast_from_expr(expr)));
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    Ok(ast)
}

fn main() {
    use std::collections::HashMap;
    let s = "
    //4*2+4+5*2;
    let test1 = 4*2+4+5*2;
    let test2 = 4/2+1;
    const test = 0;
    let test3 = test1 + test2 + 1;
    test = 1;
    print(test3);
    //const test2 = 2;
    //test1 + test2;
    //const test = 5+5*10;
    //let str = 'a'; //aaaaaaaaaauhiih dfgtdt
    //str = 'abc'; //aaabbbccc";
    let astnode = parse(&s).expect("unsuccessful parse");
    println!("---{:?}---", astnode);
    let mut env = HashMap::new();
    for reduced_expr in &astnode {
        //println!("{:?}", reduced_expr);
        match reduced_expr {
            // AstNode::Ident(ref var) => {
            //     let v = *env.get(&var[..]).unwrap();
            //     v as f64
            // },
            // AstNode::Assign(ref var_type, ref ident, ref expr) => {
            //     let val = num_interp_expr(env, expr);
            //     env.insert(ident, val);
            //     val
            // }
            // AstNode::Str(cstr) => cstr,
            _ => num_interp_expr(&mut env, reduced_expr)
        };
    }

    fn num_interp_expr<'a>(env: &mut HashMap<&'a str, f64>, reduced_expr: &'a AstNode) -> f64 {
        match reduced_expr {
            AstNode::Number(double) => *double,
            AstNode::Ident(ref var) => {
                let val = *env.get(&var[..]).unwrap();
                val as f64
            },
            AstNode::Assign(ref var_type, ref ident, ref expr) => {
                let val = num_interp_expr(env, expr);
                env.insert(ident, val);
                val
            }
            AstNode::ReAssign(ref var_type, ref ident, ref expr) => {
                let val = num_interp_expr(env, expr);
                env.insert(ident, val);
                val
            }
            AstNode::Calc (ref verb, ref lhs, ref rhs ) => {
                match verb {
                    CalcOp::Plus => { num_interp_expr(env, lhs) + num_interp_expr(env, rhs) }
                    CalcOp::Minus => { num_interp_expr(env, lhs) - num_interp_expr(env, rhs) }
                    CalcOp::Times => { num_interp_expr(env, lhs) * num_interp_expr(env, rhs) }
                    CalcOp::Divide => { num_interp_expr(env, lhs) / num_interp_expr(env, rhs) }
                    CalcOp::Modulus => { num_interp_expr(env, lhs) % num_interp_expr(env, rhs) }
                }
            },
            AstNode::Function(ref func, ref e) => {
                match func {
                    DefaultFunction::Print => {
                        let val = num_interp_expr(env, e);
                        println!("{}", val);
                        val
                    },
                }
            },
            _ => {
                1.0 // true
            },
        }
    }
}

fn build_ast_from_expr(pair: pest::iterators::Pair<Rule>) -> AstNode {
    match pair.as_rule() {
        Rule::expr => build_ast_from_expr(pair.into_inner().next().unwrap()),
        Rule::term => {
            into_expression(pair)
        }
        Rule::string => {
            let str = &pair.as_str();
            // Strip leading and ending quotes.
            let str = &str[1..str.len() - 1];
            // Escaped string quotes become single quotes here.
            let str = str.replace("''", "'");
            AstNode::Str(CString::new(&str[..]).unwrap())
        }
        Rule::assgmtExpr => {
            let mut pair = pair.into_inner();
            let var_prefix = pair.next().unwrap();
            let var_type = match var_prefix.as_str() {
                "const" => 0, // const
                "let" => 1, // let
                _ => panic!("unknown variable type: {:?}", var_prefix)
            };
            let ident = pair.next().unwrap();
            let expr = pair.next().unwrap();
            let expr = build_ast_from_expr(expr);
            AstNode::Assign (
                var_type,
                String::from(ident.as_str()),
                Box::new(expr),
            )
        }
        Rule::reAssgmtExpr => {
            let mut pair = pair.into_inner();
            let ident = pair.next().unwrap();
            let expr = pair.next().unwrap();
            let expr = build_ast_from_expr(expr);
            AstNode::ReAssign (
                2, //re-assign
                String::from(ident.as_str()),
                Box::new(expr),
            )
        }
        Rule::function => {
            let mut pair = pair.into_inner();
            let functionName = pair.next().unwrap();
            let expr = pair.next().unwrap();
            let expr = build_ast_from_expr(expr);
            match functionName.as_str() {
                "print" => AstNode::Function(DefaultFunction::Print, Box::new(expr)),
                _ => panic!("Unknown function: {:?}", functionName),
            }
        }
        unknown_expr => panic!("Unexpected expression: {:?}", unknown_expr),
    }
}

fn into_expression(pair: Pair<Rule>) -> AstNode {
    let climber = PrecClimber::new(vec![
        Operator::new(Rule::plus, Assoc::Left) |
            Operator::new(Rule::minus, Assoc::Left),
        Operator::new(Rule::times, Assoc::Left) | Operator::new(Rule::divide, Assoc::Left) |
            Operator::new(Rule::modulus, Assoc::Left),
    ]);

    consume(pair, &climber)
}

fn consume(pair: Pair<Rule>, climber: &PrecClimber<Rule>) -> AstNode {
    // println!("Rule: {:?}", pair.as_rule());
    // println!("Text: {:?}", pair.as_str());
    // println!();

    let primary = |pair| consume(pair, climber);
    let calc = |lhs, op: Pair<Rule>, rhs| match op.as_rule() {
        Rule::plus => AstNode::calc(CalcOp::Plus, lhs, rhs),
        Rule::minus => AstNode::calc(CalcOp::Minus, lhs, rhs),
        Rule::times => AstNode::calc(CalcOp::Times, lhs, rhs),
        Rule::divide => AstNode::calc(CalcOp::Divide, lhs, rhs),
        Rule::modulus => AstNode::calc(CalcOp::Modulus, lhs, rhs),
        _ => unreachable!(),
    };
    match pair.as_rule() {
        Rule::term => {
            let pairs = pair.into_inner();
            climber.climb(pairs, primary, calc)
        }
        Rule::primary => {
            let newpair = pair.into_inner().next().map(primary).unwrap();
            //println!("Rule: {:?}", newpair);
            newpair
        },
        Rule::number => {
            let number = pair.as_str().parse().unwrap();
            //println!("Rule: {:?}", number);
            AstNode::Number(number)
        }
        Rule::ident => {
            let ident = pair.as_str();
            //println!("Rule: {:?}", ident);
            AstNode::Ident(ident.to_string())
        }
        _ => unreachable!(),
    }
}
