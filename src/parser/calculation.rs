use pest::{iterators::Pair, prec_climber::{Assoc, Operator, PrecClimber}};
use std::process;
use super::{Rule, astnode::{AstNode, CalcOp, ComparisonlOperatorType, LogicalOperatorType}};
use super::function;
use super::ast_build;
use colored::*;

/**
 * This part was created by refering to 
 * https://github.com/ubnt-intrepid/pest-calculator
 * Copyright (c) 2017 Yusuke Sasaki
 * But a bit modified.
*/

pub fn into_logical_expression(location:(String, usize, usize), pair: Pair<Rule>) -> AstNode {
    let climber = PrecClimber::new(vec![
        Operator::new(Rule::op_or, Assoc::Left),
        Operator::new(Rule::op_and, Assoc::Left),
    ]);

    logical_consume(location, pair, &climber)
}

pub fn into_calc_expression(location:(String, usize, usize), pair: Pair<Rule>) -> AstNode {
    let climber = PrecClimber::new(vec![
        Operator::new(Rule::plus, Assoc::Left) | Operator::new(Rule::minus, Assoc::Left),
        Operator::new(Rule::times, Assoc::Left) | Operator::new(Rule::divide, Assoc::Left) | Operator::new(Rule::modulus, Assoc::Left),
        Operator::new(Rule::power, Assoc::Right),
    ]);

    calc_consume(location, pair, &climber)
}

fn get_op_ast_node (lhs: AstNode, op: Pair<Rule>, rhs: AstNode) -> AstNode {
    match op.as_rule() {
        Rule::op_and => AstNode::condition(ComparisonlOperatorType::AND, lhs, rhs),
        Rule::op_or => AstNode::condition(ComparisonlOperatorType::OR, lhs, rhs),
        Rule::plus => AstNode::calculation(CalcOp::Plus, lhs, rhs),
        Rule::minus => AstNode::calculation(CalcOp::Minus, lhs, rhs),
        Rule::times => AstNode::calculation(CalcOp::Times, lhs, rhs),
        Rule::divide => AstNode::calculation(CalcOp::Divide, lhs, rhs),
        Rule::modulus => AstNode::calculation(CalcOp::Modulus, lhs, rhs),
        Rule::power => AstNode::calculation(CalcOp::Power, lhs, rhs),
        _ => unreachable!(),
    }
}

fn logical_consume(location: (String, usize, usize), pair: Pair<Rule>, climber: &PrecClimber<Rule>) -> AstNode {
    match pair.as_rule() {
        Rule::condition => {
            let pairs = pair.into_inner();
            climber.climb(pairs, |pair| logical_consume(location.clone(), pair, climber), get_op_ast_node)
        }
        Rule::bool_operation => {
            let newpair = pair.into_inner().next().map(|pair| logical_consume(location, pair, climber)).unwrap();
            newpair
        }
        Rule::comparison => {
            let mut inner_pairs = pair.into_inner();
            let element = ast_build::build_ast_from_expr(location.clone(), inner_pairs.next().unwrap());
            let compare = inner_pairs.next().unwrap();
            let other = ast_build::build_ast_from_expr(location.clone(), inner_pairs.next().unwrap());
            let compare_type = match compare.as_rule() {
                Rule::two_equals => LogicalOperatorType::Equal,
                Rule::bigger_than => LogicalOperatorType::BiggerThan,
                Rule::smaller_than => LogicalOperatorType::SmallerThan,
                Rule::e_bigger_than => LogicalOperatorType::EbiggerThan,
                Rule::e_smaller_than => LogicalOperatorType::EsmallerThan,
                _ => panic!("Unknown rule: {:?}", compare),
            };
            AstNode::Comparison(location, Box::new(element), compare_type, Box::new(other))
        }
        Rule::number => {
            let number = pair.as_str().parse().unwrap();
            AstNode::Number(location, number)
        }
        Rule::array_element => {
            let mut pairs = pair.into_inner();
            let array = Box::new(ast_build::build_ast_from_expr(location.clone(),pairs.next().unwrap()));
            let indexes: Vec<AstNode> = pairs.map(|v| ast_build::build_ast_from_expr(location.clone(), v.into_inner().next().unwrap())).collect();
            AstNode::ArrayElement(location, 
                array,
                indexes
            )
        }
        Rule::string => {
            let str = &pair.as_str();
            // Strip leading and ending quotes.
            let str = &str[1..str.len() - 1];
            let number = str.parse().unwrap();
            AstNode::Number(location, number)
        }
        Rule::ident => {
            let ident = pair.as_str();
            AstNode::Ident(location, ident.to_string())
        }
        Rule::function_call => {
            let mut pair = pair.into_inner();
            let function_name = pair.next().unwrap();
            let next = pair.next();
            match next {
                None => {
                    function::function_call(location, function_name, vec![AstNode::Null])
                },
                _ => {
                    let expr = next.unwrap();
                    let args: Vec<AstNode> = expr.into_inner().map(|v| ast_build::build_ast_from_expr(location.clone(), v)).collect();
                    function::function_call(location, function_name, args)
                }
            }
        }
        Rule::val_bool => {
            match pair.into_inner().next().unwrap().as_rule() {
                Rule::bool_true => AstNode::Bool(location, true),
                Rule::bool_false => AstNode::Bool(location, false),
                _ => unreachable!()
            }
        }
        _ => panic!("Unknown rule: {:?}", pair),
    }
}

fn calc_consume(location: (String, usize, usize), pair: Pair<Rule>, climber: &PrecClimber<Rule>) -> AstNode {
    match pair.as_rule() {
        Rule::calc_term => {
            let pairs = pair.into_inner();
            climber.climb(pairs, |pair| calc_consume(location.clone(), pair, climber), get_op_ast_node)
        }
        Rule::element => {
            let newpair = pair.into_inner().next().map(|pair| calc_consume(location, pair, climber)).unwrap();
            newpair
        },
        Rule::string => {
            let str = &pair.as_str();
            // Strip leading and ending quotes.
            let str = &str[1..str.len() - 1];
            let number = str.parse().unwrap_or_else(|_x|{
                println!("{}\n{}\nLine number: {}, column number:{}: This \"{}\" is not a number.",
                    "Error!".red().bold(),    
                    location.0,    
                    location.1,
                    location.2,
                    str
                );
                process::exit(1);
            });
            AstNode::Number(location, number)
        }
        Rule::array_element => {
            let mut pairs = pair.into_inner();
            let array = Box::new(ast_build::build_ast_from_expr(location.clone(),pairs.next().unwrap()));
            let indexes: Vec<AstNode> = pairs.map(|v| ast_build::build_ast_from_expr(location.clone(), v.into_inner().next().unwrap())).collect();
            AstNode::ArrayElement(location, 
                array,
                indexes
            )
        }
        Rule::ident => {
            let ident = pair.as_str();
            AstNode::Ident(location, ident.to_string())
        }
        Rule::number => {
            let number = pair.as_str().parse().unwrap();
            AstNode::Number(location, number)
        }
        Rule::function_call => {
            let mut pair = pair.into_inner();
            let function_name = pair.next().unwrap();
            let next = pair.next();
            match next {
                None => {
                    function::function_call(location, function_name, vec![AstNode::Null])
                },
                _ => {
                    let expr = next.unwrap();
                    let args: Vec<AstNode> = expr.into_inner().map(|v| ast_build::build_ast_from_expr(location.clone(), v)).collect();
                    function::function_call(location, function_name, args)
                }
            }
        }
        _ => panic!("Unknown rule: {:?}", pair),
    }
}
