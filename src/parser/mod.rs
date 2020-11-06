pub mod astnode;
use pest::Parser;
use pest::error::Error;
use pest::iterators::{Pair, Pairs};
use pest::prec_climber::{Assoc, Operator, PrecClimber};
use std::collections::LinkedList;
use crate::value::var_type::VarType;

#[derive(Parser)]
#[grammar = "grammer/oran.pest"]
pub struct OParser;

use astnode::{AstNode, CalcOp, Function, LogicalOperatorType, ComparisonlOperatorType};

fn get_pairs(result: Result<Pairs<'_, Rule>, pest::error::Error<Rule>>)
    -> Option<Pairs<'_, Rule>> {
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
            for inner_pair in pair {
                match inner_pair.as_rule() {
                    Rule::expr | Rule::expr_without_end_mark => {
                        for expr in inner_pair.into_inner() {
                            //println!("---{:?}---", expr);
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

fn build_ast_from_expr(pair: pest::iterators::Pair<Rule>) -> AstNode {
    match pair.as_rule() {
        Rule::expr => build_ast_from_expr(pair.into_inner().next().unwrap()),
        Rule::calc_term => {
            into_calc_expression(pair)
        },
        Rule::ident => {
            let str = &pair.as_str();
            AstNode::Ident(String::from(&str[..]))
        },
        Rule::string => {
            let mut text = "".to_string();
            let pairs = pair.into_inner();
            for top_pair in pairs {
                let pairs = top_pair.into_inner();
                for pair in pairs {
                    match pair.as_rule() {
                        Rule::escaped_escape_char => {
                            text.push_str(&String::from("\\"));
                        }
                        Rule::escaped_quote => {
                            text.push_str(&String::from(pair.as_str().replace("\\", "")));
                        }
                        Rule::double_quote_char | Rule::single_quote_char => { 
                            text.push_str(&String::from(pair.as_str()));
                        }
                        _ => {}
                    }
                }
            }
            AstNode::Str(text)
        },
        Rule::number | Rule::integer => {
            let num = pair.as_str().parse::<f64>().unwrap_or_else(|e| panic!("{}", e));
            AstNode::Number(f64::from(num))
        },
        Rule::val_bool => {
            match pair.into_inner().next().unwrap().as_rule() {
                Rule::bool_true => AstNode::Bool(true),
                Rule::bool_false => AstNode::Bool(false),
                _ => unreachable!()
            }
        }
        Rule::concatenated_string => {
            let strs: Vec<AstNode> = pair.into_inner().map(build_ast_from_expr).collect();
            AstNode::Strs(strs)
        },
        Rule::assgmt_expr => {
            let mut pair = pair.into_inner();
            let var_prefix = pair.next().unwrap();
            let var_type = match var_prefix.as_rule() {
                Rule::var_const => VarType::Constant,
                Rule::var_mut => VarType::VariableFirstAssigned,
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
        Rule::re_assgmt_expr => {
            let mut pair = pair.into_inner();
            let ident = pair.next().unwrap();
            let expr = pair.next().unwrap();
            let expr = build_ast_from_expr(expr);
            AstNode::Assign (
                VarType::VariableReAssigned,
                String::from(ident.as_str()),
                Box::new(expr),
            )
        },
        Rule::function_call => {
            let mut pair = pair.into_inner();
            let function_name = pair.next().unwrap();
            let function_args = pair.next();
            match function_args {
                None => {
                    function_call(function_name, vec![AstNode::Null])
                },
                _ => {
                    let expr = function_args.unwrap();
                    let args: Vec<AstNode> = expr.into_inner().map(build_ast_from_expr).collect();
                    function_call(function_name, args)
                }
            }
        },       
        Rule::function_define => {
            let mut function_name = String::from("");
            let mut arguments: Vec<AstNode> = Vec::new();
            let mut fn_return: Box<AstNode> = Box::new(AstNode::Null);
            let mut body: Vec<AstNode> = Vec::new();
            //let mut public = false;

            for inner_pair in pair.into_inner() {
                match inner_pair.as_rule() {
                    Rule::function_name => {
                        function_name = String::from(inner_pair.as_str());
                        let default_funcs = vec!["print","println"];
                        if default_funcs.iter().any(|&i| i==function_name) {
                            panic!("You cannot define this function name that is same as one of default functions: {}", function_name)
                        }
                    },
                    Rule::arguments_for_define => arguments = parse_arguments(inner_pair),
                    Rule::stmt_in_function => {
                        for body_stmt in inner_pair.into_inner() {
                            body.push(build_ast_from_expr(body_stmt))
                        }
                    },
                    Rule::fn_return | Rule::last_stmt_in_function => {
                        let fn_return_stmt = inner_pair.into_inner().next().unwrap();
                        fn_return =  Box::new(build_ast_from_expr(fn_return_stmt));
                    }
                    _ => {}
                }
            }
            AstNode::FunctionDefine(function_name, arguments, body, fn_return)
        },
        Rule::argument => {
            AstNode::Argument(pair.as_str().to_string(), Box::new(AstNode::Null))
        }
        Rule::if_expr => {
            let mut pairs = pair.into_inner();
            let conditions = into_logical_expression(pairs.next().unwrap());
            let mut body: Vec<AstNode> = Vec::new();
            let mut else_if_bodies_conditions: LinkedList<(Vec<AstNode>, Vec<AstNode>)> = LinkedList::new();
            let mut else_body: Vec<AstNode> = Vec::new();
            let mut else_if_id = 0;
            for inner_pair in pairs {
                match inner_pair.as_rule() {
                    Rule::stmt_in_function => {
                        for p in inner_pair.into_inner() {
                            body.push(build_ast_from_expr(p));
                        }
                    },
                    Rule::fn_return => {
                        //
                    }
                    Rule::else_if_expr => {
                        let else_if_pairs = inner_pair.into_inner();
                        let mut else_if_condition: Vec<AstNode> = Vec::new();
                        let mut else_if_body: Vec<AstNode> = Vec::new();
                        for else_if_pair in else_if_pairs {
                            match else_if_pair.as_rule() {
                                Rule::condition | Rule::bool_operation => {
                                    else_if_condition.push(into_logical_expression(else_if_pair));
                                },
                                Rule::stmt_in_function => {
                                    let else_if_pairs = else_if_pair.into_inner();
                                    for else_if_inner_pair in else_if_pairs {
                                        else_if_body.push(build_ast_from_expr(else_if_inner_pair));
                                    }
                                },
                                Rule::fn_return => {
                                    //
                                }
                                _ => {}
                            }
                        }
                        else_if_bodies_conditions.push_back((else_if_condition, else_if_body));
                    },
                    Rule::else_expr => {
                        let else_pairs = inner_pair.into_inner();
                        for else_pair in else_pairs {
                            match else_pair.as_rule() {
                                Rule::stmt_in_function => {
                                    for p in else_pair.into_inner() {
                                        else_body.push(build_ast_from_expr(p));
                                    }
                                },
                                Rule::fn_return => {
                                    //
                                }
                                _ => {}
                            }
                        } 
                    },
                    _ => {println!("{:?}", inner_pair)}
                }
                else_if_id = else_if_id + 1;
            }
            AstNode::IF(Box::new(conditions), body, else_if_bodies_conditions, else_body)
        }
        Rule::for_expr => {
            let mut pairs = pair.into_inner();
            let ident_or_mut = pairs.next().unwrap();
            let ident: &str;
            let mut var_type = VarType::Constant;
            if ident_or_mut.as_rule() == Rule::ident {
                ident = ident_or_mut.as_str();
            } else {
                var_type = VarType::VariableFirstAssigned;
                ident = pairs.next().unwrap().as_str();
            }
            let mut range = pairs.next().unwrap().into_inner();
            let test = range.next().unwrap();
            let first_elemnt = build_ast_from_expr(test.into_inner().next().unwrap());
            let is_inclusive = match range.next().unwrap().as_rule() {
                Rule::op_dots => false,
                Rule::op_dots_inclusive => true,
                unknown_expr => panic!("Unexpected expression: {:?}", unknown_expr),
            };
            let last_elemnt = build_ast_from_expr(range.next().unwrap().into_inner().next().unwrap());
            let mut stmt_in_function: Vec<AstNode> = Vec::new();
            for pair in pairs {
                let pair = pair.into_inner().next().unwrap();
                stmt_in_function.push(build_ast_from_expr(pair));
            }
            AstNode::ForLoop(is_inclusive, var_type, ident.to_string(), Box::new(first_elemnt), Box::new(last_elemnt), stmt_in_function)
        },
        unknown_expr => panic!("Unexpected expression: {:?}", unknown_expr),
    }
}

fn parse_arguments(arguments: Pair<Rule>) -> Vec<AstNode> {
    let mut args: Vec<AstNode> = Vec::new();

    for arg in arguments.into_inner() {
        args.push(build_ast_from_expr(arg));
    }

    args
}

fn function_call (fn_name: Pair<'_, Rule>, arg_values: Vec<AstNode>) -> AstNode {
    match fn_name.as_str() {
        "print" => AstNode::FunctionCall(Function::Print, "".to_owned(), arg_values),
        "println" => AstNode::FunctionCall(Function::Println, "".to_owned(), arg_values),
        _ => AstNode::FunctionCall(Function::NotDefault, fn_name.as_str().to_string(), arg_values),
    }
}

fn into_logical_expression(pair: Pair<Rule>) -> AstNode {
    let climber = PrecClimber::new(vec![
        Operator::new(Rule::op_or, Assoc::Left),
        Operator::new(Rule::op_and, Assoc::Left),
    ]);

    logical_consume(pair, &climber)
}

fn into_calc_expression(pair: Pair<Rule>) -> AstNode {
    let climber = PrecClimber::new(vec![
        Operator::new(Rule::plus, Assoc::Left) | Operator::new(Rule::minus, Assoc::Left),
        Operator::new(Rule::times, Assoc::Left) | Operator::new(Rule::divide, Assoc::Left) | Operator::new(Rule::modulus, Assoc::Left),
        Operator::new(Rule::power, Assoc::Right),
    ]);

    calc_consume(pair, &climber)
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

fn logical_consume(pair: Pair<Rule>, climber: &PrecClimber<Rule>) -> AstNode {
    match pair.as_rule() {
        Rule::condition => {
            let pairs = pair.into_inner();
            climber.climb(pairs, |pair| logical_consume(pair, climber), get_op_ast_node)
        }
        Rule::bool_operation => {
            let newpair = pair.into_inner().next().map(|pair| logical_consume(pair, climber)).unwrap();
            newpair
        }
        Rule::comparison => {
            let mut inner_pairs = pair.into_inner();
            let element = build_ast_from_expr(inner_pairs.next().unwrap());
            let compare = inner_pairs.next().unwrap();
            let other = build_ast_from_expr(inner_pairs.next().unwrap());
            let compare_type = match compare.as_rule() {
                Rule::two_equals => LogicalOperatorType::Equal,
                Rule::bigger_than => LogicalOperatorType::BiggerThan,
                Rule::smaller_than => LogicalOperatorType::SmallerThan,
                Rule::e_bigger_than => LogicalOperatorType::EbiggerThan,
                Rule::e_smaller_than => LogicalOperatorType::EsmallerThan,
                _ => panic!("Unknown rule: {:?}", compare),
            };
            AstNode::Comparison(Box::new(element), compare_type, Box::new(other))
        }
        Rule::number => {
            let number = pair.as_str().parse().unwrap();
            AstNode::Number(number)
        }
        Rule::string => {
            let str = &pair.as_str();
            // Strip leading and ending quotes.
            let str = &str[1..str.len() - 1];
            let number = str.parse().unwrap();
            AstNode::Number(number)
        }
        Rule::ident => {
            let ident = pair.as_str();
            AstNode::Ident(ident.to_string())
        }
        Rule::function_call => {
            let mut pair = pair.into_inner();
            let function_name = pair.next().unwrap();
            let next = pair.next();
            match next {
                None => {
                    function_call(function_name, vec![AstNode::Null])
                },
                _ => {
                    let expr = next.unwrap();
                    let args: Vec<AstNode> = expr.into_inner().map(build_ast_from_expr).collect();
                    function_call(function_name, args)
                }
            }
        }
        Rule::val_bool => {
            match pair.into_inner().next().unwrap().as_rule() {
                Rule::bool_true => AstNode::Bool(true),
                Rule::bool_false => AstNode::Bool(false),
                _ => unreachable!()
            }
        }
        _ => panic!("Unknown rule: {:?}", pair),
    }
}

fn calc_consume(pair: Pair<Rule>, climber: &PrecClimber<Rule>) -> AstNode {
    match pair.as_rule() {
        Rule::calc_term => {
            let pairs = pair.into_inner();
            climber.climb(pairs, |pair| calc_consume(pair, climber), get_op_ast_node)
        }
        Rule::element => {
            let newpair = pair.into_inner().next().map(|pair| calc_consume(pair, climber)).unwrap();
            newpair
        },
        Rule::string => {
            let str = &pair.as_str();
            // Strip leading and ending quotes.
            let str = &str[1..str.len() - 1];
            let number = str.parse().unwrap();
            AstNode::Number(number)
        }
        Rule::ident => {
            let ident = pair.as_str();
            AstNode::Ident(ident.to_string())
        }
        Rule::number => {
            let number = pair.as_str().parse().unwrap();
            AstNode::Number(number)
        }
        Rule::function_call => {
            let mut pair = pair.into_inner();
            let function_name = pair.next().unwrap();
            let next = pair.next();
            match next {
                None => {
                    function_call(function_name, vec![AstNode::Null])
                },
                _ => {
                    let expr = next.unwrap();
                    let args: Vec<AstNode> = expr.into_inner().map(build_ast_from_expr).collect();
                    function_call(function_name, args)
                }
            }
        }
        _ => panic!("Unknown rule: {:?}", pair),
    }
}
