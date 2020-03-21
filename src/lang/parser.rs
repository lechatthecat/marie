use pest::Parser;
use pest::error::Error;
use pest::iterators::{Pair, Pairs};
use pest::prec_climber::{Assoc, Operator, PrecClimber};
use super::constant::{VARTYPE_CONSTANT, VARTYPE_VARIABLE, VARTYPE_REASSIGNED};

#[derive(Parser)]
#[grammar = "grammer/oran.pest"]
pub struct OParser;

use super::astnode::{AstNode, CalcOp, Function};

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
            into_expression(pair)
        },
        Rule::element => {
            let pair = pair.into_inner().next().unwrap();
            match pair.as_rule() {
                Rule::calc_term => {
                    into_expression(pair)
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
                                _ => { 
                                    text.push_str(&String::from(pair.as_str()));
                                }
                            }
                        }
                    }
                    AstNode::Str(text)
                },
                Rule::number => {
                    let num = pair.as_str().parse::<f64>().unwrap_or_else(|e| panic!("{}", e));
                    AstNode::Number(f64::from(num))
                },
                unknown_expr => panic!("Unexpected expression: {:?}", unknown_expr),
            }
        },
        Rule::concatenated_string => {
            let strs: Vec<AstNode> = pair.into_inner().map(build_ast_from_expr).collect();
            AstNode::Strs(strs)
        },
        Rule::assgmt_expr => {
            let mut pair = pair.into_inner();
            let var_prefix = pair.next().unwrap();
            let var_type = match var_prefix.as_str() {
                "const" => VARTYPE_CONSTANT,
                "let" => VARTYPE_VARIABLE,
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
                VARTYPE_REASSIGNED,
                String::from(ident.as_str()),
                Box::new(expr),
            )
        },
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
        },       
        Rule::function_define => {
            let mut function_name = String::from("");
            let mut arguments: Vec<AstNode> = Vec::new();
            let mut fn_return: Vec<AstNode> = Vec::new();
            let mut body: Vec<AstNode> = Vec::new();
            //let mut public = false;

            let inner_pairs = pair.into_inner();
            for inner_pair in inner_pairs {
                match inner_pair.as_rule() {
                    Rule::function_name => function_name = String::from(inner_pair.as_str()),
                    Rule::arguments => arguments = parse_arguments(inner_pair),
                    Rule::stmt_in_function => {
                        for body_stmt in inner_pair.into_inner() {
                            body.push(build_ast_from_expr(body_stmt))
                        }
                    },
                    Rule::fn_return | Rule::last_stmt_in_function => {
                        for fn_return_stmt in inner_pair.into_inner() {
                            fn_return.push(build_ast_from_expr(fn_return_stmt))
                        }
                    }
                    _ => {}
                }
            }
            AstNode::FunctionDefine(function_name, arguments, body, fn_return)
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

fn function_call (fn_name: Pair<'_, Rule>, args: Vec<AstNode>) -> AstNode {
    match fn_name.as_str() {
        "print" => AstNode::FunctionCall(Function::Print, "".to_owned(), args),
        "println" => AstNode::FunctionCall(Function::Println, "".to_owned(), args),
        _ => AstNode::FunctionCall(Function::NotDefault, fn_name.as_str().to_string(), args),
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
    let element = |pair| consume(pair, climber);
    let calc = |lhs, op: Pair<Rule>, rhs| match op.as_rule() {
        Rule::plus => AstNode::calc(CalcOp::Plus, lhs, rhs),
        Rule::minus => AstNode::calc(CalcOp::Minus, lhs, rhs),
        Rule::times => AstNode::calc(CalcOp::Times, lhs, rhs),
        Rule::divide => AstNode::calc(CalcOp::Divide, lhs, rhs),
        Rule::modulus => AstNode::calc(CalcOp::Modulus, lhs, rhs),
        _ => unreachable!(),
    };
    match pair.as_rule() {
        Rule::calc_term => {
            let pairs = pair.into_inner();
            climber.climb(pairs, element, calc)
        }
        Rule::element => {
            let newpair = pair.into_inner().next().map(element).unwrap();
            newpair
        },
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
        _ => panic!("Unknown rule: {:?}", pair),
    }
}
