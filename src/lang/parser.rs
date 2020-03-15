use pest::Parser;
use pest::error::Error;
use pest::iterators::{Pair, Pairs};
use pest::prec_climber::{Assoc, Operator, PrecClimber};
use super::constant::{VARTYPE_CONSTANT, VARTYPE_VARIABLE, VARTYPE_REASSIGNED};

#[derive(Parser)]
#[grammar = "grammer/oran.pest"]
pub struct OParser;

use super::astnode;

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

pub fn parse(source: &str) -> Result<Vec<Box<astnode::AstNode>>, Error<Rule>> {
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

fn build_ast_from_expr(pair: pest::iterators::Pair<Rule>) -> astnode::AstNode {
    use astnode::AstNode;

    match pair.as_rule() {
        Rule::expr => build_ast_from_expr(pair.into_inner().next().unwrap()),
        Rule::term => {
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
                        Rule::escaped => {
                            text.push_str(&String::from(pair.as_str().replace("\\", "")));
                        }
                        _ => { 
                            text.push_str(&String::from(pair.as_str()));
                        }
                    }
                }
            }
            AstNode::Str(text)
        }
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
            astnode::AstNode::Null
        },
        unknown_expr => panic!("Unexpected expression: {:?}", unknown_expr),
    }
}

fn function_call (fn_name: Pair<'_, Rule>, args: Vec<astnode::AstNode>) -> astnode::AstNode {
    use astnode::AstNode;
    use astnode::DefaultFunction;

    match fn_name.as_str() {
        "print" => AstNode::FunctionCall(DefaultFunction::Print, args),
        _ => panic!("Unknown function: {:?}", fn_name),
    }
}

fn into_expression(pair: Pair<Rule>) -> astnode::AstNode {
    let climber = PrecClimber::new(vec![
        Operator::new(Rule::plus, Assoc::Left) |
            Operator::new(Rule::minus, Assoc::Left),
        Operator::new(Rule::times, Assoc::Left) | Operator::new(Rule::divide, Assoc::Left) |
            Operator::new(Rule::modulus, Assoc::Left),
    ]);

    consume(pair, &climber)
}

fn consume(pair: Pair<Rule>, climber: &PrecClimber<Rule>) -> astnode::AstNode {
    use astnode::AstNode;
    use astnode::CalcOp;

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
            newpair
        },
        Rule::number => {
            let number = pair.as_str().parse().unwrap();
            AstNode::Number(number)
        }
        Rule::ident => {
            let ident = pair.as_str();
            AstNode::Ident(ident.to_string())
        }
        _ => unreachable!(),
    }
}
