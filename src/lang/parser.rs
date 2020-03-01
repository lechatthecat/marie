use pest::Parser;
use pest::error::Error;
use pest::iterators::Pair;
use pest::prec_climber::{Assoc, Operator, PrecClimber};

#[derive(Parser)]
#[grammar = "grammer/oran.pest"]
pub struct OParser;

use super::astnode;

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

pub fn parse(source: &str) -> Result<Vec<Box<astnode::AstNode>>, Error<Rule>> {
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

fn build_ast_from_expr(pair: pest::iterators::Pair<Rule>) -> astnode::AstNode {
    use astnode::AstNode;
    use astnode::DefaultFunction;
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
            let str = &pair.as_str();
            // Strip leading and ending quotes.
            let str = &str[1..str.len() - 1];
            // Strip escape characters.
            let str = str.replace("\\", "");
            AstNode::Str(str.to_string())
        }
        Rule::concatenated_string => {
            let strs: Vec<AstNode> = pair.into_inner().map(build_ast_from_expr).collect();
            AstNode::Strs(strs)
        },
        Rule::assgmt_expr => {
            let mut pair = pair.into_inner();
            let var_prefix = pair.next().unwrap();
            let var_type = match var_prefix.as_str() {
                "const" => 1,
                "let" => 2,
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
                3, //re-assign
                String::from(ident.as_str()),
                Box::new(expr),
            )
        },
        Rule::function_call => {
            let mut pair = pair.into_inner();
            let function_name = pair.next().unwrap();
            let expr = pair.next().unwrap();
            let expr = build_ast_from_expr(expr);
            match function_name.as_str() {
                "print" => AstNode::FunctionCall(DefaultFunction::Print, Box::new(expr)),
                _ => panic!("Unknown function: {:?}", function_name),
            }
        }
        unknown_expr => panic!("Unexpected expression: {:?}", unknown_expr),
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
