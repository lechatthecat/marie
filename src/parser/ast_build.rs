use pest::iterators::Pairs;
use std::collections::LinkedList;
use crate::value::var_type::VarType;
use super::Rule;
use super::astnode::AstNode;
use super::function;
use super::calculation;

pub fn get_pairs(filename: String, result: Result<Pairs<'_, Rule>, pest::error::Error<Rule>>)
    -> Option<Pairs<'_, Rule>> {
    match result {
        Ok(pairs) => {
            return Some(pairs);
        },
        Err(e) => {
            println!("{}{}", filename,e);
            return None;
        },
    }
}

pub fn build_ast_from_expr(pair: pest::iterators::Pair<Rule>) -> AstNode {
    let span = pair.as_span();
    let location = span.start_pos().line_col();
    match pair.as_rule() {
        Rule::expr => build_ast_from_expr(pair.into_inner().next().unwrap()),
        Rule::calc_term => {
            calculation::into_calc_expression(location, pair)
        },
        Rule::ident => {
            let str = &pair.as_str();
            AstNode::Ident(location, String::from(&str[..]))
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
            AstNode::Str(location, text)
        },
        Rule::number | Rule::integer => {
            let num = pair.as_str().parse::<f64>().unwrap_or_else(|e| panic!("{}", e));
            AstNode::Number(location, f64::from(num))
        },
        Rule::val_bool => {
            match pair.into_inner().next().unwrap().as_rule() {
                Rule::bool_true => AstNode::Bool(location, true),
                Rule::bool_false => AstNode::Bool(location, false),
                _ => unreachable!()
            }
        }
        Rule::concatenated_string => {
            let strs: Vec<AstNode> = pair.into_inner().map(build_ast_from_expr).collect();
            AstNode::Strs(location, strs)
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
                location,
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
                location,
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
                    function::function_call(location, function_name, vec![AstNode::Null])
                },
                _ => {
                    let expr = function_args.unwrap();
                    let args: Vec<AstNode> = expr.into_inner().map(build_ast_from_expr).collect();
                    function::function_call(location, function_name, args)
                }
            }
        },       
        Rule::function_define => {
            let mut function_name = String::from("");
            let mut arguments: Vec<AstNode> = Vec::new();
            let mut fn_return: Box<AstNode> = Box::new(AstNode::Null);
            let mut body: Vec<AstNode> = Vec::new();
            //let mut is_public = false;

            for inner_pair in pair.into_inner() {
                match inner_pair.as_rule() {
                    Rule::function_name => {
                        function_name = String::from(inner_pair.as_str());
                        let default_funcs = vec!["print","println"];
                        if default_funcs.iter().any(|&i| i==function_name) {
                            panic!("You cannot define this function name that is same as one of default functions: {}", function_name)
                        }
                    },
                    Rule::arguments_for_define => arguments = function::parse_arguments(inner_pair),
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
            AstNode::FunctionDefine(location, function_name, arguments, body, fn_return)
        },
        Rule::argument => {
            AstNode::Argument(location, pair.as_str().to_string(), Box::new(AstNode::Null))
        }
        Rule::if_expr => {
            let mut pairs = pair.into_inner();
            let conditions = calculation::into_logical_expression(location, pairs.next().unwrap());
            let mut body: Vec<AstNode> = Vec::new();
            let mut else_if_bodies_conditions: LinkedList<(Vec<AstNode>, Vec<AstNode>)> = LinkedList::new();
            let mut else_body: Vec<AstNode> = Vec::new();
            let mut else_if_id = 0;
            for inner_pair in pairs {
                match inner_pair.as_rule() {
                    Rule::stmt_in_function | Rule::fn_return => {
                        for p in inner_pair.into_inner() {
                            body.push(build_ast_from_expr(p));
                        }
                    },
                    Rule::else_if_expr => {
                        let else_if_pairs = inner_pair.into_inner();
                        let mut else_if_condition: Vec<AstNode> = Vec::new();
                        let mut else_if_body: Vec<AstNode> = Vec::new();
                        for else_if_pair in else_if_pairs {
                            match else_if_pair.as_rule() {
                                Rule::condition | Rule::bool_operation => {
                                    else_if_condition.push(calculation::into_logical_expression(location, else_if_pair));
                                },
                                Rule::stmt_in_function | Rule::fn_return => {
                                    let else_if_pairs = else_if_pair.into_inner();
                                    for else_if_inner_pair in else_if_pairs {
                                        else_if_body.push(build_ast_from_expr(else_if_inner_pair));
                                    }
                                },
                                _ => {}
                            }
                        }
                        else_if_bodies_conditions.push_back((else_if_condition, else_if_body));
                    },
                    Rule::else_expr => {
                        let else_pairs = inner_pair.into_inner();
                        for else_pair in else_pairs {
                            match else_pair.as_rule() {
                                Rule::stmt_in_function | Rule::fn_return => {
                                    for p in else_pair.into_inner() {
                                        else_body.push(build_ast_from_expr(p));
                                    }
                                },
                                _ => {}
                            }
                        } 
                    },
                    _ => {println!("{:?}", inner_pair)}
                }
                else_if_id = else_if_id + 1;
            }
            AstNode::IF(location, Box::new(conditions), body, else_if_bodies_conditions, else_body)
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
            AstNode::ForLoop(location, is_inclusive, var_type, ident.to_string(), Box::new(first_elemnt), Box::new(last_elemnt), stmt_in_function)
        },
        unknown_expr => panic!("Unexpected expression: {:?}", unknown_expr),
    }
}
