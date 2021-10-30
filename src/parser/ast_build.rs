use pest::iterators::Pairs;
use pest::error::{Error, ErrorVariant};
use std::process;
use std::collections::LinkedList;
use crate::value::var_type::VarType;
use super::Rule;
use super::astnode::AstNode;
use super::function;
use super::calculation;

pub fn get_pairs(filename: String, result: Result<Pairs<'_, Rule>, Error<Rule>>)
    -> Option<Pairs<'_, Rule>> {
    match result {
        Ok(pairs) => {
            return Some(pairs);
        },
        Err(e) => {
            let e = e.renamed_rules(|rule| {
                match *rule {
                    Rule::ident => "variable".to_owned(),
                    Rule::escape_char 
                    | Rule::escaped_escape_char 
                    | Rule::escaped_quote
                    | Rule::double_quote_char => "alphamumeric values".to_owned(),
                    Rule::single_quote_string
                    | Rule::double_quote_string
                    | Rule::string
                    | Rule::concatenated_string => "string".to_owned(),
                    Rule::two_equals => "==".to_owned(),
                    Rule::bigger_than => ">".to_owned(),
                    Rule::smaller_than => "<".to_owned(),
                    Rule::e_bigger_than => "=>".to_owned(),
                    Rule::e_smaller_than => "=<".to_owned(),
                    Rule::op_or => "||".to_owned(),
                    Rule::op_and => "&&".to_owned(),
                    Rule::bool_true => "true".to_owned(),
                    Rule::bool_false => "false".to_owned(),
                    Rule::plus => "+".to_owned(),
                    Rule::minus => "-".to_owned(),
                    Rule::times => "*".to_owned(),
                    Rule::divide => "/".to_owned(),
                    Rule::modulus => "%".to_owned(),
                    Rule::power => "^".to_owned(),
                    Rule::assgmt_expr
                    | Rule::re_assgmt_expr => "expression".to_owned(),
                    Rule::calc_term => "variable/value".to_owned(),
                    Rule::function_name => "funcation name".to_owned(),
                    Rule::function_call => "function call".to_owned(),
                    Rule::function_define => "definition of function".to_owned(),
                    Rule::arguments_for_call
                    | Rule::argument
                    | Rule::arguments_for_define => "arguments of function".to_owned(),
                    Rule::op_dots => "..".to_owned(),
                    Rule::op_dots_inclusive => "..=".to_owned(),
                    Rule::first_element => "first value of the range".to_owned(),
                    Rule::last_element => "last value of the range".to_owned(),
                    Rule::op_for => "for".to_owned(),
                    Rule::op_in => "in".to_owned(),
                    Rule::for_var_mut => "mut".to_owned(),
                    Rule::op_if => "if".to_owned(),
                    Rule::op_else => "else".to_owned(),
                    Rule::op_else_if => "else if".to_owned(),
                    Rule::if_expr => "expression for if statement".to_owned(),
                    Rule::else_if_expr => "expression for else-if statement".to_owned(),
                    Rule::else_expr => "expression for else statement".to_owned(),
                    Rule::op_return => "return".to_owned(),
                    Rule::fn_return => "return {{function}}".to_owned(),
                    Rule::end_mark => "semicolon".to_owned(),
                    Rule::last_stmt_in_function => "value or variable to be return from the function".to_owned(),
                    Rule::EOI => "EOI".to_owned(),
                    _ => {
                        "".to_owned()
                    }
                }
            });
            println!("{}{}", filename, e);
            return None;
        },
    }
}

pub fn build_ast_from_expr(
        location:(String, usize, usize),
        pair: pest::iterators::Pair<Rule>
    ) -> AstNode {
    let location = location.clone();
    match pair.as_rule() {
        Rule::expr => build_ast_from_expr(location, pair.into_inner().next().unwrap()),
        Rule::calc_term => {
            calculation::into_calc_expression(location, pair)
        },
        Rule::ident => {
            let str = &pair.as_str();
            AstNode::Ident(location, String::from(&str[..]))
        },
        Rule::string => {
            let mut text = "".to_owned();
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
            let strs: Vec<AstNode> = pair.into_inner().map(|v| build_ast_from_expr(location.clone(), v)).collect();
            AstNode::Strs(location, strs)
        },
        Rule::assgmt_expr => {
            let mut pair = pair.into_inner();
            let var_prefix = pair.next().unwrap();
            let var_type = match var_prefix.as_rule() {
                Rule::var_const => VarType::Constant,
                Rule::var_mut => VarType::VariableFirstAssigned,
                _ => {
                    let mut message = "unknown variable type: ".to_owned();
                    message.push_str(&var_prefix.as_str());
                    let error: Error<Rule> = Error::new_from_span(
                        ErrorVariant::CustomError{
                            message: message
                        },
                        var_prefix.as_span()
                    );
                    println!("{}{}", location.0, error);
                    process::exit(1);
                }
            };
            let ident = pair.next().unwrap();
            let expr = pair.next().unwrap();
            let expr = build_ast_from_expr(location.clone(), expr);
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
            let expr = build_ast_from_expr(location.clone(), expr);
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
                    let args: Vec<AstNode> = expr.into_inner().map(|v| build_ast_from_expr(location.clone(), v)).collect();
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
                            let mut message = "You cannot define this function name that is same as one of default functions: ".to_owned();
                            message.push_str(&function_name);
                            let error: Error<Rule> = Error::new_from_span(
                                ErrorVariant::CustomError{
                                    message: message
                                },
                                inner_pair.as_span()
                            );
                            println!("{}{}", location.0, error);
                            process::exit(1);
                        }
                    },
                    Rule::arguments_for_define => arguments = function::parse_arguments(location.clone(), inner_pair),
                    Rule::stmt_in_function => {
                        inner_pair.into_inner().for_each(|body_stmt|
                            body.push(build_ast_from_expr(location.clone(), body_stmt))
                        );
                    },
                    Rule::fn_return | Rule::last_stmt_in_function => {
                        let fn_return_stmt = inner_pair.into_inner().next().unwrap();
                        fn_return =  Box::new(build_ast_from_expr(location.clone(), fn_return_stmt));
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
            let conditions = calculation::into_logical_expression(location.clone(), pairs.next().unwrap());
            let mut body: Vec<AstNode> = Vec::new();
            let mut else_if_bodies_conditions: LinkedList<(Vec<AstNode>, Vec<AstNode>)> = LinkedList::new();
            let mut else_body: Vec<AstNode> = Vec::new();
            let location_for_inner_scope = location.clone();
            for inner_pair in pairs {
                match inner_pair.as_rule() {
                    Rule::stmt_in_function | Rule::fn_return => {
                        for p in inner_pair.into_inner() {
                            body.push(build_ast_from_expr(location_for_inner_scope.clone(), p));
                        }
                    },
                    Rule::else_if_expr => {
                        let else_if_pairs = inner_pair.into_inner();
                        let mut else_if_condition: Vec<AstNode> = Vec::new();
                        let mut else_if_body: Vec<AstNode> = Vec::new();
                        for else_if_pair in else_if_pairs {
                            match else_if_pair.as_rule() {
                                Rule::condition | Rule::bool_operation => {
                                    else_if_condition.push(calculation::into_logical_expression(location_for_inner_scope.clone(), else_if_pair));
                                },
                                Rule::stmt_in_function | Rule::fn_return => {
                                    let else_if_pairs = else_if_pair.into_inner();
                                    for else_if_inner_pair in else_if_pairs {
                                        else_if_body.push(build_ast_from_expr(location_for_inner_scope.clone(), else_if_inner_pair));
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
                                        else_body.push(build_ast_from_expr(location.clone(), p));
                                    }
                                },
                                _ => {}
                            }
                        } 
                    },
                    _ => {println!("{:?}", inner_pair)}
                }
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
            let first_elemnt = build_ast_from_expr(location.clone(), test.into_inner().next().unwrap());
            let is_inclusive = match range.next().unwrap().as_rule() {
                Rule::op_dots => false,
                Rule::op_dots_inclusive => true,
                unknown_expr => panic!("Unexpected expression: {:?}", unknown_expr),
            };
            let last_elemnt = build_ast_from_expr(location.clone(), range.next().unwrap().into_inner().next().unwrap());
            let stmt_in_function= pairs.map(|pair|
                build_ast_from_expr(location.clone(), pair.into_inner().next().unwrap())
            ).collect::<Vec<AstNode>>();
            AstNode::ForLoop(location, is_inclusive, var_type, ident.to_string(), Box::new(first_elemnt), Box::new(last_elemnt), stmt_in_function)
        },
        Rule::array => {
            let elements_in_array = pair.into_inner().map(
                |pair|build_ast_from_expr(location.clone(), pair)
            ).collect::<Vec<AstNode>>();
            AstNode::Array(location, elements_in_array)
        },
        Rule::array_element => {
            let mut pairs = pair.into_inner();
            let array = Box::new(build_ast_from_expr(location.clone(),pairs.next().unwrap()));
            let index = Box::new(build_ast_from_expr(location.clone(),pairs.next().unwrap().into_inner().next().unwrap()));
            AstNode::ArrayElement(location, 
                array,
                index
            )
        },
        unknown_expr => panic!("Unexpected expression: {:?}", unknown_expr),
    }
}
