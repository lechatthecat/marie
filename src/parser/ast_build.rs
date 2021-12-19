use pest::iterators::Pairs;
use pest::iterators::Pair;
use pest::error::{Error, ErrorVariant};
use std::process;
use crate::value::var_type::VarType;
use super::Rule;
use super::astnode::AstNode;
use super::function;
use super::calculation;

pub fn build_ast_from_expr<'a>(filename: &'a str, pair: Pair<'a, Rule>) -> AstNode<'a> {
    match pair.as_rule() {
        Rule::expr => build_ast_from_expr(filename, pair.into_inner().next().unwrap()),
        Rule::calc_term => {
            calculation::into_calc_expression(filename, pair)
        },
        Rule::ident => {
            let str = &pair.as_str();
            AstNode::Ident(String::from(&str[..]))
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
            let strs: Vec<AstNode> = pair.into_inner().map(|v| build_ast_from_expr(filename, v)).collect();
            AstNode::Strs(strs)
        },
        Rule::assgmt_expr => {
            let mut pairs = pair.into_inner();
            let var_prefix = pairs.next().unwrap();
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
                    ).with_path(&filename);
                    println!("Error!{}", error);
                    process::exit(1);
                }
            };
            let ident = pairs.next().unwrap();
            let expr = pairs.next().unwrap();
            let expr = build_ast_from_expr(filename, expr);
            AstNode::Assign (
                var_type,
                String::from(ident.as_str()),
                Box::new(expr),
            )
        }
        Rule::re_assgmt_expr => {
            let mut pairs = pair.into_inner();
            let ident = pairs.next().unwrap();
            let expr = build_ast_from_expr(filename, pairs.next().unwrap());
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
                    function::function_call(function_name, vec![AstNode::Null])
                },
                _ => {
                    let expr = function_args.unwrap();
                    let args: Vec<AstNode> = expr.into_inner().map(|v| build_ast_from_expr(filename, v)).collect();
                    function::function_call(function_name, args)
                }
            }
        },       
        Rule::function_define => {
            let mut function_name = String::from("");
            let mut arguments: Vec<AstNode> = Vec::new();
            let mut fn_return: Box<AstNode> = Box::new(AstNode::Null);
            let mut body: Vec<(AstNode, Pair<'a, Rule>)> = Vec::new();
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
                            ).with_path(&filename);
                            println!("Error!{}", error);
                            process::exit(1);
                        }
                    },
                    Rule::arguments_for_define => arguments = function::parse_arguments(filename, inner_pair),
                    Rule::stmt_in_function => {
                        inner_pair.into_inner().for_each(|body_stmt|
                            body.push((build_ast_from_expr(filename,  body_stmt.clone()), body_stmt))
                        );
                    },
                    Rule::fn_return | Rule::last_stmt_in_function => {
                        let fn_return_stmt = inner_pair.into_inner().next().unwrap();
                        fn_return =  Box::new(build_ast_from_expr(filename, fn_return_stmt));
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
            let conditions = calculation::into_logical_expression(filename, pairs.next().unwrap());
            let mut body: Vec<(AstNode, Pair<'a, Rule>)> = Vec::new();
            let mut else_if_bodies_conditions: Vec<(Vec<AstNode>, Vec<(AstNode<'a>, Pair<'a, Rule>)>)> = Vec::new();
            let mut else_body: Vec<(AstNode, Pair<'a, Rule>)> = Vec::new();
            for inner_pair in pairs {
                match inner_pair.as_rule() {
                    Rule::stmt_in_function | Rule::fn_return => {
                        for p in inner_pair.into_inner() {
                            body.push((build_ast_from_expr(filename, p.clone()), p));
                        }
                    },
                    Rule::else_if_expr => {
                        let else_if_pairs = inner_pair.into_inner();
                        let mut else_if_condition: Vec<AstNode> = Vec::new();
                        let mut else_if_body: Vec<(AstNode, Pair<'a, Rule>)> = Vec::new();
                        for else_if_pair in else_if_pairs {
                            match else_if_pair.as_rule() {
                                Rule::condition | Rule::bool_operation => {
                                    else_if_condition.push(calculation::into_logical_expression(filename, else_if_pair));
                                },
                                Rule::stmt_in_function | Rule::fn_return => {
                                    let else_if_pairs = else_if_pair.into_inner();
                                    for else_if_inner_pair in else_if_pairs {
                                        else_if_body.push((build_ast_from_expr(filename, else_if_inner_pair.clone()), else_if_inner_pair));
                                    }
                                },
                                _ => {}
                            }
                        }
                        else_if_bodies_conditions.push((else_if_condition, else_if_body));
                    },
                    Rule::else_expr => {
                        let else_pairs = inner_pair.into_inner();
                        for else_pair in else_pairs {
                            match else_pair.as_rule() {
                                Rule::stmt_in_function | Rule::fn_return => {
                                    for p in else_pair.into_inner() {
                                        else_body.push((build_ast_from_expr(filename, p.clone()), p));
                                    }
                                },
                                _ => {}
                            }
                        } 
                    },
                    _ => {println!("{:?}", inner_pair)}
                }
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
            let mut ranges = pairs.next().unwrap().into_inner();
            let range = ranges.next().unwrap();
            let first_elemnt = build_ast_from_expr(filename, range.into_inner().next().unwrap());
            let is_inclusive = match ranges.next().unwrap().as_rule() {
                Rule::op_dots => false,
                Rule::op_dots_inclusive => true,
                unknown_expr => panic!("Unexpected expression: {:?}", unknown_expr),
            };
            let last_elemnt = build_ast_from_expr(filename, ranges.next().unwrap().into_inner().next().unwrap());
            let stmt_in_function= pairs.map(|pair|
                (build_ast_from_expr(filename, pair.clone().into_inner().next().unwrap()), pair)
            ).collect::<Vec<(AstNode, Pair<'a, Rule>)>>();
            AstNode::ForLoop(is_inclusive, var_type, ident.to_string(), Box::new(first_elemnt), Box::new(last_elemnt), stmt_in_function)
        },
        Rule::for_expr_ident => {
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
            let array = pairs.next().unwrap().as_str();
            let stmt_in_function= pairs.map(|pair|
                build_ast_from_expr(filename, pair.into_inner().next().unwrap())
            ).collect::<Vec<AstNode>>();
             
            AstNode::ForLoopIdent(var_type, ident.to_string(), array.to_string(), stmt_in_function)
        },
        Rule::for_expr_array => {
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
            let array = pairs.next().unwrap().into_inner().map(|v|
                build_ast_from_expr(filename, v)
            ).collect::<Vec<AstNode>>();
            let stmt_in_function= pairs.map(|pair|
                build_ast_from_expr(filename, pair.into_inner().next().unwrap())
            ).collect::<Vec<AstNode>>();
             
            AstNode::ForLoopArray(var_type, ident.to_string(), array, stmt_in_function)
        },
        Rule::array => {
            let elements_in_array = pair.into_inner().map(
                |pair|build_ast_from_expr(filename, pair)
            ).collect::<Vec<AstNode>>();
            AstNode::Array(elements_in_array)
        },
        Rule::array_element => {
            let mut pairs = pair.into_inner();
            let array = Box::new(build_ast_from_expr(filename, pairs.next().unwrap()));
            let indexes: Vec<AstNode> = pairs.map(|v| build_ast_from_expr(filename, v.into_inner().next().unwrap())).collect();
            AstNode::ArrayElement(
                array,
                indexes
            )
        },
        Rule::array_re_assgmt_expr => {
            let mut pairs = pair.into_inner();
            let mut inner_pairs = pairs.next().unwrap().into_inner();
            let array_name = inner_pairs.next().unwrap().as_str();
            let index = Box::new(build_ast_from_expr(filename, inner_pairs.next().unwrap().into_inner().next().unwrap()));
            let expr = Box::new(build_ast_from_expr(filename, pairs.next().unwrap()));
            AstNode::ArrayElementAssign (
                VarType::VariableReAssigned,
                String::from(array_name),
                index,
                expr
            )
        },
        unknown_expr => panic!("Unexpected expression: {:?}", unknown_expr),
    }
}

pub fn get_pairs<'a>(filename: &'a str, result: Result<Pairs<'a, Rule>, Error<Rule>>)
    -> Option<Pairs<'a, Rule>> {
    match result {
        Ok(pairs) => {
            return Some(pairs);
        },
        Err(mut e) => {
            let variant = match e.variant {
                ErrorVariant::ParsingError {
                    positives,
                    negatives,
                } => {
                    let message = parsing_error_message(&positives, &negatives, |rule| {
                        match *rule {
                            Rule::left_parenthesis => "(".to_owned(),
                            Rule::right_parenthesis => ")".to_owned(),
                            Rule::left_curly_brace => "{".to_owned(),
                            Rule::right_curly_brace =>  "}".to_owned(),
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
                            Rule::op_return | Rule::fn_return  => "}".to_owned(),
                            Rule::end_mark | Rule::EOI => ";".to_owned(),
                            Rule::last_stmt_in_function => "value or variable to be returned from the function".to_owned(),
                            Rule::array => "array".to_owned(),
                            Rule::array_element => "array's element".to_owned(),
                            Rule::element => "any value".to_owned(),
                            Rule::val_bool => "boolean".to_owned(),
                            Rule::number => "number".to_owned(),
                            _ => {
                                "".to_owned()
                            }
                        }
                    });
                    ErrorVariant::CustomError { message }
                }
                variant => variant,
            };
    
            e.variant = variant;
            let e = e.with_path(&filename);
            println!("Error!{}", &e);
            return None;
        },
    }
}

fn parsing_error_message<F>(positives: &[Rule], negatives: &[Rule], mut f: F) -> String
where
    F: FnMut(&Rule) -> String,
{
    match (negatives.is_empty(), positives.is_empty()) {
        (false, false) => format!(
            "unexpected {}; expected {}",
            enumerate(negatives, &mut f),
            enumerate(positives, &mut f)
        ),
        (false, true) => format!("unexpected {}", enumerate(negatives, &mut f)),
        (true, false) => format!("expected {}", enumerate(positives, &mut f)),
        (true, true) => "unknown parsing error".to_owned(),
    }
}

fn enumerate<F>(rules: &[Rule], f: &mut F) -> String
where
    F: FnMut(&Rule) -> String,
{
    let mut string_rules: Vec<String> = Vec::with_capacity(rules.len());
    let rules = rules
        .iter()
        .filter(|x|{
            let mut is_unique = false;
            if !string_rules.contains(&f(&x)) {
                string_rules.push(f(&x));
                is_unique = true;
            }
            is_unique && !f(&x).is_empty()
        })
        .collect::<Vec<_>>();
    match rules.len() {
        1 => f(&rules[0]),
        2 => format!("{} or {}", f(&rules[0]), f(&rules[1])),
        l => {
            let separated = rules
                .iter()
                .take(l - 1)
                .map(|r| f(r))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}, or {}", separated, f(&rules[l - 1]))
        }
    }
}
