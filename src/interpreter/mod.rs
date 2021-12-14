use crate::parser::astnode::{AstNode, CalcOp, LogicalOperatorType, ComparisonlOperatorType};
use crate::value::oran_value::{OranValue, FunctionDefine};
use crate::value::oran_variable::{OranVariable, OranVariableValue};
use crate::value::oran_string::OranString;
use crate::value::oran_scope::OranScope;
use crate::value::var_type::{FunctionOrValueType, VarType};
use std::collections::HashMap;
use std::io::{self, Write};
use std::borrow::Cow;
use std::process;
use pest::error::{Error, ErrorVariant};
use pest::iterators::Pair;
use crate::parser::Rule;
mod util;

pub fn interp_expr<'a, 'b:'a>(
    scope: OranScope, 
    env : &mut HashMap<(
            OranScope,
            FunctionOrValueType,
            OranString<'b>
        ),
        OranValue<'b>
    >, 
    reduced_expr: &'b AstNode,
    filename: &'b str,
    pair: &'b Pair<'b, Rule>,
    ) -> OranValue<'a> {

    match reduced_expr {
        AstNode::Number(double) => OranValue::Float(*double),
        AstNode::Calc (verb, lhs, rhs) => {
            let lhs_val = interp_expr(scope, env, lhs, filename, pair);
            let rhs_val = interp_expr(scope, env, rhs, filename, pair);
            match verb {
                CalcOp::Plus => { add(lhs_val, rhs_val, filename,pair) }
                CalcOp::Minus => { sub(lhs_val, rhs_val, filename,pair) }
                CalcOp::Times => { mul(lhs_val, rhs_val, filename,pair) }
                CalcOp::Divide => { div(lhs_val, rhs_val, filename,pair) }
                CalcOp::Modulus => { rem(lhs_val, rhs_val, filename,pair) }
                CalcOp::Power => { pow(lhs_val, rhs_val, filename,pair) }
            }
        }
        AstNode::Ident(ident) => {
            let val_tuple = util::get_val(
                scope,
                env,
                ident,
                FunctionOrValueType::Value
            );
            let val = match val_tuple.0 {
                None => {
                    let message = format!("{}{}{}", "The variable \"".to_owned(), ident, "\" is not defined.");
                    show_error_message(message, filename, pair);
                    process::exit(1);
                },
                Some(oran_val) => oran_val
            };
            val
        }
        AstNode::ArrayElementAssign(variable_type, array_name, index, expr) => {
            util::is_mutable(filename, pair, scope, env, array_name, variable_type);
            let usize_index = f64::from(interp_expr(scope, env, index, filename, pair)) as usize;
            let val = util::get_val(
                scope,
                env,
                array_name,
                FunctionOrValueType::Value
            );
            match val.0 {
                None => {
                    let message = format!("{}{}{}", "This array \"".to_owned(), array_name, "\" doesn't exist.");
                    show_error_message(message, filename, pair);
                    process::exit(1);
                },
                Some(v) => {
                    let mut array = match OranValue::from(v) {
                        OranValue::Array(a) => a,
                        OranValue::Variable(v) => {
                            let array = match OranValue::from(v.value) {
                                OranValue::Array(a) => a,
                                _ => {
                                    show_error_message("This variable isn't array.".to_owned(), filename, pair);
                                    process::exit(1);
                                }
                            };
                            array
                        }
                        _ => {
                            show_error_message("This variable isn't array.".to_owned(), filename, pair);
                            process::exit(1);
                        }
                    };
                    array[usize_index] = interp_expr(scope, env, expr, filename, pair);
                    env.insert((scope, FunctionOrValueType::Value, OranString::from(array_name)), OranValue::Array(array));
                }
            };
            OranValue::Null
        }
        AstNode::Assign(variable_type, ident, expr) => {
            util::is_mutable(filename, pair, scope, env, ident, variable_type);
            if *variable_type == VarType::VariableReAssigned {
                let val = util::get_val(
                    scope,
                    env,
                    ident,
                    FunctionOrValueType::Value
                );
                let oran_val = OranValue::Variable(OranVariable {
                    var_type: *variable_type,
                    name: ident,
                    value: OranVariableValue::from(&interp_expr(scope, env, expr, filename, pair)),
                });
                match val.0 {
                    None => {
                        env.insert((scope, FunctionOrValueType::Value, OranString::from(ident)), oran_val);
                    },
                    Some(_) => {
                        env.insert((val.1, FunctionOrValueType::Value, OranString::from(ident)), oran_val);
                    }
                };
            } else {
                let oran_val = OranValue::Variable(OranVariable {
                    var_type: *variable_type,
                    name: ident,
                    value: OranVariableValue::from(&interp_expr(scope, env, expr, filename, pair)),
                });
                env.insert((scope, FunctionOrValueType::Value, OranString::from(ident)), oran_val);
            }
            OranValue::Null
        }
        AstNode::FunctionCall(name, arg_values) => {
            match name.as_ref() {
                "print" => {
                    let mut text = "".to_owned();
                    arg_values.into_iter().for_each(|str|
                        text.push_str(&String::from(&interp_expr(scope, env, &str, filename, pair)))
                    );
                    print!("{}", text);
                    io::stdout().flush().unwrap();
                    OranValue::Null
                },
                "println" => {
                    let mut text = "".to_owned();
                    arg_values.into_iter().for_each(|str|
                        text.push_str(&String::from(&interp_expr(scope, env, &str, filename, pair)))
                    );
                    println!("{}", text);
                    OranValue::Null
                },
                _ => {
                    //Try to get function definition from current scope.
                    let this_func_scope = scope+1;
                    let func_tuple = util::get_val(
                        scope,
                        env,
                        name,
                        FunctionOrValueType::Function
                    );
                    let func: OranValue = match func_tuple.0 {
                        None => {
                            let message = format!("{}{}{}", "Function \"".to_owned(), name, "\" is not defined.");
                            show_error_message(message, filename, pair);
                            process::exit(1);
                        },
                        Some(v) => v
                    };
                    let func = FunctionDefine::from(&func);
                    for i in 0..func.args.len() {
                        let arg_name = interp_expr(this_func_scope, env, func.args.into_iter().nth(i).unwrap(), filename, pair);
                        let arg_ast = arg_values.into_iter().nth(i).unwrap_or_else(||
                            {
                                let message = "Argument is necessary but not supplied.".to_owned();
                                show_error_message(message, filename, pair);
                                process::exit(1);
                            });
                        let val = interp_expr(scope, env, arg_ast, filename, pair);
                        env.insert((this_func_scope, FunctionOrValueType::Value, OranString::from(arg_name)), val);
                    }
                    let mut returned_val = OranValue::Null;
                    for body in func.body {
                        returned_val = interp_expr(this_func_scope, env, &body, filename, pair);
                        match returned_val {
                            OranValue::Null => {}
                            _ => { break }
                        }
                    }
                    match returned_val {
                        OranValue::Null => {
                            returned_val = interp_expr(this_func_scope, env, func.fn_return, filename, pair);
                        }
                        _ => {}
                    }
                    // delete unnecessary data when exiting a scope
                    // TODO garbage colloctor
                    env.retain(|(s, __k, _label), _orn_val| *s != this_func_scope);
                    returned_val
                }
            }
        }
        AstNode::FunctionDefine(func_name, args, astnodes, fn_return) => {
            let val = OranValue::Function(FunctionDefine {
                name: func_name,
                args: args,
                body: astnodes,
                fn_return: fn_return
            });
            env.insert((scope, FunctionOrValueType::Function, OranString::from(func_name)), val.clone());
            val
        }
        AstNode::Argument(argument_name, val) => {
            let val = interp_expr(scope, env, val, filename, pair);
            env.insert((scope, FunctionOrValueType::Value, OranString::from(argument_name)), val);
            OranValue::Str(OranString::from(argument_name))
        }
        AstNode::Str (str_val) => {
            OranValue::Str(OranString::from(str_val))
        }
        AstNode::Strs (strs) => {
            let mut text = "".to_owned();
            for str in strs {
                text.push_str(&String::from(interp_expr(scope, env, &str, filename, pair)))
            }
            OranValue::Str(OranString {
                val_str: Cow::from(text)
            })
        }
        AstNode::Condition (c, e, o) => {
            let e = interp_expr(scope, env, e, filename, pair);
            let o = interp_expr(scope, env, o, filename, pair);
            match c {
                ComparisonlOperatorType::AND => {
                    if bool::from(e) && bool::from(o) {
                        return OranValue::Boolean(true);
                    }
                    OranValue::Boolean(false)
                }
                ComparisonlOperatorType::OR => {
                    if bool::from(e) || bool::from(o) {
                        return OranValue::Boolean(true);
                    }
                    OranValue::Boolean(false)
                }
            }
        }
        AstNode::Comparison (e, c, o) => {
            let e = interp_expr(scope, env, e, filename, pair);
            let o = interp_expr(scope, env, o, filename, pair);

            let is_num_e  = match Result::<f64, String>::from(&e) {
                Ok(_v) => true,
                Err(_e) => false
            };
            let is_num_o = match Result::<f64, String>::from(&o) {
                Ok(_v) => true,
                Err(_e) => false
            };

            if !is_num_e || !is_num_o {
                match c {
                    LogicalOperatorType::Equal => {
                        if e.to_string() == o.to_string() {
                            return OranValue::Boolean(true);
                        }
                        OranValue::Boolean(false)
                    },
                    _ => {
                        let message = format!("{}{}{}{}", "One of these are not number: ".to_owned(), String::from(e), ", ", String::from(o));
                        show_error_message(message, filename, pair);
                        process::exit(1);
                    },
                }
            } else {
                match c {
                    LogicalOperatorType::Equal => {
                        if e == o {
                            return OranValue::Boolean(true);
                        }
                        OranValue::Boolean(false)
                    },
                    LogicalOperatorType::BiggerThan => {
                        if e > o {
                            return OranValue::Boolean(true);
                        }
                        OranValue::Boolean(false)
                    },
                    LogicalOperatorType::SmallerThan => {
                        if e < o {
                            return OranValue::Boolean(true);
                        }
                        OranValue::Boolean(false)
                    },
                    LogicalOperatorType::EbiggerThan => {
                        if e >= o {
                            return OranValue::Boolean(true);
                        }
                        OranValue::Boolean(false)
                    },
                    LogicalOperatorType::EsmallerThan => {
                        if e <= o {
                            return OranValue::Boolean(true);
                        }
                        OranValue::Boolean(false)
                    },
                }
            }
        }
        AstNode::IF(if_conditions, body, else_if_bodies_conditions, else_bodies) => {
            // if
            let condition_result = interp_expr(scope, env, if_conditions, filename, pair);
            if bool::from(condition_result) {
                let mut returned_val =  OranValue::Null;
                for astnode in body {
                    returned_val = interp_expr(scope, env, &astnode, filename, pair);
                }
                return returned_val;
            }
            // else if
            let mut _is_all_false = true;
            if !else_if_bodies_conditions.is_empty() {
                for (conditions, else_if_body) in else_if_bodies_conditions {
                    for c in conditions {
                        let result = interp_expr(scope, env, &c, filename, pair);
                        if bool::from(result) {
                            _is_all_false = false;
                            let mut returned_val =  OranValue::Null;
                            for astnode in else_if_body {
                                returned_val = interp_expr(scope, env, &astnode, filename, pair);
                            }
                            return returned_val;
                        }
                    }
                    
                }
                if _is_all_false == false {
                    return OranValue::Null;
                }
            }
            // else
            if !else_bodies.is_empty() {
                let mut returned_val =  OranValue::Null;
                for astnode in else_bodies {
                    returned_val = interp_expr(scope, env, astnode, filename, pair);
                }
                return returned_val;
            }
            OranValue::Null
        }
        AstNode::Bool (b) => {
            OranValue::Boolean(*b)
        }
        AstNode::ForLoop(is_inclusive, var_type, i, first, last, stmts) => {
            let first = interp_expr(scope, env, first, filename, pair);
            let first = f64::from(first).round() as i64;
            let last = interp_expr(scope, env, last, filename, pair);
            let last = f64::from(last).round() as i64;
            let i_name = OranString::from(i);
            let this_for_loop_scope = scope + 1;
            match is_inclusive {
                true => {
                    for num in first..=last {
                        env.insert(
                            (this_for_loop_scope, FunctionOrValueType::Value, i_name.clone()),
                            OranValue::Variable(OranVariable {
                                var_type: *var_type,
                                name: i,
                                value: OranVariableValue::Float(num as f64)
                            })
                        );
                        let mut returned_val: OranValue;
                        for stmt in stmts {
                            returned_val = interp_expr(this_for_loop_scope, env, stmt, filename, pair);
                            match returned_val {
                                OranValue::Null => {},
                                _ => { return returned_val }
                            }
                        }
                    }
                }
                false => {
                    for num in first..last {
                        env.insert(
                            (this_for_loop_scope, FunctionOrValueType::Value, i_name.clone()),
                            OranValue::Variable(OranVariable {
                                var_type: *var_type,
                                name: i,
                                value: OranVariableValue::Float(num as f64)
                            })
                        );
                        let mut returned_val: OranValue;
                        for stmt in stmts {
                            returned_val = interp_expr(this_for_loop_scope, env, stmt, filename, pair);
                            match returned_val {
                                OranValue::Null => {},
                                _ => { return returned_val }
                            }
                        }
                    }
                }
            }
            // delete unnecessary data when exiting a scope
            // TODO garbage colloctor
            env.retain(|
                (s,
                __k,
                _label),
                _orn_val|*s != this_for_loop_scope
            );
            OranValue::Null
        },
        AstNode::ForLoopIdent(var_type, i, array, stmts) => {
            let val_tuple = util::get_val(
                scope,
                env,
                array,
                FunctionOrValueType::Value
            );
            let array = match val_tuple.0 {
                None => {
                    let message = format!("{}{}{}", "The variable \"".to_owned(), array, "\" is not defined.");
                    show_error_message(message, filename, pair);
                    process::exit(1);
                },
                Some(oran_val) => {
                    match oran_val {
                        OranValue::Variable(v) => {
                            let array = match OranValue::from(v.value) {
                                OranValue::Array(a) => a,
                                _ => {
                                    show_error_message("This variable isn't array.".to_owned(), filename, pair);
                                    process::exit(1);
                                }
                            };
                            array
                        }
                        _ => {
                            show_error_message("This variable isn't array.".to_owned(), filename, pair);
                            process::exit(1);
                        }
                    }
                }
            };
            let i_name = OranString::from(i);
            let this_for_loop_scope = scope + 1;
            for array_v in &array {
                env.insert(
                    (this_for_loop_scope, FunctionOrValueType::Value, i_name.clone()),
                    OranValue::Variable(OranVariable {
                        var_type: *var_type,
                        name: i,
                        value: OranVariableValue::from(array_v)
                    })
                );
                let mut returned_val: OranValue;
                for stmt in stmts {
                    returned_val = interp_expr(this_for_loop_scope, env, stmt, filename, pair);
                    match returned_val {
                        OranValue::Null => {},
                        _ => { return returned_val }
                    }
                }
            }
                
            // delete unnecessary data when exiting a scope
            // TODO garbage colloctor
            env.retain(|
                (s,
                __k,
                _label),
                _orn_val|*s != this_for_loop_scope
            );
            OranValue::Null
        },
        AstNode::ForLoopArray(var_type, i, array, stmts) => {
            let i_name = OranString::from(i);
            let this_for_loop_scope = scope + 1;
            for array_v in array {
                let val = interp_expr(scope, env, array_v, filename, pair);
                env.insert(
                    (this_for_loop_scope, FunctionOrValueType::Value, i_name.clone()),
                    OranValue::Variable(OranVariable {
                        var_type: *var_type,
                        name: i,
                        value: OranVariableValue::from(val)
                    })
                );
                let mut returned_val: OranValue;
                for stmt in stmts {
                    returned_val = interp_expr(this_for_loop_scope, env, stmt, filename, pair);
                    match returned_val {
                        OranValue::Null => {},
                        _ => { return returned_val }
                    }
                }
            }
                
            // delete unnecessary data when exiting a scope
            // TODO garbage colloctor
            env.retain(|
                (s,
                __k,
                _label),
                _orn_val|*s != this_for_loop_scope
            );
            OranValue::Null
        },
        AstNode::Array(array) => {
            let oran_vals: Vec<OranValue> = array.iter()
                                            .map(|v| 
                                                interp_expr(scope, env, v, filename, pair)
                                            )
                                            .collect::<Vec<OranValue>>();
            OranValue::Array(oran_vals)
        },
        AstNode::ArrayElement(array_ast, indexes) => {
            let mut last_val: Option<OranValue> = None;
            for index in indexes.iter() {
                let usize_index = f64::from(interp_expr(scope, env, index, filename, pair)) as usize;
                if last_val == None {
                    last_val = Some(interp_expr(scope, env, array_ast, filename, pair));
                }
                let array = last_val.unwrap();
                last_val = Some(match array {
                    OranValue::Array(a) => {
                        a.into_iter().nth(usize_index).unwrap_or_else(||
                            {
                                show_error_message("This index doesn't exist in the specified array.".to_owned(), filename, pair);
                                process::exit(1);
                            })
                    },
                    OranValue::Variable(v) => {
                        let usize_index = f64::from(interp_expr(scope, env, index, filename, pair)) as usize;
                        let array = match OranValue::from(v.value) {
                            OranValue::Array(a) => a,
                            _ => {
                                show_error_message("This variable isn't array.".to_owned(), filename, pair);
                                process::exit(1);
                            }
                        };
                        array.into_iter().nth(usize_index).unwrap_or_else(||
                            {
                                show_error_message("This index doesn't exist in the specified array.".to_owned(), filename, pair);
                                process::exit(1);
                            })
                    }
                    _ => {
                        let message = format!("{}{}{}", "\"".to_owned(), &String::from(array), "\" is not array but index was specified.".to_owned());
                        show_error_message(message, filename, pair);
                        process::exit(1);
                    }
                });
            }
            last_val.unwrap()
        }
        AstNode::Null => OranValue::Null,
        //_ => unreachable!("{:?}", reduced_expr)
    }
}

fn add<'a, 'b>(a: OranValue<'a>, other: OranValue<'a>, filename: &'b str, pair: &'b Pair<'b, Rule>,) -> OranValue<'a> {
    match a {
        OranValue::Float(ref fl) => { OranValue::Float(fl + f64::from(other)) },
        OranValue::Str(ref s) => OranValue::Float(s.val_str.as_ref().parse::<f64>().unwrap_or_else(|_|
            {
                let message = format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned());
                show_error_message(message, filename, pair);
                process::exit(1);
            }
        ) + f64::from(other)),
        OranValue::Variable(ref v) => { 
            match v.value {
                OranVariableValue::Float(ref vfl) => { OranValue::Float(vfl + f64::from(other)) },
                OranVariableValue::Str(ref s) => {
                    OranValue::Float(s.val_str.as_ref().parse::<f64>().unwrap_or_else(|_|{
                        let message = format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned());
                        show_error_message(message, filename, pair);
                        process::exit(1);
                    }) + f64::from(other))
                },
                _ => {
                    let message = format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned());
                    show_error_message(message, filename, pair);
                    process::exit(1);
                }
            }
        },
        _ => {
            let message = format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned());
            show_error_message(message, filename, pair);
            process::exit(1);
        }
    }
}

fn sub<'a, 'b>(a: OranValue<'a>, other: OranValue<'a>, filename: &'b str, pair: &'b Pair<'b, Rule>,) -> OranValue<'a> {
    match a {
        OranValue::Float(ref fl) => { OranValue::Float(fl - f64::from(other)) },
        OranValue::Str(ref s) => OranValue::Float(s.val_str.as_ref().parse::<f64>().unwrap_or_else(|_|
            {
                let message = format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned());
                show_error_message(message, filename, pair);
                process::exit(1);
            }
        ) - f64::from(other)),
        OranValue::Variable(ref v) => { 
            match v.value {
                OranVariableValue::Float(ref vfl) => { OranValue::Float(vfl - f64::from(other)) },
                OranVariableValue::Str(ref s) => {
                    OranValue::Float(s.val_str.as_ref().parse::<f64>().unwrap_or_else(|_|{
                        let message = format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned());
                        show_error_message(message, filename, pair);
                        process::exit(1);
                    }) - f64::from(other))
                },
                _ => {
                    let message = format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned());
                    show_error_message(message, filename, pair);
                    process::exit(1);
                }
            }
        },
        _ => {
            let message = format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned());
            show_error_message(message, filename, pair);
            process::exit(1);
        }
    }
}

fn mul<'a, 'b>(a: OranValue<'a>, other: OranValue<'a>, filename: &'b str, pair: &'b Pair<'b, Rule>,) -> OranValue<'a> {
    match a {
        OranValue::Float(ref fl) => { OranValue::Float(fl * f64::from(other)) },
        OranValue::Str(ref s) => OranValue::Float(s.val_str.as_ref().parse::<f64>().unwrap_or_else(|_|
            {
                let message = format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned());
                show_error_message(message, filename, pair);
                process::exit(1);
            }
        ) * f64::from(other)),
        OranValue::Variable(ref v) => { 
            match v.value {
                OranVariableValue::Float(ref vfl) => { OranValue::Float(vfl * f64::from(other)) },
                OranVariableValue::Str(ref s) => {
                    OranValue::Float(s.val_str.as_ref().parse::<f64>().unwrap_or_else(|_|{
                        let message = format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned());
                        show_error_message(message, filename, pair);
                        process::exit(1);
                    }) * f64::from(other))
                },
                _ => {
                    let message = format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned());
                    show_error_message(message, filename, pair);
                    process::exit(1);
                }
            }
        },
        _ => {
            let message = format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned());
            show_error_message(message, filename, pair);
            process::exit(1);
        }
    }
}

fn div<'a, 'b>(a: OranValue<'a>, other: OranValue<'a>, filename: &'b str, pair: &'b Pair<'b, Rule>,) -> OranValue<'a> {
    match a {
        OranValue::Float(ref fl) => { OranValue::Float(fl / f64::from(other)) },
        OranValue::Str(ref s) => OranValue::Float(s.val_str.as_ref().parse::<f64>().unwrap_or_else(|_|
            {
                let message = format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned());
                show_error_message(message, filename, pair);
                process::exit(1);
            }
        ) / f64::from(other)),
        OranValue::Variable(ref v) => { 
            match v.value {
                OranVariableValue::Float(ref vfl) => { OranValue::Float(vfl / f64::from(other)) },
                OranVariableValue::Str(ref s) => {
                    OranValue::Float(s.val_str.as_ref().parse::<f64>().unwrap_or_else(|_|{
                        let message = format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned());
                        show_error_message(message, filename, pair);
                        process::exit(1);
                    }) / f64::from(other))
                },
                _ => {
                    let message = format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned());
                    show_error_message(message, filename, pair);
                    process::exit(1);
                }
            }
        },
        _ => {
            let message = format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned());
            show_error_message(message, filename, pair);
            process::exit(1);
        }
    }
}

fn rem<'a, 'b>(a: OranValue<'a>, other: OranValue<'a>, filename: &'b str, pair: &'b Pair<'b, Rule>,) -> OranValue<'a> {
    match a {
        OranValue::Float(ref fl) => { OranValue::Float(fl % f64::from(other)) },
        OranValue::Str(ref s) => OranValue::Float(s.val_str.as_ref().parse::<f64>().unwrap_or_else(|_|
            {
                let message = format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned());
                show_error_message(message, filename, pair);
                process::exit(1);
            }
        ) % f64::from(other)),
        OranValue::Variable(ref v) => { 
            match v.value {
                OranVariableValue::Float(ref vfl) => { OranValue::Float(vfl % f64::from(other)) },
                OranVariableValue::Str(ref s) => {
                    OranValue::Float(s.val_str.as_ref().parse::<f64>().unwrap_or_else(|_|{
                        let message = format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned());
                        show_error_message(message, filename, pair);
                        process::exit(1);
                    }) % f64::from(other))
                },
                _ => {
                    let message = format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned());
                    show_error_message(message, filename, pair);
                    process::exit(1);
                }
            }
        },
        _ => {
            let message = format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned());
            show_error_message(message, filename, pair);
            process::exit(1);
        }
    }
}

fn pow<'a, 'b>(a: OranValue<'a>, other: OranValue<'a>, filename: &'b str, pair: &'b Pair<'b, Rule>) -> OranValue<'a> {
    match a {
        OranValue::Float(ref fl) => { OranValue::Float(fl.powf(f64::from(other))) },
        OranValue::Str(ref s) => OranValue::Float(s.val_str.as_ref().parse::<f64>().unwrap_or_else(|_|
            {
                let message = format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned());
                show_error_message(message, filename, pair);
                process::exit(1);
            }
        ).powf(f64::from(other))),
        OranValue::Variable(ref v) => { 
            match v.value {
                OranVariableValue::Float(ref vfl) => { OranValue::Float(vfl.powf(f64::from(other))) },
                OranVariableValue::Str(ref s) => {
                    OranValue::Float(s.val_str.as_ref().parse::<f64>().unwrap_or_else(|_|{
                        let message = format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned());
                        show_error_message(message, filename, pair);
                        process::exit(1);
                    }).powf(f64::from(other)))
                },
                _ => {
                    let message = format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned());
                    show_error_message(message, filename, pair);
                    process::exit(1);
                }
            }
        },
        _ => {
            let message = format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned());
            show_error_message(message, filename, pair);
            process::exit(1);
        }
    }
}

fn show_error_message<'a>(message: String, filename: &'a str, pair: &'a Pair<'a, Rule>) {
    let error: Error<Rule> = Error::new_from_span(
        ErrorVariant::CustomError{
            message: message
        },
        pair.as_span()
    ).with_path(filename);
    println!("Runtime Error!{}", error);
}
