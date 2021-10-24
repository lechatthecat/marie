use crate::parser::astnode::{AstNode, CalcOp, LogicalOperatorType, ComparisonlOperatorType};
use crate::value::oran_value::{OranValue, FunctionDefine};
use crate::value::oran_variable::{OranVariable, OranVariableValue};
use crate::value::oran_string::OranString;
use crate::value::oran_scope::OranScope;
use crate::value::scope::ROOT_SCOPE;
use crate::value::var_type::{FunctionOrValueType, VarType};
use colored::*;
use std::collections::HashMap;
use std::io::{self, Write};
use std::borrow::Cow;
use num_traits::Pow;
use std::process;
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
    reduced_expr: &'b AstNode
    ) -> OranValue<'a> {

    match reduced_expr {
        AstNode::Number(_location, double) => OranValue::Float(*double),
        AstNode::Calc (verb, lhs, rhs) => {
            match verb {
                CalcOp::Plus => { interp_expr(scope, env, lhs) + interp_expr(scope, env, rhs) }
                CalcOp::Minus => { interp_expr(scope, env, lhs) - interp_expr(scope, env, rhs) }
                CalcOp::Times => { interp_expr(scope, env, lhs) * interp_expr(scope, env, rhs) }
                CalcOp::Divide => { interp_expr(scope, env, lhs) / interp_expr(scope, env, rhs) }
                CalcOp::Modulus => { interp_expr(scope, env, lhs) % interp_expr(scope, env, rhs) }
                CalcOp::Power => {Pow::pow(interp_expr(scope, env, lhs), interp_expr(scope, env, rhs))}
            }
        }
        AstNode::Ident(location, ident) => {
            let val = &env.get(
                &(
                    scope,
                    FunctionOrValueType::Value,
                    OranString::from(ident)
                )
            );
            let val: &OranValue = match val {
                None => {
                    //If not found, then try to get function definition from higher levels.
                    let mut check_scope = scope.clone();
                    let val = loop {
                        if check_scope == ROOT_SCOPE {
                            break None;
                        }
                        check_scope = check_scope - 1;
                        let found = &env.get(&(check_scope, FunctionOrValueType::Value, OranString::from(ident)));
                        match found {
                            None => {
                                continue;
                            },
                            Some(found) => {
                                break Some(found.clone());
                            }
                        };
                    };
                    match val {
                        Some(oran_val) => {
                            oran_val
                        }
                        None => {
                            println!("{}\n{}\nLine number: {}, column number:{}: The variable \"{}\" is not defined.",
                                "Error!".red().bold(),    
                                location.0,    
                                location.1,
                                location.2,
                                ident
                            );
                            process::exit(1);
                        }
                    }

                },
                Some(oran_val) => oran_val
            };
            val.clone()
        }
        AstNode::Assign(location, variable_type, ident, expr) => {
            util::is_mutable(location, scope, env, ident, variable_type);

            if *variable_type == VarType::VariableReAssigned && *variable_type == VarType::VariableReAssigned {
                let val = &env.get(
                    &(
                        scope,
                        FunctionOrValueType::Value,
                        OranString::from(ident)
                    )
                );
                match val {
                    None => {
                        //If not found, then try to get function definition from higher levels.
                        let mut check_scope = scope.clone();
                        loop {
                            if check_scope == ROOT_SCOPE {
                                break;
                            }
                            check_scope = check_scope - 1;
                            let found = &env.get(&(check_scope, FunctionOrValueType::Value, OranString::from(ident)));
                            match found {
                                None => {
                                    continue;
                                },
                                Some(_) => {
                                    let oran_val = OranValue::Variable(OranVariable {
                                        var_type: *variable_type,
                                        name: ident,
                                        value: OranVariableValue::from(&interp_expr(scope, env, expr)),
                                    });
                                    env.insert((check_scope, FunctionOrValueType::Value, OranString::from(ident)), oran_val.clone());
                                    break;
                                }
                            };
                        };

                    },
                    Some(_) => {
                        let oran_val = OranValue::Variable(OranVariable {
                            var_type: *variable_type,
                            name: ident,
                            value: OranVariableValue::from(&interp_expr(scope, env, expr)),
                        });
                        env.insert((scope, FunctionOrValueType::Value, OranString::from(ident)), oran_val.clone());
                    }
                };
            } else {
                let oran_val = OranValue::Variable(OranVariable {
                    var_type: *variable_type,
                    name: ident,
                    value: OranVariableValue::from(&interp_expr(scope, env, expr)),
                });
                env.insert((scope, FunctionOrValueType::Value, OranString::from(ident)), oran_val.clone());
            }
            OranValue::Null
        }
        AstNode::FunctionCall(location, name, arg_values) => {
            match name.as_ref() {
                "print" => {
                    let mut text = "".to_owned();
                    for str in arg_values {
                        text.push_str(&String::from(&interp_expr(scope, env, &str)))
                    }
                    print!("{}", text);
                    io::stdout().flush().unwrap();
                    OranValue::Null
                },
                "println" => {
                    let mut text = "".to_owned();
                    for str in arg_values {
                        text.push_str(&String::from(&interp_expr(scope, env, &str)))
                    }
                    println!("{}", text);
                    OranValue::Null
                },
                _ => {
                    //Try to get function definition from current scope.
                    let this_func_scope = scope+1;
                    let func = *&env.get(&(scope, FunctionOrValueType::Function, OranString::from(name)));
                    let func: &OranValue = match func {
                        None => {
                            //If not found, then try to get function definition from higher levels.
                            let mut check_scope = scope.clone();
                            let func_defined = loop {
                                if check_scope == ROOT_SCOPE {
                                    break None;
                                }
                                check_scope = check_scope - 1;
                                let found = &env.get(&(check_scope, FunctionOrValueType::Function, OranString::from(name)));
                                match found {
                                    None => {
                                        continue;
                                    },
                                    Some(found) => {
                                        break Some(found.clone());
                                    }
                                };
                            };
                            match func_defined {
                                Some(oran_val) => {
                                    oran_val
                                }
                                None => {
                                    println!("{}\n{}\nLine number: {}, column number:{}: Function \"{}\" is not defined.",
                                        "Error!".red().bold(),    
                                        location.0,    
                                        location.1,
                                        location.2,
                                        name
                                    );
                                    process::exit(1);
                                }
                            }

                        },
                        _ => func.unwrap()
                    };
                    let func = FunctionDefine::from(func);
                    for i in 0..func.args.len() {
                        let arg_name = interp_expr(this_func_scope, env, func.args.into_iter().nth(i).unwrap());
                        let arg_ast = arg_values.into_iter().nth(i).unwrap_or_else(||
                            {
                                println!("{}\n{}\nLine number: {}, column number:{}: Argument is necessary but not supplied.",
                                    "Error!".red().bold(),    
                                    location.0,    
                                    location.1,
                                    location.2,
                                );
                                process::exit(1);
                            });
                        let val = interp_expr(scope, env, arg_ast);
                        env.insert((this_func_scope, FunctionOrValueType::Value, OranString::from(arg_name)), val);
                    }
                    let mut returned_val = OranValue::Null;
                    for body in func.body {
                        returned_val = interp_expr(this_func_scope, env, &body);
                        match returned_val {
                            OranValue::Null => {}
                            _ => { break }
                        }
                    }
                    match returned_val {
                        OranValue::Null => {
                            returned_val = interp_expr(this_func_scope, env, func.fn_return);
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
        AstNode::FunctionDefine(_location, func_name, args, astnodes, fn_return) => {
            let val = OranValue::Function(FunctionDefine {
                name: func_name,
                args: args,
                body: astnodes,
                fn_return: fn_return
            });
            env.insert((scope, FunctionOrValueType::Function, OranString::from(func_name)), val.clone());
            val
        }
        AstNode::Argument(_location, argument_name, val) => {
            let val = interp_expr(scope, env, val);
            env.insert((scope, FunctionOrValueType::Value, OranString::from(argument_name)), val);
            OranValue::Str(OranString::from(argument_name))
        }
        AstNode::Str (_location, str_val) => {
            OranValue::Str(OranString::from(str_val))
        }
        AstNode::Strs (_location, strs) => {
            let mut text = "".to_owned();
            for str in strs {
                text.push_str(&String::from(interp_expr(scope, env, &str)))
            }
            OranValue::Str(OranString {
                val_str: Cow::from(text)
            })
        }
        AstNode::Condition (c, e, o) => {
            let e = interp_expr(scope, env, e);
            let o = interp_expr(scope, env, o);
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
        AstNode::Comparison (location, e, c, o) => {
            let e = interp_expr(scope, env, e);
            let o = interp_expr(scope, env, o);

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
                        println!("{}\n{}\nLine number: {}, column number:{}: One of these are not number: {}, {}",
                            "Error!".red().bold(),    
                            location.0,    
                            location.1,
                            location.2,
                            e,
                            o
                        );
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
        AstNode::IF(_location, if_conditions, body, else_if_bodies_conditions, else_bodies) => {
            // if
            let condition_result = interp_expr(scope, env, if_conditions);
            if bool::from(condition_result) {
                let mut returned_val =  OranValue::Null;
                for astnode in body {
                    returned_val = interp_expr(scope, env, &astnode);
                }
                return returned_val;
            }
            // else if
            let mut _is_all_false = true;
            if !else_if_bodies_conditions.is_empty() {
                for (conditions, else_if_body) in else_if_bodies_conditions {
                    for c in conditions {
                        let result = interp_expr(scope, env, &c);
                        if bool::from(result) {
                            _is_all_false = false;
                            let mut returned_val =  OranValue::Null;
                            for astnode in else_if_body {
                                returned_val = interp_expr(scope, env, &astnode);
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
                    returned_val = interp_expr(scope, env, astnode);
                }
                return returned_val;
            }
            OranValue::Null
        }
        AstNode::Bool (_location, b) => {
            OranValue::Boolean(*b)
        }
        AstNode::ForLoop(_location, is_inclusive, var_type, i, first, last, stmts) => {
            let first = interp_expr(scope, env, first);
            let first = f64::from(first).round() as i64;
            let last = interp_expr(scope, env, last);
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
                            returned_val = interp_expr(this_for_loop_scope, env, stmt);
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
                            returned_val = interp_expr(this_for_loop_scope, env, stmt);
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
            env.retain(|(s, __k, _label), _orn_val| *s != this_for_loop_scope);
            OranValue::Null
        }
        AstNode::Null => OranValue::Null,
        //_ => unreachable!("{:?}", reduced_expr)
    }
}
