use crate::parser::astnode::{AstNode, CalcOp, Function, LogicalOperatorType, ComparisonlOperatorType};
use crate::value::oran_value::{OranValue, FunctionDefine};
use crate::value::oran_variable::{OranVariable, OranVariableValue};
use crate::value::oran_string::OranString;
use crate::value::var_type::{VarType, OranValueType};
use std::collections::HashMap;
use std::io::{self, Write};
use std::borrow::Cow;

pub fn interp_expr<'a>(scope: usize, env : &mut HashMap<(usize, OranValueType, OranString<'a>), OranValue<'a>>, reduced_expr: &'a AstNode, var_type: OranValueType) -> OranValue<'a> {
    match reduced_expr {
        AstNode::Number(ref double) => OranValue::Float(*double),
        AstNode::Calc (ref verb, ref lhs, ref rhs ) => {
            match verb {
                CalcOp::Plus => { interp_expr(scope, env, lhs, var_type) + interp_expr(scope, env, rhs, var_type) }
                CalcOp::Minus => { interp_expr(scope, env, lhs, var_type) - interp_expr(scope, env, rhs, var_type) }
                CalcOp::Times => { interp_expr(scope, env, lhs, var_type) * interp_expr(scope, env, rhs, var_type) }
                CalcOp::Divide => { interp_expr(scope, env, lhs, var_type) / interp_expr(scope, env, rhs, var_type) }
                CalcOp::Modulus => { interp_expr(scope, env, lhs, var_type) % interp_expr(scope, env, rhs, var_type) }
                CalcOp::Power => {
                    let base: f64 = f64::from(interp_expr(scope, env, lhs, var_type));
                    let pow: f64 = f64::from(interp_expr(scope, env, rhs, var_type));
                    let val = base.powf(pow);
                    OranValue::Float(val)
                }
            }
        }
        AstNode::Ident(ref ident) => {
            let val = &*env.get(
                &(
                    scope,
                    var_type,
                    OranString::from(ident)
                )
            ).unwrap_or_else(
                ||
                &*env.get(
                    &(scope, OranValueType::Value, OranString::from(ident))
                ).unwrap_or_else(
                    || panic!("The variable \"{}\" is not defined.", ident)
                )
            );
            val.clone()
        }
        AstNode::Assign(ref variable_type, ref ident, ref expr) => {
            is_mutabile(scope, env, ident, variable_type);
            let oran_val = OranValue::Variable(OranVariable {
                var_type: *variable_type,
                name: ident,
                value: OranVariableValue::from(&interp_expr(scope, env, expr, var_type)),
            });
            env.insert((scope, var_type, OranString::from(ident)), oran_val.clone());
            oran_val
        }
        AstNode::FunctionCall(ref func_ast, ref name, ref arg_values) => {
            match func_ast {
                Function::Print => {
                    let mut text = "".to_string();
                    for str in arg_values {
                        text.push_str(&String::from(&interp_expr(scope, env, &str, var_type)))
                    }
                    print!("{}", text);
                    io::stdout().flush().unwrap();
                    OranValue::Boolean(true)
                },
                Function::Println => {
                    let mut text = "".to_string();
                    for str in arg_values {
                        text.push_str(&String::from(&interp_expr(scope, env, &str, var_type)))
                    }
                    println!("{}", text);
                    OranValue::Boolean(true)
                },
                Function::NotDefault => {
                    let func = *&env.get(&(scope, OranValueType::Function, OranString::from(name)));
                    let called_func = match func {
                        None => panic!("Function {} is not defined.", name),
                        _ => func.unwrap()
                    };
                    let func = FunctionDefine::from(called_func);
                    for i in 0..func.args.len() {
                        let arg_name = interp_expr(scope+1, env, func.args.into_iter().nth(i).unwrap(), var_type);
                        let arg_ast = arg_values.into_iter().nth(i).unwrap_or_else(|| panic!("Argument is necessary but not supplied."));
                        let val = interp_expr(scope, env, arg_ast, var_type);
                        env.insert((scope+1, var_type, OranString::from(arg_name)), val);
                    }
                    for body in func.body {
                        interp_expr(scope+1, env, &body, var_type);
                    }
                    let val = interp_expr(scope+1, env, func.fn_return, var_type);
                    // delete unnecessary data when exiting a scope
                    // TODO garbage colloctor
                    // TODO conditional return in functions
                    env.retain(|(s, __k, _label), _val| *s != scope+1);
                    val
                }
            }
        }
        AstNode::FunctionDefine(ref func_name, ref args, ref astnodes, ref fn_return) => {
            let val = OranValue::Function(FunctionDefine {
                name: func_name,
                args: args,
                body: astnodes,
                fn_return: fn_return
            });
            env.insert((scope, OranValueType::Function, OranString::from(func_name)), val.clone());
            val
        }
        AstNode::Argument(ref argument_name, ref val) => {
            let val = interp_expr(scope, env, val, var_type);
            env.insert((scope, OranValueType::Value, OranString::from(argument_name)), val);
            OranValue::Str(OranString::from(argument_name))
        }
        AstNode::Str (ref str) => {
            OranValue::Str(OranString::from(str))
        }
        AstNode::Strs (strs) => {
            let mut text = "".to_string();
            for str in strs {
                text.push_str(&String::from(interp_expr(scope, env, &str, var_type)))
            }
            OranValue::Str(OranString {
                val_str: Some(Cow::from(text))
            })
        }
        AstNode::Condition (ref c, ref e, ref o) => {
            let e = interp_expr(scope, env, e, var_type);
            let o = interp_expr(scope, env, o, var_type);
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
        AstNode::Comparison (ref e, ref c, ref o) => {
            let e = interp_expr(scope, env, e, var_type);
            let o = interp_expr(scope, env, o, var_type);
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
        AstNode::IF(ref if_conditions, ref body, else_if_conditions, ref else_if_bodies, ref else_bodies) => {
            // if
            let condition_result = interp_expr(scope, env, if_conditions, var_type);
            if bool::from(condition_result) {
                for astnode in body {
                    interp_expr(scope, env, &astnode, var_type);
                }
                return OranValue::Null;
            }
            // else if
            if !else_if_conditions.is_empty() {
                for i in 0..else_if_conditions.len() {
                    let conditions = else_if_conditions.into_iter().nth(i).unwrap();
                    let result = interp_expr(scope, env, conditions, var_type);
                    if bool::from(result) {
                        for astnode in else_if_bodies.into_iter().nth(i).unwrap() {
                            interp_expr(scope, env, &astnode, var_type);
                        }
                        return OranValue::Null;
                    }
                }        
            }
            // else
            if !else_bodies.is_empty() {
                for astnode in else_bodies {
                    interp_expr(scope, env, astnode, var_type);
                }
            }
            //env.retain(|(_s, k, _label), _val| *k != OranValueType::Temp);
            OranValue::Null
        }
        AstNode::Bool (b) => {
            OranValue::Boolean(*b)
        }
        AstNode::ForLoop(is_inclusive, var_type, i, first, last, stmts) => {
            let first = interp_expr(scope, env, first, OranValueType::Value);
            let first = f64::from(first).round() as i64;
            let last = interp_expr(scope, env, last, OranValueType::Value);
            let last = f64::from(last).round() as i64;
            let i_name = OranString::from(i);
            match is_inclusive {
                true => {
                    for num in first..=last {
                        &env.insert(
                            (scope, OranValueType::Value, i_name.clone()),
                            OranValue::Variable(OranVariable {
                                var_type: *var_type,
                                name: i,
                                value: OranVariableValue::Float(num as f64)
                            })
                        );
                        for stmt in stmts {
                            interp_expr(scope, env, stmt, OranValueType::Value);
                        }
                        //env.retain(|(_s, k, _label), _val| *k != OranValueType::Temp);
                    }
                }
                false => {
                    for num in first..last {
                        &env.insert(
                            (scope, OranValueType::Value, i_name.clone()),
                            OranValue::Variable(OranVariable {
                                var_type: *var_type,
                                name: i,
                                value: OranVariableValue::Float(num as f64)
                            })
                        );
                        for stmt in stmts {
                            interp_expr(scope, env, stmt, OranValueType::Value);
                        }
                        //env.retain(|(_s, k, _label), _val| *k != OranValueType::Temp);
                    }
                }
            }
            
            OranValue::Null
        }
        AstNode::Null => OranValue::Null,
        //_ => unreachable!("{:?}", reduced_expr)
    }
}

fn is_mutabile<'a> (scope: usize, env : &mut HashMap<(usize, OranValueType, OranString<'a>), OranValue<'a>>, ident: &str, variable_type: &VarType) -> bool {
    let mut val = env.get(
        &(
            scope,
            OranValueType::Value,
            OranString::from(ident)
        )
    );
    if val == None {
        val = env.get(
            &(
                scope,
                OranValueType::Temp,
                OranString::from(ident)
            )
        );
    }
    if *variable_type == VarType::VariableReAssigned && val != None {
        if OranVariable::from(val.unwrap()).var_type == VarType::Constant {
            panic!("You can't assign value twice to a constant variable.");
        }
    } else if *variable_type == VarType::VariableReAssigned && val == None {
        panic!("You can't assign value without 'let'.");
    }
    true
}