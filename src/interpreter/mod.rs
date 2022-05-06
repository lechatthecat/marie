use crate::parser::astnode::{AstNode, CalcOp, LogicalOperatorType, ComparisonlOperatorType};
use crate::value::oran_value::{OranValue, FunctionDefine};
use crate::value::oran_variable::{OranVariable, OranVariableValue};
use crate::value::oran_string::OranString;
use crate::value::oran_scope::OranScope;
use crate::value::oran_error::OranError;
use crate::value::var_type::{FunctionOrValueType, VarType};
use std::collections::HashMap;
use std::io::{self, Write};
use std::borrow::Cow;
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
    ) -> Result<OranValue<'a>, OranError<'a>> {

    match reduced_expr {
        AstNode::Number(double) => Ok(OranValue::Float(*double)),
        AstNode::Calc (verb, lhs, rhs) => {
            let lhs_val = interp_expr(scope, env, lhs);
            let rhs_val = interp_expr(scope, env, rhs);
            match verb {
                CalcOp::Plus => { add(lhs_val?, rhs_val?) }
                CalcOp::Minus => { sub(lhs_val?, rhs_val?) }
                CalcOp::Times => { mul(lhs_val?, rhs_val?) }
                CalcOp::Divide => { div(lhs_val?, rhs_val?) }
                CalcOp::Modulus => { rem(lhs_val?, rhs_val?) }
                CalcOp::Power => { pow(lhs_val?, rhs_val?) }
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
                    Err(
                        OranError{
                            message: format!("{}{}{}", "The variable \"".to_owned(), ident, "\" is not defined."),
                            pair: None
                        }
                    )
                },
                Some(oran_val) => Ok(oran_val)
            };
            val
        }
        AstNode::ArrayElementAssign(variable_type, array_name, index, expr) => {
            util::is_mutable(scope, env, array_name, variable_type)?;
            let usize_index = f64::from(interp_expr(scope, env, index)?) as usize;
            let val = util::get_val(
                scope,
                env,
                array_name,
                FunctionOrValueType::Value
            );
            match val.0 {
                None => {
                    Err(
                        OranError{
                            message: format!("{}{}{}", "This array \"".to_owned(), array_name, "\" doesn't exist."),
                            pair: None
                        }
                    )
                },
                Some(v) => {
                    let array = match OranValue::from(v) {
                        OranValue::Array(a) => Ok(a),
                        OranValue::Variable(v) => {
                            let array = match OranValue::from(v.value) {
                                OranValue::Array(a) => Ok(a),
                                _ => {
                                    Err(OranError {
                                        message: "This variable isn't array.".to_owned(),
                                        pair: None
                                    })
                                }
                            };
                            array
                        }
                        _ => {
                            Err(OranError {
                                    message: "This variable isn't array.".to_owned(),
                                    pair: None
                                })
                        }
                    };
                    let mut array = array?;
                    array[usize_index] = interp_expr(scope, env, expr)?;
                    Ok(
                        env.insert(
                            (
                                scope,
                                FunctionOrValueType::Value,
                                OranString::from(array_name)
                            ),
                            OranValue::Array(array)
                        )
                    )
                }
            }?;
            Ok(OranValue::Null)
        }
        AstNode::Assign(variable_type, ident, expr) => {
            util::is_mutable(scope, env, ident, variable_type)?;
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
                    value: OranVariableValue::from(&interp_expr(scope, env, expr)?),
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
                    value: OranVariableValue::from(&interp_expr(scope, env, expr)?),
                });
                env.insert((scope, FunctionOrValueType::Value, OranString::from(ident)), oran_val);
            }
            Ok(OranValue::Null)
        }
        AstNode::FunctionCall(name, arg_values) => {
            match name.as_ref() {
                "print" => {
                    let mut text = "".to_owned();
                    for string_pair in arg_values.into_iter(){
                        let string_val = &String::from(&interp_expr(scope, env, &string_pair)?);
                        text.push_str(string_val);
                    }
                    print!("{}", text);
                    io::stdout().flush().unwrap();
                    Ok(OranValue::Null)
                },
                "println" => {
                    let mut text = "".to_owned();
                    for string_pair in arg_values.into_iter(){
                        let string_val = &String::from(&interp_expr(scope, env, &string_pair)?);
                        text.push_str(string_val);
                    }
                    println!("{}", text);
                    Ok(OranValue::Null)
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
                    let func = match func_tuple.0 {
                        None => Err(OranError {
                            message: format!("{}{}{}", "Function \"".to_owned(), name, "\" is not defined."),
                            pair: None
                        }),
                        Some(v) => Ok(v)
                    };
                    let func = FunctionDefine::from(&func?);
                    for i in 0..func.args.len() {
                        let nth_arg = func.args.into_iter().nth(i);
                        let nth_arg = match nth_arg {
                            Some(arg_val) => Ok(arg_val),
                            None => Err(OranError {
                                message: "Argument is necessary but not supplied.".to_owned(),
                                pair: None
                            })
                        }?;
                        let arg_name = interp_expr(this_func_scope, env, nth_arg)?;
                        let arg_ast = arg_values.into_iter().nth(i);
                        let arg_ast = match arg_ast {
                            Some(val) => Ok(val),
                            None =>  Err(OranError {
                                message: "Argument is necessary but not supplied.".to_owned(),
                                pair: None
                            })
                        };
                        let val = interp_expr(scope, env, arg_ast?)?;
                        env.insert((this_func_scope, FunctionOrValueType::Value, OranString::from(arg_name)), val);
                    }
                    let mut returned_val = OranValue::Null;
                    for body in func.body {
                        let returned_val_result = interp_expr(this_func_scope, env, &body.0);
                        match returned_val_result {
                            Ok(v) => {
                                match v {
                                    OranValue::Null => { Ok(())},
                                    _ => { 
                                        returned_val = v;
                                        break; 
                                    }
                                }
                            },
                            Err(mut err) => {
                                if err.pair == None {
                                    err.pair = Some(&body.1);
                                }
                                Err(err)
                            }
                        }?;
                    }
                    match returned_val {
                        OranValue::Null => {
                            returned_val = interp_expr(this_func_scope, env, func.fn_return)?;
                        }
                        _ => {}
                    }
                    // delete unnecessary data when exiting a scope
                    // TODO garbage colloctor
                    env.retain(|(s, __k, _label), _orn_val| *s != this_func_scope);
                    Ok(returned_val)
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
            Ok(val)
        }
        AstNode::Argument(argument_name, val) => {
            let val = interp_expr(scope, env, val);
            env.insert((scope, FunctionOrValueType::Value, OranString::from(argument_name)), val?);
            Ok(OranValue::Str(OranString::from(argument_name)))
        }
        AstNode::Str (str_val) => {
            Ok(OranValue::Str(OranString::from(str_val)))
        }
        AstNode::Strs (strs) => {
            let mut text = "".to_owned();
            for str in strs {
                text.push_str(&String::from(interp_expr(scope, env, &str)?))
            }
            Ok(OranValue::Str(OranString {
                val_str: Cow::from(text)
            }))
        }
        AstNode::Condition (c, e, o) => {
            let e = interp_expr(scope, env, e);
            let o = interp_expr(scope, env, o);
            match c {
                ComparisonlOperatorType::AND => {
                    if bool::from(e?) && bool::from(o?) {
                        return Ok(OranValue::Boolean(true));
                    }
                    Ok(OranValue::Boolean(false))
                }
                ComparisonlOperatorType::OR => {
                    if bool::from(e?) || bool::from(o?) {
                        return Ok(OranValue::Boolean(true));
                    }
                    Ok(OranValue::Boolean(false))
                }
            }
        }
        AstNode::Comparison (e, c, o) => {
            let e = interp_expr(scope, env, e)?;
            let o = interp_expr(scope, env, o)?;

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
                            return Ok(OranValue::Boolean(true));
                        }
                        Ok(OranValue::Boolean(false))
                    },
                    _ => {
                        Err(OranError{
                            message: format!("{}{}{}{}", "One of these are not number: ".to_owned(), String::from(e), ", ", String::from(o)),
                            pair: None
                        })
                    },
                }
            } else {
                match c {
                    LogicalOperatorType::Equal => {
                        if e == o {
                            return Ok(OranValue::Boolean(true));
                        }
                        Ok(OranValue::Boolean(false))
                    },
                    LogicalOperatorType::BiggerThan => {
                        if e > o {
                            return Ok(OranValue::Boolean(true));
                        }
                        Ok(OranValue::Boolean(false))
                    },
                    LogicalOperatorType::SmallerThan => {
                        if e < o {
                            return Ok(OranValue::Boolean(true));
                        }
                        Ok(OranValue::Boolean(false))
                    },
                    LogicalOperatorType::EbiggerThan => {
                        if e >= o {
                            return Ok(OranValue::Boolean(true));
                        }
                        Ok(OranValue::Boolean(false))
                    },
                    LogicalOperatorType::EsmallerThan => {
                        if e <= o {
                            return Ok(OranValue::Boolean(true));
                        }
                        Ok(OranValue::Boolean(false))
                    },
                }
            }
        }
        AstNode::IF(if_conditions, body, else_if_bodies_conditions, else_bodies) => {
            // if
            let condition_result = interp_expr(scope, env, if_conditions)?;
            if bool::from(condition_result) {
                let mut returned_val =  OranValue::Null;
                for astnode in body {
                    let returned_val_result = interp_expr(scope, env, &astnode.0);
                    match returned_val_result {
                        Ok(v) => {
                            match v {
                                OranValue::Null => { Ok(())},
                                _ => { 
                                    returned_val = v;
                                    break; 
                                }
                            }
                        },
                        Err(mut err) => {
                            if err.pair == None {
                                err.pair = Some(&astnode.1);
                            }
                            Err(err)
                        }
                    }?;
                }
                return Ok(returned_val);
            }
            // else if
            let mut _is_all_false = true;
            if !else_if_bodies_conditions.is_empty() {
                for (conditions, else_if_body) in else_if_bodies_conditions {
                    for c in conditions {
                        let result = interp_expr(scope, env, &c)?;
                        if bool::from(result) {
                            _is_all_false = false;
                            let mut returned_val =  OranValue::Null;
                            for astnode in else_if_body {
                                let returned_val_result = interp_expr(scope, env, &astnode.0);
                                match returned_val_result {
                                    Ok(v) => {
                                        match v {
                                            OranValue::Null => { Ok(())},
                                            _ => { 
                                                returned_val = v;
                                                break; 
                                            }
                                        }
                                    },
                                    Err(mut err) => {
                                        if err.pair == None {
                                            err.pair = Some(&astnode.1);
                                        }
                                        Err(err)
                                    }
                                }?;
                            }
                            return Ok(returned_val);
                        }
                    }
                    
                }
                if _is_all_false == false {
                    return Ok(OranValue::Null);
                }
            }
            // else
            if !else_bodies.is_empty() {
                let mut returned_val =  OranValue::Null;
                for astnode in else_bodies {
                    let returned_val_result = interp_expr(scope, env, &astnode.0);
                    match returned_val_result {
                        Ok(v) => {
                            match v {
                                OranValue::Null => { Ok(())},
                                _ => { 
                                    returned_val = v;
                                    break; 
                                }
                            }
                        },
                        Err(mut err) => {
                            if err.pair == None {
                                err.pair = Some(&astnode.1);
                            }
                            Err(err)
                        }
                    }?;
                    
                }
                return Ok(returned_val);
            }
            Ok(OranValue::Null)
        }
        AstNode::Bool (b) => {
            Ok(OranValue::Boolean(*b))
        }
        AstNode::ForLoop(is_inclusive, var_type, i, first, last, stmts) => {
            let first = interp_expr(scope, env, first)?;
            let first = f64::from(first).round() as i64;
            let last = interp_expr(scope, env, last)?;
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
                        for stmt in stmts {
                            let returned_val_result = interp_expr(this_for_loop_scope, env, &stmt.0);
                            match returned_val_result {
                                Ok(v) => {
                                    match v {
                                        OranValue::Null => { Ok(())},
                                        _ => { 
                                            return Ok(v)
                                        }
                                    }
                                },
                                Err(mut err) => {
                                    if err.pair == None {
                                        err.pair = Some(&stmt.1);
                                    }
                                    Err(err)
                                }
                            }?;
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
                        for stmt in stmts {
                            let returned_val_result = interp_expr(this_for_loop_scope, env, &stmt.0);
                            match returned_val_result {
                                Ok(v) => {
                                    match v {
                                        OranValue::Null => { Ok(())},
                                        _ => { 
                                            return Ok(v)
                                        }
                                    }
                                },
                                Err(mut err) => {
                                    if err.pair == None {
                                        err.pair = Some(&stmt.1);
                                    }
                                    Err(err)
                                }
                            }?;
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
            Ok(OranValue::Null)
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
                    Err(OranError{
                        message: format!("{}{}{}", "The variable \"".to_owned(), array, "\" is not defined."),
                        pair: None
                    })
                },
                Some(oran_val) => {
                    match oran_val {
                        OranValue::Variable(v) => {
                            let array = match OranValue::from(v.value) {
                                OranValue::Array(a) => Ok(a),
                                _ => {
                                    Err(OranError{
                                        message: "This variable isn't array.".to_owned(),
                                        pair: None
                                    })
                                }
                            };
                            array
                        }
                        _ => {
                            Err(OranError{
                                message: "This variable isn't array.".to_owned(),
                                pair: None
                            })
                        }
                    }
                }
            }?;
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
                    returned_val = interp_expr(this_for_loop_scope, env, stmt)?;
                    match returned_val {
                        OranValue::Null => {},
                        _ => { return Ok(returned_val) }
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
            Ok(OranValue::Null)
        },
        AstNode::ForLoopArray(var_type, i, array, stmts) => {
            let i_name = OranString::from(i);
            let this_for_loop_scope = scope + 1;
            for array_v in array {
                let val = interp_expr(scope, env, array_v)?;
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
                    returned_val = interp_expr(this_for_loop_scope, env, stmt)?;
                    match returned_val {
                        OranValue::Null => {},
                        _ => { return Ok(returned_val) }
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
            Ok(OranValue::Null)
        },
        AstNode::Array(array) => {
            let mut oran_vals: Vec<OranValue> = vec![];
            for arry_element in array.iter() {
                oran_vals.push(interp_expr(scope, env, arry_element)?);
            }
            Ok(OranValue::Array(oran_vals))
        },
        AstNode::ArrayElement(array_ast, indexes) => {
            let mut last_val: Option<Result<OranValue, OranError>> = None;
            for index in indexes.iter() {
                let usize_index = f64::from(interp_expr(scope, env, index)?) as usize;
                if last_val == None {
                    last_val = Some(interp_expr(scope, env, array_ast));
                }
                let array = last_val.unwrap()?;
                last_val = Some(match array {
                    OranValue::Array(a) => {
                        match a.into_iter().nth(usize_index) {
                            Some(val) => Ok(val),
                            None => Err(OranError{
                                message: "This index doesn't exist in the specified array.".to_owned(),
                                pair: None
                            })
                        }
                    },
                    OranValue::Variable(v) => {
                        let usize_index = f64::from(interp_expr(scope, env, index)?) as usize;
                        let array = match OranValue::from(v.value) {
                            OranValue::Array(a) => Ok(a),
                            _ => Err(OranError{
                                message: "This variable isn't array.".to_owned(),
                                pair: None
                            })
                        }?;
                        let elem = array.into_iter().nth(usize_index);
                        match elem {
                            Some(val) => Ok(val),
                            None => Err(OranError{
                                message: "This index doesn't exist in the specified array.".to_owned(),
                                pair: None
                            })
                        }
                    }
                    _ => {
                        Err(OranError{
                            message: format!("{}{}{}", "\"".to_owned(), &String::from(array), "\" is not array but index was specified.".to_owned()),
                            pair: None
                        })
                    }
                });
            }
            last_val.unwrap()
        }
        AstNode::Null => Ok(OranValue::Null),
        //_ => unreachable!("{:?}", reduced_expr)
    }
}

fn add<'a, 'b>(a: OranValue<'a>, other: OranValue<'a>) -> Result<OranValue<'a>, OranError<'a>> {
    match a {
        OranValue::Float(ref fl) => { Ok(OranValue::Float(fl + f64::from(other))) },
        OranValue::Str(ref s) => {
            let num = s.val_str.as_ref().parse::<f64>().or_else(|_|
                Err(OranError{
                    message: format!("{}{}{}", "This value's type is not Number: \"".to_string(), 
                                String::from(a).to_string(),
                                "\"".to_owned()
                            ),
                    pair: None
                })
            )?;
            Ok(OranValue::Float(
                num + f64::from(other)
            ))
        },
        OranValue::Variable(ref v) => { 
            match v.value {
                OranVariableValue::Float(ref vfl) => { Ok(OranValue::Float(vfl + f64::from(other))) },
                OranVariableValue::Str(ref s) => {
                    Ok(OranValue::Float(s.val_str.as_ref().parse::<f64>().or_else(|_|{
                        Err(OranError{
                            message: format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned()),
                            pair: None
                        })
                    })? + f64::from(other)))
                },
                _ => {
                    Err(OranError{
                        message: format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned()),
                        pair: None
                    })
                }
            }
        },
        _ => {
            Err(OranError{
                message: format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned()),
                pair: None
            })
        }
    }
}

fn sub<'a, 'b>(a: OranValue<'a>, other: OranValue<'a>) -> Result<OranValue<'a>, OranError<'a>> {
    match a {
        OranValue::Float(ref fl) => { Ok(OranValue::Float(fl - f64::from(other))) },
        OranValue::Str(ref s) => Ok(OranValue::Float(s.val_str.as_ref().parse::<f64>().or_else(|_|
            {
                Err(OranError{
                    message: format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned()),
                    pair: None
                })
            }
        )? - f64::from(other))),
        OranValue::Variable(ref v) => { 
            match v.value {
                OranVariableValue::Float(ref vfl) => { Ok(OranValue::Float(vfl - f64::from(other))) },
                OranVariableValue::Str(ref s) => {
                    Ok(OranValue::Float(s.val_str.as_ref().parse::<f64>().or_else(|_| {
                        Err(OranError{
                            message: format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned()),
                            pair: None
                        })
                    })? - f64::from(other)))
                },
                _ => {
                    Err(OranError{
                        message: format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned()),
                        pair: None
                    })
                }
            }
        },
        _ => {
            Err(OranError{
                message: format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned()),
                pair: None
            })
        }
    }
}

fn mul<'a, 'b>(a: OranValue<'a>, other: OranValue<'a>) -> Result<OranValue<'a>, OranError<'a>> {
    match a {
        OranValue::Float(ref fl) => { Ok(OranValue::Float(fl * f64::from(other))) },
        OranValue::Str(ref s) => Ok(OranValue::Float(s.val_str.as_ref().parse::<f64>().or_else(|_|
            {
                Err(OranError{
                    message: format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned()),
                    pair: None
                })
            }
        )? * f64::from(other))),
        OranValue::Variable(ref v) => { 
            match v.value {
                OranVariableValue::Float(ref vfl) => { Ok(OranValue::Float(vfl * f64::from(other))) },
                OranVariableValue::Str(ref s) => {
                    Ok(OranValue::Float(s.val_str.as_ref().parse::<f64>().or_else(|_|{
                        Err(OranError{
                            message: format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned()),
                            pair: None
                        })
                    })? * f64::from(other)))
                },
                _ => {
                    Err(OranError{
                        message: format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned()),
                        pair: None
                    })
                }
            }
        },
        _ => {
            Err(OranError{
                message: format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned()),
                pair: None
            })
        }
    }
}

fn div<'a, 'b>(a: OranValue<'a>, other: OranValue<'a>) -> Result<OranValue<'a>, OranError<'a>> {
    match a {
        OranValue::Float(ref fl) => { Ok(OranValue::Float(fl / f64::from(other))) },
        OranValue::Str(ref s) => Ok(OranValue::Float(s.val_str.as_ref().parse::<f64>().or_else(|_|
            {
                Err(OranError{
                    message: format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned()),
                    pair: None
                })
            }
        )? / f64::from(other))),
        OranValue::Variable(ref v) => { 
            match v.value {
                OranVariableValue::Float(ref vfl) => { Ok(OranValue::Float(vfl / f64::from(other))) },
                OranVariableValue::Str(ref s) => {
                    Ok(OranValue::Float(s.val_str.as_ref().parse::<f64>().or_else(|_|{
                        Err(OranError{
                            message: format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned()),
                            pair: None
                        })
                    })? / f64::from(other)))
                },
                _ => {
                    Err(OranError{
                        message: format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned()),
                        pair: None
                    })
                }
            }
        },
        _ => {
            Err(OranError{
                message: format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned()),
                pair: None
            })
        }
    }
}

fn rem<'a, 'b>(a: OranValue<'a>, other: OranValue<'a>) -> Result<OranValue<'a>, OranError<'a>> {
    match a {
        OranValue::Float(ref fl) => { Ok(OranValue::Float(fl % f64::from(other))) },
        OranValue::Str(ref s) => Ok(OranValue::Float(s.val_str.as_ref().parse::<f64>().or_else(|_|
            Err(OranError{
                message: format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned()),
                pair: None
            })
        )? % f64::from(other))),
        OranValue::Variable(ref v) => { 
            match v.value {
                OranVariableValue::Float(ref vfl) => { Ok(OranValue::Float(vfl % f64::from(other))) },
                OranVariableValue::Str(ref s) => {
                    Ok(OranValue::Float(s.val_str.as_ref().parse::<f64>().or_else(|_|{
                        Err(OranError{
                            message: format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned()),
                            pair: None
                        })
                    })? % f64::from(other)))
                },
                _ => {
                    Err(OranError{
                        message: format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned()),
                        pair: None
                    })
                }
            }
        },
        _ => {
            Err(OranError{
                message: format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned()),
                pair: None
            })
        }
    }
}

fn pow<'a, 'b>(a: OranValue<'a>, other: OranValue<'a>) -> Result<OranValue<'a>, OranError<'a>> {
    match a {
        OranValue::Float(ref fl) => { Ok(OranValue::Float(fl.powf(f64::from(other)))) },
        OranValue::Str(ref s) => Ok(OranValue::Float(s.val_str.as_ref().parse::<f64>().or_else(|_|
            Err(OranError{
                message: format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned()),
                pair: None
            })
        )?.powf(f64::from(other)))),
        OranValue::Variable(ref v) => { 
            match v.value {
                OranVariableValue::Float(ref vfl) => { Ok(OranValue::Float(vfl.powf(f64::from(other)))) },
                OranVariableValue::Str(ref s) => {
                    Ok(OranValue::Float(s.val_str.as_ref().parse::<f64>().or_else(|_|{
                        Err(OranError{
                            message: format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned()),
                            pair: None
                        })
                    })?.powf(f64::from(other))))
                },
                _ => {
                    Err(OranError{
                        message: format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned()),
                        pair: None
                    })
                }
            }
        },
        _ => {
            Err(OranError{
                message: format!("{}{}{}", "This value's type is not Number: \"".to_string(), String::from(a).to_string(), "\"".to_owned()),
                pair: None
            })
        }
    }
}

