use crate::parser::astnode::{AstNode, CalcOp, Function};
use crate::value::oran_value::{OranValue, FunctionDefine};
use crate::value::oran_variable::{OranVariable, OranVariableValue};
use crate::value::oran_string::OranString;
use crate::value::constant::{VARTYPE_CONSTANT, VARTYPE_REASSIGNED, SCOPE_FUNCTION};
use std::collections::HashMap;

pub fn interp_expr<'a>(scope: usize, env : &mut HashMap<(usize, OranString<'a>), OranValue<'a>>, reduced_expr: &'a AstNode) -> OranValue<'a> {
    match reduced_expr {
        AstNode::Number(ref double) => OranValue::Float(*double),
        AstNode::Calc (ref verb, ref lhs, ref rhs ) => {
            match verb {
                CalcOp::Plus => { interp_expr(scope, env, lhs) + interp_expr(scope, env, rhs) }
                CalcOp::Minus => { interp_expr(scope, env, lhs) - interp_expr(scope, env, rhs) }
                CalcOp::Times => { interp_expr(scope, env, lhs) * interp_expr(scope, env, rhs) }
                CalcOp::Divide => { interp_expr(scope, env, lhs) / interp_expr(scope, env, rhs) }
                CalcOp::Modulus => { interp_expr(scope, env, lhs) % interp_expr(scope, env, rhs) }
            }
        }
        AstNode::Ident(ref ident) => {
            let val = &*env.get(&(scope, OranString::from(ident))).unwrap_or_else(|| panic!("The variable \"{}\" is not defined.", ident));
            val.clone()
        }
        AstNode::Assign(ref var_type, ref ident, ref expr) => {
            if *var_type == VARTYPE_REASSIGNED {
                match env.get(&(scope, OranString::from(ident))).unwrap() {
                    OranValue::Variable(ref v) => { 
                        if v.var_type == VARTYPE_CONSTANT {
                            panic!("You can't assign value twice to a constant variable.");
                        }
                    }
                    _ => {}
                }
            }
            let oran_val = OranValue::Variable(OranVariable {
                var_type: *var_type,
                name: ident,
                value: OranVariableValue::from(&interp_expr(scope, env, expr)),
            });
            env.insert((scope, OranString::from(ident)), oran_val.clone());
            oran_val
        }
        AstNode::FunctionCall(ref func_ast, ref name, ref arg_values) => {
            match func_ast {
                Function::Print => {
                    let mut text = "".to_string();
                    for str in arg_values {
                        text.push_str(&String::from(interp_expr(scope, env, &str)))
                    }
                    print!("{}", text);
                    OranValue::Boolean(true)
                },
                Function::Println => {
                    let mut text = "".to_string();
                    for str in arg_values {
                        text.push_str(&String::from(interp_expr(scope, env, &str)))
                    }
                    println!("{}", text);
                    OranValue::Boolean(true)
                },
                Function::NotDefault => {
                    let func = *&env.get(&(SCOPE_FUNCTION, OranString::from(name))).unwrap();
                    let func = FunctionDefine::from(func);
                    for i in 0..func.args.len() {
                        let arg_name = interp_expr(scope+1, env, func.args.into_iter().nth(i).unwrap());
                        let arg_ast = arg_values.into_iter().nth(i).unwrap();
                        let val = interp_expr(scope+1, env, arg_ast);
                        env.insert((scope+1, OranString::from(arg_name)), val);
                    }
                    for body in func.body {
                        interp_expr(scope+1, env, &body);
                    }
                    let val = interp_expr(scope+1, env, func.fn_return);
                    // delete unnecessary data when exiting a scope
                    env.retain(|(s, _label), _val| *s != scope+1);
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
            env.insert((SCOPE_FUNCTION, OranString::from(func_name)), val.clone());
            val
        }
        AstNode::Argument(ref argument_name, ref val) => {
            let val = interp_expr(scope, env, val);
            env.insert((scope, OranString::from(argument_name)), val);
            OranValue::Str(OranString::from(argument_name))
        }
        AstNode::Str (ref str) => {
            OranValue::Str(OranString::from(str))
        }
        AstNode::Strs (strs) => {
            let mut text = "".to_string();
            for str in strs {
                text.push_str(&String::from(interp_expr(scope, env, &str)))
            }
            OranValue::Str(OranString {
                is_ref: false,
                ref_str: None,
                val_str: Some(text)
            })
        }
        AstNode::Bool (b) => {
            OranValue::Boolean(*b)
        }
        AstNode::Comparison (ref e, ref c, ref o) => {
            let e = interp_expr(scope, env, e);
            let o = interp_expr(scope, env, o);
            match c {
                0 => {
                    if e == o {
                        return OranValue::Boolean(true);
                    }
                    OranValue::Boolean(false)
                },
                1 => {
                    if e > o {
                        return OranValue::Boolean(true);
                    }
                    OranValue::Boolean(false)
                },
                2 => {
                    if e < o {
                        return OranValue::Boolean(true);
                    }
                    OranValue::Boolean(false)
                },
                3 => {
                    if e >= o {
                        return OranValue::Boolean(true);
                    }
                    OranValue::Boolean(false)
                },
                4 => {
                    if e <= o {
                        return OranValue::Boolean(true);
                    }
                    OranValue::Boolean(false)
                },
                _ => { OranValue::Boolean(false) }
            }
        }
        AstNode::IF(condtions, ref body, else_if_conditions, ref else_if_bodies, ref else_bodies) => {
            let mut if_condition_results = Vec::new();
            let mut else_if_condition_results = Vec::new();
            let if_condition;
            let else_if_condition;
            for condition in condtions {
                if_condition_results.push(interp_expr(scope, env, &condition));
            }
            if_condition = if_condition_results.into_iter().all(|x| bool::from(x) );
            if if_condition {
                for astnode in body {
                    interp_expr(scope+1, env, &astnode);
                }
                env.retain(|(s, _label), _val| *s != scope+1);
                return OranValue::Null;
            }
            if !else_if_conditions.is_empty() {
                for i in 0..else_if_conditions.len() {
                let conditions = else_if_conditions.into_iter().nth(i).unwrap();
                    let mut condition_results = Vec::new();
                    for condition in conditions {
                        condition_results.push(interp_expr(scope, env, condition));
                    }
                    let result = condition_results.into_iter().all(|x| bool::from(x) );
                    else_if_condition_results.push(result);
                    if result {
                        for astnode in else_if_bodies.into_iter().nth(i).unwrap() {
                            interp_expr(scope+1, env, &astnode);
                        }
                        env.retain(|(s, _label), _val| *s != scope+1);
                        return OranValue::Null;
                    }
                }        
            }
            else_if_condition = else_if_condition_results.into_iter().all(|x| !bool::from(x) );
            if !if_condition && else_if_condition {
                for astnode in else_bodies {
                    interp_expr(scope+1, env, astnode);
                }
                env.retain(|(s, _label), _val| *s != scope+1);
            } 
            OranValue::Null
        }
        AstNode::Null => OranValue::Null
    }
}
