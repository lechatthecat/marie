use super::{astnode, oran_value::OranValue, oran_variable::OranVariable, oran_variable::OranVariableValue, oran_string::OranString};
use super::constant::{VARTYPE_CONSTANT, VARTYPE_REASSIGNED};
use super::astnode::{AstNode, CalcOp, Function};
use super::oran_value::FunctionDefine;
use std::collections::HashMap;

pub fn interp_expr<'a>(env : &mut HashMap<(i32, &'a str), OranValue<'a>>, reduced_expr: &'a astnode::AstNode) -> OranValue<'a> {
    match reduced_expr {
        AstNode::Number(ref double) => OranValue::Float(*double),
        AstNode::Ident(ref ident) => {
            let val = &*env.get(&(1, &ident[..])).unwrap_or_else(|| panic!("The variable \"{}\" is not defined.", ident));
            val.clone()
        }
        AstNode::Assign(ref var_type, ref ident, ref expr) => {
            if *var_type == VARTYPE_REASSIGNED {
                match env.get(&(1, &ident[..])).unwrap() {
                    OranValue::Variable(ref v) => { 
                        if v.var_type == VARTYPE_CONSTANT {
                            panic!("You can't assign value twice to a constant variable.");
                        }
                    }
                    _ => {}
                }
            }
            let val = &interp_expr(env, expr);
            let oran_val = OranValue::Variable(OranVariable {
                var_type: *var_type,
                name: ident.clone(),
                value: OranVariableValue::from(val.clone()),
            });
            env.insert((1, ident), oran_val.clone());
            oran_val
        }
        AstNode::Calc (ref verb, ref lhs, ref rhs ) => {
            match verb {
                CalcOp::Plus => { interp_expr(env, lhs) + interp_expr(env, rhs) }
                CalcOp::Minus => { interp_expr(env, lhs) - interp_expr(env, rhs) }
                CalcOp::Times => { interp_expr(env, lhs) * interp_expr(env, rhs) }
                CalcOp::Divide => { interp_expr(env, lhs) / interp_expr(env, rhs) }
                CalcOp::Modulus => { interp_expr(env, lhs) % interp_expr(env, rhs) }
            }
        }
        AstNode::FunctionCall(ref func_ast, ref name, ref args) => {
            match func_ast {
                Function::Print => {
                    let mut text = "".to_string();
                    for str in args {
                        text.push_str(&String::from(interp_expr(env, &str)))
                    }
                    print!("{}", text);
                    OranValue::Boolean(true)
                },
                Function::Println => {
                    let mut text = "".to_string();
                    for str in args {
                        text.push_str(&String::from(interp_expr(env, &str)))
                    }
                    println!("{}", text);
                    OranValue::Boolean(true)
                },
                _ => {
                    let func = env.get(&(0, name)).unwrap();
                    let func = FunctionDefine::from(func);
                    for body in func.body {
                        interp_expr(env, &body);
                    }
                    OranValue::Boolean(true)
                }
            }
        }
        AstNode::FunctionDefine(ref func_name, ref args, ref astnodes, ref fn_return) => {
            let val = OranValue::Function(FunctionDefine {
                name: func_name.clone(),
                args: args,
                body: astnodes,
                fn_return: fn_return
            });
            env.insert((0, func_name), val.clone());
            val
        }
        AstNode::Str (str) => {
            OranValue::Str(OranString {
                is_ref: true,
                ref_str: Some(str),
                val_str: None
            })
        }
        AstNode::Strs (strs) => {
            let mut text = "".to_string();
            for str in strs {
                text.push_str(&String::from(interp_expr(env, &str)))
            }
            OranValue::Str(OranString {
                is_ref: false,
                ref_str: None,
                val_str: Some(text)
            })
        }
        AstNode::Null => { OranValue::Null }
    }
}