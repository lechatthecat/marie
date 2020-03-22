use super::{astnode, oran_value::OranValue, oran_variable::OranVariable, oran_variable::OranVariableValue, oran_string::OranString};
use super::constant::{VARTYPE_CONSTANT, VARTYPE_REASSIGNED, SCOPE_FUNCTION};
use super::astnode::{AstNode, CalcOp, Function};
use super::oran_value::FunctionDefine;
use std::collections::HashMap;

pub fn interp_expr<'a>(scope: usize, env : &mut HashMap<(usize, String), OranValue<'a>>, reduced_expr: &'a astnode::AstNode) -> OranValue<'a> {
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
            let val = &*env.get(&(scope, ident[..].to_string())).unwrap_or_else(|| panic!("The variable \"{}\" is not defined.", ident));
            val.clone()
        }
        AstNode::Assign(ref var_type, ref ident, ref expr) => {
            if *var_type == VARTYPE_REASSIGNED {
                match env.get(&(scope, ident[..].to_string())).unwrap() {
                    OranValue::Variable(ref v) => { 
                        if v.var_type == VARTYPE_CONSTANT {
                            panic!("You can't assign value twice to a constant variable.");
                        }
                    }
                    _ => {}
                }
            }
            let val = &interp_expr(scope, env, expr);
            let oran_val = OranValue::Variable(OranVariable {
                var_type: *var_type,
                name: ident,
                value: OranVariableValue::from(val.clone()),
            });
            env.insert((scope, ident.to_string()), oran_val.clone());
            oran_val
        }
        AstNode::FunctionCall(ref func_ast, ref name, ref args) => {
            match func_ast {
                Function::Print => {
                    let mut text = "".to_string();
                    for str in args {
                        text.push_str(&String::from(interp_expr(scope, env, &str)))
                    }
                    print!("{}", text);
                    OranValue::Boolean(true)
                },
                Function::Println => {
                    let mut text = "".to_string();
                    for str in args {
                        text.push_str(&String::from(interp_expr(scope, env, &str)))
                    }
                    println!("{}", text);
                    OranValue::Boolean(true)
                },
                Function::NotDefault => {
                    let func = env.get(&(SCOPE_FUNCTION, name.to_string())).unwrap();
                    let func = FunctionDefine::from(func);
                    for i in 0..func.args.len() {
                        let name = interp_expr(scope+1, env, func.args.into_iter().nth(i).unwrap());
                        let name = String::from(name);
                        let arg_ast = args.into_iter().nth(i).unwrap();
                        let val = interp_expr(scope+1, env, arg_ast);
                        env.insert((scope+1, name), val);
                    }
                    for body in func.body {
                        interp_expr(scope+1, env, &body);
                    }
                    interp_expr(scope+1, env, func.fn_return)
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
            env.insert((SCOPE_FUNCTION, func_name.to_string()), val.clone());
            val
        }
        AstNode::Argument(ref argument_name, ref val) => {
            let val = interp_expr(scope, env, val);
            env.insert((scope, argument_name.to_string()), val);
            OranValue::Str(OranString {
                is_ref: true,
                ref_str: Some(argument_name),
                val_str: None
            })
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
                text.push_str(&String::from(interp_expr(scope, env, &str)))
            }
            OranValue::Str(OranString {
                is_ref: false,
                ref_str: None,
                val_str: Some(text)
            })
        }
        AstNode::Null => OranValue::Null
    }
}
