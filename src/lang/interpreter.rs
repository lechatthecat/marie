use super::{astnode, oran_value::OranValue, oran_variable::OranVariable, oran_variable::OranVariableValue, oran_string::OranString};
use super::constant::{VARTYPE_CONSTANT, VARTYPE_REASSIGNED};
use std::collections::HashMap;

pub fn interp_expr<'a>(env : &mut HashMap<&'a str, OranValue<'a>>, reduced_expr: &'a astnode::AstNode) -> OranValue<'a> {
    use astnode::AstNode;
    use astnode::CalcOp;
    use astnode::DefaultFunction;

    match reduced_expr {
        AstNode::Number(ref double) => OranValue::Float(*double),
        AstNode::Ident(ref ident) => {
            let val = &*env.get(&ident[..]).unwrap_or_else(|| panic!("The variable \"{}\" is not defined.", ident));
            val.to_owned()
        }
        AstNode::Assign(ref var_type, ref ident, ref expr) => {
            if *var_type == VARTYPE_REASSIGNED {
                match env.get(&ident[..]).unwrap() {
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
                name: ident.to_owned(),
                value: OranVariableValue::from(val.to_owned()),
            });
            env.insert(ident, oran_val.clone());
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
        AstNode::FunctionCall(ref func, ref args) => {
            match func {
                DefaultFunction::Print => {
                    let mut text = "".to_string();
                    for str in args {
                        text.push_str(&String::from(interp_expr(env, &str)))
                    }
                    println!("{}", text);
                    OranValue::Boolean(true)
                },
            }
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