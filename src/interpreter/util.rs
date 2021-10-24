use std::collections::HashMap;
use std::process;
use colored::*;
use crate::value::{oran_scope::OranScope, oran_string::OranString, oran_value::OranValue, oran_variable::OranVariable, scope::ROOT_SCOPE, var_type::{FunctionOrValueType, VarType}};

pub fn is_mutable<'a> (
    location: &(String, usize, usize),
    scope: OranScope, 
    env : &HashMap<(OranScope, FunctionOrValueType, OranString<'a>), OranValue<'a>>,
    ident: &str,
    variable_type: &VarType) -> bool {

    let val = env.get(
        &(
            scope,
            FunctionOrValueType::Value,
            OranString::from(ident)
        )
    );
    match val {
        Some(v) => {
            if *variable_type == VarType::VariableReAssigned && OranVariable::from(v).var_type == VarType::Constant {
                println!("{}\n{}\nLine number: {}, column number:{}: You can't assign value twice to a constant variable.",
                    "Error!".red().bold(),    
                    location.0,    
                    location.1,
                    location.2
                );
                process::exit(1);
            }
        },
        None => {
            //If not found, then try to get function definition from higher levels.
            let mut check_scope = scope; 
            let val = loop {
                if check_scope == ROOT_SCOPE {
                    break None;
                }
                check_scope = check_scope - 1;
                let val = *&env.get(&(check_scope, FunctionOrValueType::Value, OranString::from(ident)));
                match val {
                    None => {
                        continue;
                    },
                    _ => {
                        break val;
                    }
                };
            };
            match val {
                Some(v) => {
                    if *variable_type == VarType::VariableReAssigned && OranVariable::from(v).var_type == VarType::Constant {
                        println!("{}\n{}\nLine number: {}, column number:{}: You can't assign value twice to a constant variable.",
                            "Error!".red().bold(),    
                            location.0,    
                            location.1,
                            location.2
                        );
                        process::exit(1);
                    }
                },
                None => {
                    if *variable_type == VarType::VariableReAssigned {
                        println!("{}\n{}\nLine number: {}, column number:{}: You can't assign value without \"let\".",
                            "Error!".red().bold(),    
                            location.0,    
                            location.1,
                            location.2
                        );
                        process::exit(1);
                    }
                }
            }
        }
    }
    true
}
