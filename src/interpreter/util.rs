use std::collections::HashMap;
use std::process;
use colored::*;
use crate::value::{oran_scope::OranScope, oran_string::OranString, oran_value::OranValue, oran_variable::OranVariable, scope::ROOT_SCOPE, var_type::{FunctionOrValueType, VarType}};

pub fn is_mutable<'a> (
    location: &(String, usize, usize),
    scope: OranScope, 
    env : &mut HashMap<(OranScope, FunctionOrValueType, OranString<'a>), OranValue<'a>>,
    ident: &str,
    variable_type: &VarType) -> bool {

    let val = get_val(
        scope,
        env,
        ident,
        FunctionOrValueType::Value
    );
    match val.0 {
        Some(v) => {
            if *variable_type == VarType::VariableReAssigned 
                && OranVariable::from(&v).var_type == VarType::Constant {
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
    true
}

pub fn get_val<'a, 'b:'a>(
    scope: OranScope, 
    env : &mut HashMap<(
            OranScope,
            FunctionOrValueType,
            OranString<'b>
        ),
        OranValue<'b>
    >, 
    ident: &str,
    function_val_type: FunctionOrValueType
    ) -> (Option<OranValue<'a>>, OranScope) {

    //If not found, then try to get function definition from higher levels.
    let mut check_scope = scope; 
    let op_val = loop {
        let val = env.get(
            &(check_scope,
            function_val_type,
            OranString::from(ident))
        );
        match val {
            None => {
                if check_scope == ROOT_SCOPE {
                    break (None, check_scope);
                }
                check_scope = check_scope - 1;
                continue;
            },
            Some(v) => {
                break (Some(v.clone()), check_scope);
            }
        };
    };
    op_val
}
