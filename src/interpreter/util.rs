use std::collections::HashMap;
use std::process;
use colored::*;
use crate::value::{oran_string::OranString, oran_value::OranValue, oran_variable::OranVariable, var_type::{FunctionOrValueType, VarType}};

pub fn is_mutable<'a> (
    location: (usize, usize),
    scope: usize, 
    env : &HashMap<(usize, FunctionOrValueType, OranString<'a>), OranValue<'a>>,
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
                println!("{} Line number: {}, column number:{}: You can't assign value twice to a constant variable.", "Error!".red().bold(), location.0, location.1);
                process::exit(1);
            }
        },
        None => {
            if *variable_type == VarType::VariableReAssigned {
                println!("{} Line number: {}, column number:{}: You can't assign value without \"let\".", "Error!".red().bold(), location.0, location.1);
                process::exit(1);
            }
        }
    }
    true
}
