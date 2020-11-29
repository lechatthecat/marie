use std::{hash::BuildHasherDefault, collections::HashMap};

use crate::{hash::simple::SimpleHasher, value::{oran_string::OranString, oran_value::OranValue, oran_variable::OranVariable, var_type::{FunctionOrValueType, VarType}}};


pub fn is_mutable<'a> (
    scope: usize, 
    env : &HashMap<(usize, FunctionOrValueType, OranString<'a>), OranValue<'a>, BuildHasherDefault<SimpleHasher>>,
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
                panic!("You can't assign value twice to a constant variable.");
            }
        },
        None => {
            if *variable_type == VarType::VariableReAssigned {
                panic!("You can't assign value without 'let'.");
            }
        }
    }
    true
}
