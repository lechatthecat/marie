use std::fmt::Display;

use super::{native_function::NativeFunction, expr, functions::Type};

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Nil,
    NativeFunction(NativeFunction),
    Function(
        expr::Symbol,
        /*id*/ u64,
        /*this binding*/ Option<Box<Value>>,
        Type,
    ),
    Class(expr::Symbol, /*id*/ u64),
    Instance(expr::Symbol, /*id*/ u64),
    Variable(expr::Symbol, /*id*/ u64),
    List(/*id*/ u64),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(val) => write!(f, "{}", val),
            Value::Float(val) => write!(f, "{}", val),
            Value::Bool(val) => write!(f, "{}", val),
            Value::String(val) => write!(f, "{}", val),
            Value::Nil => write!(f, "None"),
            _ => write!(f, ""),
        }
    }
}
