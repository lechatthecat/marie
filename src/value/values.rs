use std::fmt::Display;

use super::{native_function::NativeFunction, expr, functions::Type};

#[derive(Debug, Clone)]
pub enum Value {
    Integer,
    Float,
    String,
    Bool,
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
