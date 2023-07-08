use super::{native_function::NativeFunction, expr, functions::Type};

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
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
    List(/*id*/ u64),
}
