use super::{native_function::NativeFunction, expr};

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
    ),
    Class(expr::Symbol, /*id*/ u64),
    Instance(expr::Symbol, /*id*/ u64),
    List(/*id*/ u64),
}
