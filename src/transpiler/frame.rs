use std::{cell::RefCell, rc::Rc};

use crate::value::{values::Value, functions::Function, environment::Environment};


#[derive(Debug, Clone)]
pub struct CallFrame {
    pub closure: Closure,
    pub ip: usize,
}

#[derive(Debug, Clone)]
pub struct Closure {
    pub env: Environment,
}
