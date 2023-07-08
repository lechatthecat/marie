use crate::treewalk_interpreter::Interpreter;
use super::values::Value;
use std::fmt;

#[derive(Clone)]
pub struct NativeFunction {
    pub name: String,
    pub arguments: u8,
    pub callable: fn(&mut Interpreter, &[Value]) -> Result<Value, String>,
}

impl fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "NativeFunction({})", self.name)
    }
}

impl Callable for NativeFunction {
    fn arguments(&self, _interpreter: &Interpreter) -> u8 {
        self.arguments
    }
    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, String> {
        (self.callable)(interpreter, args)
    }
}

pub trait Callable {
    fn arguments(&self, interpreter: &Interpreter) -> u8;
    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, String>;
}