use crate::treewalk_transpiler::Transpiler;
use super::values::Value;
use std::fmt;

#[derive(Clone)]
pub struct NativeFunction {
    pub name: String,
    pub arguments: u8,
    pub callable: fn(&mut Transpiler, &[Value]) -> Result<Value, String>,
}

impl fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "NativeFunction({})", self.name)
    }
}

impl Callable for NativeFunction {
    fn arguments(&self, _interpreter: &Transpiler) -> u8 {
        self.arguments
    }
    fn call(&self, interpreter: &mut Transpiler, args: &[Value], file_name: &str) -> Result<(String, Value), String> {
        Ok(("".to_owned(), (self.callable)(interpreter, args)?))
    }
}

pub trait Callable {
    fn arguments(&self, interpreter: &Transpiler) -> u8;
    fn call(&self, interpreter: &mut Transpiler, args: &[Value], file_name: &str) -> Result<(String, Value), String>;
}