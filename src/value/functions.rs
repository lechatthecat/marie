use std::collections::HashMap;

use crate::interpreter::treewalk_interpreter::Interpreter;

use super::{
    expr::{Symbol, Stmt, SourceLocation},
    environment::Environment, values::Value, native_function::Callable
};

#[derive(Clone, Debug)]
pub struct Function {
    pub id: u64,
    pub name: Symbol,
    pub parameters: Vec<Symbol>,
    pub body: Vec<Stmt>,
    pub closure: Environment,
    pub this_binding: Option<Box<Value>>,
    pub superclass: Option<u64>,
    pub is_initializer: bool,
}

impl Callable for Function {
    fn arguments(&self, _interpreter: &Interpreter) -> u8 {
        self.parameters.len().try_into().unwrap()
    }
    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, String> {
        let args_env: HashMap<_, _> = self
            .parameters
            .iter()
            .zip(args.iter())
            .map(|(param, arg)| {
                (
                    param.name.clone(),
                    (
                        Some(arg.clone()),
                        SourceLocation {
                            line: param.line,
                            col: param.col,
                        },
                    ),
                )
            })
            .collect();

        let saved_env = interpreter.env.clone();
        let saved_retval = interpreter.retval.clone();


        Ok(Value::Nil)
    }
}

#[derive(Clone, Debug)]
pub struct Class {
    pub name: Symbol,
    pub superclass: Option<u64>,
    pub id: u64,
    pub methods: HashMap<String, u64>,
}

impl Callable for Class {
    fn arguments(&self, interpreter: &Interpreter) -> u8 {
        match self.init(interpreter) {
            Some(initializer) => initializer.parameters.len().try_into().unwrap(),
            None => 0,
        }
    }
    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, String> {
        let instance = interpreter.create_instance(&self.name, self.id);

        if let Some(mut initializer) = self.init(interpreter) {
            initializer.this_binding = Some(Box::new(instance.clone()));
            initializer.call(interpreter, args)?;
        }

        Ok(instance)
    }
}

impl Class {
    fn init(&self, interpreter: &Interpreter) -> Option<Function> {
        self.methods
            .get(&String::from("init"))
            .map(|initializer_id| interpreter.get_function(*initializer_id).clone())
    }

    fn find_method(
        &self,
        method_name: &str,
        interpreter: &Interpreter,
    ) -> Option<(Symbol, u64)> {
        if let Some(method_id) = self.methods.get(method_name) {
            let lox_fn = interpreter.get_function(*method_id);
            return Some((lox_fn.name.clone(), *method_id));
        } else if let Some(superclass_id) = self.superclass {
            return interpreter
                .get_class(superclass_id)
                .find_method(method_name, interpreter);
        }
        None
    }
}

#[derive(Clone, Debug)]
pub struct Instance {
    pub class_name: Symbol,
    pub class_id: u64,
    pub id: u64,
    pub fields: HashMap<String, Value>,
}

impl Instance {
    fn getattr(&self, attr: &str, interpreter: &Interpreter) -> Result<Value, String> {
        match self.fields.get(attr) {
            Some(val) => Ok(val.clone()),
            None => {
                let cls = interpreter.get_class(self.class_id);
                if let Some((func_name, method_id)) = cls.find_method(attr, interpreter) {
                    return Ok(Value::Function(
                        func_name,
                        method_id,
                        Some(Box::new(Value::Instance(
                            self.class_name.clone(),
                            self.id,
                        ))),
                    ));
                }
                Err(format!(
                    "AttributeError: '{}' instance has no '{}' attribute.",
                    self.class_name.name, attr
                ))
            }
        }
    }
}

fn as_callable(interpreter: &Interpreter, value: &Value) -> Option<Box<dyn Callable>> {
    match value {
        Value::NativeFunction(f) => Some(Box::new(f.clone())),
        Value::Function(_, id, this_binding) => {
            let f = interpreter.get_function(*id);
            let mut f_copy = f.clone();
            f_copy.this_binding = this_binding.clone();
            Some(Box::new(f_copy))
        }
        Value::Class(_, id) => Some(Box::new(interpreter.get_class(*id).clone())),
        _ => None,
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Type {
    Number,
    String,
    Bool,
    Nil,
    NativeFunction,
    Function,
    Class,
    Instance,
    List,
}

pub fn type_of(val: &Value) -> Type {
    match val {
        Value::Number(_) => Type::Number,
        Value::String(_) => Type::String,
        Value::Bool(_) => Type::Bool,
        Value::Nil => Type::Nil,
        Value::NativeFunction(_) => Type::NativeFunction,
        Value::Function(_, _, _) => Type::Function,
        Value::Class(_, _) => Type::Class,
        Value::Instance(_, _) => Type::Instance,
        Value::List(_) => Type::List,
    }
}