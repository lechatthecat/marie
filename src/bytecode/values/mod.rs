pub mod value;

use crate::bytecode::{
    bytecode::{self, ValueMeta},
    bytecode_interpreter::{Interpreter, InterpreterError},
};

#[allow(dead_code)]
#[derive(Debug)]
pub enum Binop {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Modulus,
}

impl std::fmt::Display for Binop {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Binop::Add => write!(fmt, "Add"),
            Binop::Sub => write!(fmt, "Sub"),
            Binop::Mul => write!(fmt, "Mul"),
            Binop::Div => write!(fmt, "Div"),
            Binop::Pow => write!(fmt, "Pow"),
            Binop::Modulus => write!(fmt, "Modulus"),
        }
    }
}

impl Interpreter {
    pub fn is_falsey(&self, val: &value::Value) -> bool {
        match val {
            value::Value::Null => true,
            value::Value::Bool(b) => !*b,
            value::Value::Number(f) => *f == 0.0,
            value::Value::Function(_) => false,
            value::Value::NativeFunction(_) => false,
            value::Value::Class(_) => false,
            value::Value::Instance(_) => false,
            value::Value::BoundMethod(_) => false,
            value::Value::String(id) => self.get_str(*id).is_empty(),
            value::Value::List(id) => self.get_list_elements(*id).is_empty(),
        }
    }

    pub fn apply_numeric_binop(left: f64, right: f64, binop: Binop) -> f64 {
        match binop {
            Binop::Add => left + right,
            Binop::Sub => left - right,
            Binop::Mul => left * right,
            Binop::Div => left / right,
            Binop::Pow => f64::powf(left, right),
            Binop::Modulus => left.rem_euclid(right),
        }
    }

    pub fn print_val(&mut self, val: &value::Value) {
        let output = self.format_val(val);
        println!("{}", output);
        self.output.push(output);
    }

    pub fn values_equal(&self, val1: &value::Value, val2: &value::Value) -> bool {
        match (val1, val2) {
            (value::Value::Number(n1), value::Value::Number(n2)) => (n1 - n2).abs() < f64::EPSILON,
            (value::Value::Bool(b1), value::Value::Bool(b2)) => b1 == b2,
            (value::Value::String(s1), value::Value::String(s2)) => {
                self.get_str(*s1) == self.get_str(*s2)
            }
            (value::Value::Null, value::Value::Null) => true,
            (_, _) => false,
        }
    }

    pub fn numeric_binop(
        &mut self,
        binop: Binop,
        lineno: bytecode::Lineno,
    ) -> Result<(), InterpreterError> {
        let val1 = self.peek_by(0).clone();
        let val2 = self.peek_by(1).clone();

        match (&val1, &val2) {
            (value::Value::Number(n1), value::Value::Number(n2)) => {
                self.pop_stack_and_stackmeta();
                self.pop_stack_and_stackmeta();
                self.stack
                    .push(value::Value::Number(Interpreter::apply_numeric_binop(
                        *n2, *n1, binop, // note the order!
                    )));
                self.stack_meta.push(ValueMeta {
                    is_public: true,
                    is_mutable: true,
                });
                Ok(())
            }
            (value::Value::String(n1), value::Value::Number(n2)) => {
                self.pop_stack_and_stackmeta();
                self.pop_stack_and_stackmeta();
                let n1 = self.get_str(*n1).parse::<f64>();
                let num;
                match n1 {
                    Err(_) => {
                        return Err(InterpreterError::Runtime(format!(
                            "Expected numbers in {:?} expression. Found {:?} and {:?} (line={})",
                            binop,
                            value::type_of(&val1),
                            value::type_of(&val2),
                            lineno.value
                        )));
                    }
                    Ok(val) => {
                        num = val;
                    }
                }
                self.stack
                    .push(value::Value::Number(Interpreter::apply_numeric_binop(
                        *n2, num, binop, // note the order!
                    )));
                self.stack_meta.push(ValueMeta {
                    is_public: true,
                    is_mutable: true,
                });
                Ok(())
            }
            (value::Value::Number(n1), value::Value::String(n2)) => {
                self.pop_stack_and_stackmeta();
                self.pop_stack_and_stackmeta();
                let n2 = self.get_str(*n2).parse::<f64>();
                let num;
                match n2 {
                    Err(_) => {
                        return Err(InterpreterError::Runtime(format!(
                            "Expected numbers in {:?} expression. Found {:?} and {:?} (line={})",
                            binop,
                            value::type_of(&val1),
                            value::type_of(&val2),
                            lineno.value
                        )));
                    }
                    Ok(val) => {
                        num = val;
                    }
                }
                self.stack
                    .push(value::Value::Number(Interpreter::apply_numeric_binop(
                        num, *n1, binop, // note the order!
                    )));
                self.stack_meta.push(ValueMeta {
                    is_public: true,
                    is_mutable: true,
                });
                Ok(())
            }
            (value::Value::String(n1), value::Value::String(n2)) => {
                self.pop_stack_and_stackmeta();
                self.pop_stack_and_stackmeta();
                let n1 = self.get_str(*n1).parse::<f64>();
                let num1;
                match n1 {
                    Err(_) => {
                        return Err(InterpreterError::Runtime(format!(
                            "Expected numbers in {:?} expression. Found {:?} and {:?} (line={})",
                            binop,
                            value::type_of(&val1),
                            value::type_of(&val2),
                            lineno.value
                        )));
                    }
                    Ok(val) => {
                        num1 = val;
                    }
                }
                let n2 = self.get_str(*n2).parse::<f64>();
                let num2;
                match n2 {
                    Err(_) => {
                        return Err(InterpreterError::Runtime(format!(
                            "Expected numbers in {:?} expression. Found {:?} and {:?} (line={})",
                            binop,
                            value::type_of(&val1),
                            value::type_of(&val2),
                            lineno.value
                        )));
                    }
                    Ok(val) => {
                        num2 = val;
                    }
                }
                self.stack
                    .push(value::Value::Number(Interpreter::apply_numeric_binop(
                        num2, num1, binop, // note the order!
                    )));
                self.stack_meta.push(ValueMeta {
                    is_public: true,
                    is_mutable: true,
                });
                Ok(())
            }
            _ => Err(InterpreterError::Runtime(format!(
                "Expected numbers in {} expression. Found {} and {} (line={})",
                binop,
                value::type_of(&val1),
                value::type_of(&val2),
                lineno.value
            ))),
        }
    }
}
