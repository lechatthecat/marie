use std::time::{SystemTime, UNIX_EPOCH};

use crate::bytecode::bytecode_interpreter;
use crate::value;

use super::bytecode::ValueMeta;
use super::StepResult;

/*
Arity checking is done in the interpreter prior to calling a builtin function.
*/

pub fn exp(
    _interp: &mut bytecode_interpreter::Interpreter,
    args: &[value::Value],
) -> Result<value::Value, String> {
    match args[0] {
        value::Value::Number(num) => Ok(value::Value::Number(num.exp())),
        _ => Err(format!(
            "Invalid call: expected number, got {:?}.",
            value::type_of(&args[0])
        )),
    }
}

pub fn int_pow(
    _interp: &mut bytecode_interpreter::Interpreter,
    args: &[value::Value],
) -> Result<value::Value, String> {
    match (args[0].clone(), args[1].clone()) {
        (value::Value::Number(num1), value::Value::Number(num2)) 
            => Ok(value::Value::Number(i64::pow(num1 as i64, num2 as u32) as f64)),
        _ => Err(format!(
            "Invalid call: expected number, got {:?}.",
            value::type_of(&args[0])
        )),
    }
}

pub fn sqrt(
    _interp: &mut bytecode_interpreter::Interpreter,
    args: &[value::Value],
) -> Result<value::Value, String> {
    match args[0] {
        value::Value::Number(num) => Ok(value::Value::Number(num.sqrt())),
        _ => Err(format!(
            "Invalid call: expected number, got {:?}.",
            value::type_of(&args[0])
        )),
    }
}

pub fn clock(
    _interp: &mut bytecode_interpreter::Interpreter,
    _args: &[value::Value],
) -> Result<value::Value, String> {
    let start = SystemTime::now();
    let since_the_epoch = start.duration_since(UNIX_EPOCH).unwrap();

    Ok(value::Value::Number(since_the_epoch.as_millis() as f64))
}

pub fn len(
    interp: &mut bytecode_interpreter::Interpreter,
    args: &[value::Value],
) -> Result<value::Value, String> {
    match &args[0] {
        value::Value::String(id) => Ok(value::Value::Number(interp.heap.get_str(*id).len() as f64)),
        value::Value::List(id) => Ok(value::Value::Number(
            interp.heap.get_list_elements(*id).len() as f64,
        )),
        val => Err(format!(
            "Ojbect of type {:?} has no len.",
            value::type_of(val)
        )),
    }
}

pub fn for_each(
    interp: &mut bytecode_interpreter::Interpreter,
    args: &[value::Value],
) -> Result<value::Value, String> {
    match &args[0] {
        value::Value::List(id) => {
            let list_elements = interp.heap.get_list_elements(*id).clone();
            let callable = args[1].clone();
            for element in list_elements.iter() {
                interp.stack.push(callable.clone());
                interp.stack.push(element.clone());
                interp.stack_meta.push(ValueMeta { is_public: true, is_mutable: true, });
                interp.stack_meta.push(ValueMeta { is_public: true, is_mutable: true, });

                // stash the current frame number if we're going to call a pure function ...
                let frame_idx = interp.frames.len();

                if let Err(bytecode_interpreter::InterpreterError::Runtime(err)) =
                    interp.call_value(callable.clone(), 1)
                {
                    return Err(err);
                }

                // If we're calling a pure function, `interp.call_value` doesn't actually
                // call the value, it just sets up a call frame. We loop the interpreter
                // until it his an error or returns to the call frame with `frame_idx`.
                // Unfortunately, this doesn't play well with our current debugger
                // implementation, which manually calls `interpreter.step()`
                loop {
                    if interp.frames.len() == frame_idx {
                        break;
                    }

                    if let StepResult::Err(bytecode_interpreter::InterpreterError::Runtime(err)) = interp.step()
                    {
                        return Err(err);
                    }
                }
            }
            Ok(value::Value::Null)
        }
        val => Err(format!(
            "Can't call forEach on value of type {:?}.",
            value::type_of(val)
        )),
    }
}

pub fn map(
    interp: &mut bytecode_interpreter::Interpreter,
    args: &[value::Value],
) -> Result<value::Value, String> {
    match &args[1] {
        value::Value::List(id) => {
            let list_elements = interp.heap.get_list_elements(*id).clone();
            let callable = args[0].clone();
            let mut res_elements = Vec::new();
            for element in list_elements.iter() {
                interp.stack.push(callable.clone());
                interp.stack.push(element.clone());
                interp.stack_meta.push(ValueMeta { is_public: true, is_mutable: true, });
                interp.stack_meta.push(ValueMeta { is_public: true, is_mutable: true, });

                //stash the current frame number if we're going to call a pure function ...
                let frame_idx = interp.frames.len();

                if let Err(bytecode_interpreter::InterpreterError::Runtime(err)) =
                    interp.call_value(callable.clone(), 1)
                {
                    return Err(err);
                }

                // If we're calling a pure function, `interp.call_value` doesn't actually
                // call the value, it just sets up a call frame. We loop the interpreter
                // until it his an error or returns to the call frame with `frame_idx`.
                // Unfortunately, this doesn't play well with our current debugger
                // implementation, which manually calls `interpreter.step()`
                loop {
                    if interp.frames.len() == frame_idx {
                        break;
                    }

                    if let StepResult::Err(bytecode_interpreter::InterpreterError::Runtime(err)) = interp.step()
                    {
                        return Err(err);
                    }
                }

                res_elements.push(interp.pop_stack());
                interp.pop_stack_meta();
            }
            Ok(value::Value::List(interp.heap.manage_list(res_elements)))
        }
        val => Err(format!(
            "Can't call forEach on value of type {:?}.",
            value::type_of(val)
        )),
    }
}
