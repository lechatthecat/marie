use crate::bytecode::{bytecode::ValueMeta, bytecode_interpreter::{Interpreter, InterpreterError}, values::{value::{self, Type}, Binop}, StepResult};

pub fn op_add(vm: &mut Interpreter, _: u32, lineno: u32) -> StepResult<(), InterpreterError> {
    let val1 = vm.peek_by(0).clone();
    let val2 = vm.peek_by(1).clone();

    match (&val1, &val2) {
        (value::Value::Number(_), value::Value::Number(_)) |
        (value::Value::String(_), value::Value::Number(_)) |
        (value::Value::Number(_), value::Value::String(_)) |
        (value::Value::String(_), value::Value::String(_)) => {
            return match vm.numeric_binop(Binop::Add, lineno) {
                Ok(()) => StepResult::Ok(()),
                Err(e) => StepResult::Err(e),
            };
        }
        (value::Value::List(id1), value::Value::List(id2)) => {
            vm.pop_stack_and_stackmeta();
            vm.pop_stack_and_stackmeta();

            let mut res = vm.get_list_elements(*id2).clone();
            res.extend(vm.get_list_elements(*id1).clone());
            vm.stack
                .push(value::Value::List(vm.heap.manage_list(res)));
            vm.stack_meta.push(ValueMeta {
                is_public: true,
                is_mutable: true,
                value_type: Type::List
            });
            StepResult::Ok(())
        }
        _ => {
            return StepResult::Err(InterpreterError::Runtime(format!(
                "invalid operands of type {} and {} in add expression: \
                        both operands must be number or list (line={})",
                value::type_of(&val1),
                value::type_of(&val2),
                lineno
            )))
        }
    }
}

pub fn op_subtract(vm: &mut Interpreter, _: u32, lineno: u32) -> StepResult<(), InterpreterError> {
    let val1 = vm.peek_by(0).clone();
    let val2 = vm.peek_by(1).clone();

    match (&val1, &val2) {
        (value::Value::Number(_), value::Value::Number(_)) |
        (value::Value::String(_), value::Value::Number(_)) |
        (value::Value::Number(_), value::Value::String(_)) |
        (value::Value::String(_), value::Value::String(_)) => {
            return match vm.numeric_binop(Binop::Sub, lineno) {
                Ok(()) => StepResult::Ok(()),
                Err(e) => StepResult::Err(e),
            };
        }
        _ => {
            return StepResult::Err(InterpreterError::Runtime(format!(
                "invalid operands of type {} and {} in subtract expression: \
                        both operands must be number (line={})",
                value::type_of(&val1),
                value::type_of(&val2),
                lineno
            )))
        }
    }
}

pub fn op_multiply(vm: &mut Interpreter, _: u32, lineno: u32) -> StepResult<(), InterpreterError> {
    let val1 = vm.peek_by(0).clone();
    let val2 = vm.peek_by(1).clone();

    match (&val1, &val2) {
        (value::Value::Number(_), value::Value::Number(_)) |
        (value::Value::String(_), value::Value::Number(_)) |
        (value::Value::Number(_), value::Value::String(_)) |
        (value::Value::String(_), value::Value::String(_)) => {
            return match vm.numeric_binop(Binop::Mul, lineno) {
                Ok(()) => StepResult::Ok(()),
                Err(e) => StepResult::Err(e),
            };
        }
        _ => {
            return StepResult::Err(InterpreterError::Runtime(format!(
                "invalid operands of type {} and {} in multiply expression: \
                        both operands must be number (line={})",
                value::type_of(&val1),
                value::type_of(&val2),
                lineno
            )))
        }
    }
}

pub fn op_power(vm: &mut Interpreter, _: u32, lineno: u32) -> StepResult<(), InterpreterError> {
    let val1 = vm.peek_by(0).clone();
    let val2 = vm.peek_by(1).clone();

    match (&val1, &val2) {
        (value::Value::Number(_), value::Value::Number(_)) |
        (value::Value::String(_), value::Value::Number(_)) |
        (value::Value::Number(_), value::Value::String(_)) |
        (value::Value::String(_), value::Value::String(_)) => {
            return match vm.numeric_binop(Binop::Pow, lineno) {
                Ok(()) => StepResult::Ok(()),
                Err(e) => StepResult::Err(e),
            };
        }
        _ => {
            return StepResult::Err(InterpreterError::Runtime(format!(
                "invalid operands of type {} and {} in power expression: \
                        both operands must be number (line={})",
                value::type_of(&val1),
                value::type_of(&val2),
                lineno
            )))
        }
    }
}

pub fn op_modulus(vm: &mut Interpreter, _: u32, lineno: u32) -> StepResult<(), InterpreterError> {
    let val1 = vm.peek_by(0).clone();
    let val2 = vm.peek_by(1).clone();

    match (&val1, &val2) {
        (value::Value::Number(_), value::Value::Number(_)) |
        (value::Value::String(_), value::Value::Number(_)) |
        (value::Value::Number(_), value::Value::String(_)) |
        (value::Value::String(_), value::Value::String(_)) => {
            return match vm.numeric_binop(Binop::Modulus, lineno) {
                Ok(()) => StepResult::Ok(()),
                Err(e) => StepResult::Err(e),
            };
        }
        _ => {
            return StepResult::Err(InterpreterError::Runtime(format!(
                "invalid operands of type {} and {} in modulus expression: \
                        both operands must be number (line={})",
                value::type_of(&val1),
                value::type_of(&val2),
                lineno
            )))
        }
    }
}

pub fn op_divide(vm: &mut Interpreter, _: u32, lineno: u32) -> StepResult<(), InterpreterError> {
    let val1 = vm.peek_by(0).clone();
    let val2 = vm.peek_by(1).clone();

    match (&val1, &val2) {
        (value::Value::Number(_), value::Value::Number(_)) |
        (value::Value::String(_), value::Value::Number(_)) |
        (value::Value::Number(_), value::Value::String(_)) |
        (value::Value::String(_), value::Value::String(_)) => {
            return match vm.numeric_binop(Binop::Div, lineno) {
                Ok(()) => StepResult::Ok(()),
                Err(e) => StepResult::Err(e),
            };
        }
        _ => {
            return StepResult::Err(InterpreterError::Runtime(format!(
                "invalid operands of type {} and {} in divide expression: \
                        both operands must be number (line={})",
                value::type_of(&val1),
                value::type_of(&val2),
                lineno
            )))
        }
    }
}


