use crate::bytecode::{bytecode::ValueMeta, bytecode_interpreter::{Interpreter, InterpreterError}, values::value, StepResult};

pub fn op_not(vm: &mut Interpreter, _: u32, lineno: u32) -> StepResult<(), InterpreterError> {
    let top_stack = &vm.peek();
    let maybe_bool = Interpreter::extract_bool(top_stack);

    match maybe_bool {
        Some(b) => {
            vm.pop_stack();
            vm.pop_stack_meta();
            vm.stack.push(value::Value::Bool(!b));
            vm.stack_meta.push(ValueMeta {
                is_public: true,
                is_mutable: true,
            });
        }
        None => {
            return StepResult::Err(InterpreterError::Runtime(format!(
            "invalid operand in not expression. Expected boolean, found {} at line {}",
            value::type_of(top_stack),
            lineno
        )))
        }
    }
    StepResult::Ok(())
}

pub fn op_equal(vm: &mut Interpreter, _: u32, _: u32) -> StepResult<(), InterpreterError> {
    let val1 = vm.pop_stack();
    let val2 = vm.pop_stack();
    vm.pop_stack_meta();
    vm.pop_stack_meta();
    vm.stack
        .push(value::Value::Bool(vm.values_equal(&val1, &val2)));
    vm.stack_meta.push(ValueMeta {
        is_public: true,
        is_mutable: true,
    });
    StepResult::Ok(())
}

pub fn op_true(vm: &mut Interpreter, _: u32, _: u32) -> StepResult<(), InterpreterError> {
    vm.stack.push(value::Value::Bool(true));
    vm.stack_meta.push(ValueMeta {
        is_public: true,
        is_mutable: true,
    });
    StepResult::Ok(())
}

pub fn op_false(vm: &mut Interpreter, _: u32, _: u32) -> StepResult<(), InterpreterError> {
    vm.stack.push(value::Value::Bool(false));
    vm.stack_meta.push(ValueMeta {
        is_public: true,
        is_mutable: true,
    });
    StepResult::Ok(())
}

pub fn op_negate(vm: &mut Interpreter, _: u32, lineno: u32) -> StepResult<(), InterpreterError> {
    let top_stack = &vm.peek();
    let maybe_number = Interpreter::extract_number(top_stack);

    match maybe_number {
        Some(to_negate) => {
            vm.pop_stack_and_stackmeta();
            vm.stack.push(value::Value::Number(-to_negate));
            vm.stack_meta.push(ValueMeta {
                is_public: true,
                is_mutable: true,
            });
        }
        None => {
            return StepResult::Err(InterpreterError::Runtime(format!(
            "invalid operand to unary op negate. Expected number, found {} at line {}",
            value::type_of(top_stack),
            lineno
        )))
        }
    }
    StepResult::Ok(())
}

pub fn op_greater(vm: &mut Interpreter, _: u32, lineno: u32) -> StepResult<(), InterpreterError> {
    let val1 = vm.peek_by(0).clone();
    let val2 = vm.peek_by(1).clone();

    match (&val1, &val2) {
            (value::Value::Number(n1), value::Value::Number(n2)) => {
                vm.pop_stack();
                vm.pop_stack();
                vm.pop_stack_meta();
                vm.pop_stack_meta();

                vm.stack.push(value::Value::Bool(n2 > n1));
                vm.stack_meta.push(ValueMeta { is_public: true, is_mutable: true, });
            }
            _ => return StepResult::Err(InterpreterError::Runtime(format!(
                "invalid operands in Greater expression. Expected numbers, found {} and {} at line {}",
                value::type_of(&val1), value::type_of(&val2), lineno)))

        }
    StepResult::Ok(())
}

pub fn op_less(vm: &mut Interpreter, _: u32, lineno: u32) -> StepResult<(), InterpreterError> {
    let val1 = vm.peek_by(0).clone();
    let val2 = vm.peek_by(1).clone();

    match (&val1, &val2) {
            (value::Value::Number(n1), value::Value::Number(n2)) => {
                vm.pop_stack();
                vm.pop_stack();
                vm.pop_stack_meta();
                vm.pop_stack_meta();
                vm.stack.push(value::Value::Bool(n2 < n1));
                vm.stack_meta.push(ValueMeta { is_public: true, is_mutable: true, });
            }
            _ => return StepResult::Err(InterpreterError::Runtime(format!(
                "invalid operands in Less expression. Expected numbers, found {} and {} at line {}",
                value::type_of(&val1), value::type_of(&val2), lineno)))

        }
    StepResult::Ok(())
}

pub fn op_jump_if_false(vm: &mut Interpreter, operand: u32, lineno: u32) -> StepResult<(), InterpreterError> {
    let offset = operand as usize;
    if vm.is_falsey(&vm.peek()) {
        vm.frame_mut().instruction_pointer += offset;
    }
    StepResult::Ok(())
}

pub fn op_jump(vm: &mut Interpreter, operand: u32, _: u32) -> StepResult<(), InterpreterError> {
    let offset = operand as usize;
    vm.frame_mut().instruction_pointer += offset;
    StepResult::Ok(())
}

