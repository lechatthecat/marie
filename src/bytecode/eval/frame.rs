use crate::bytecode::{bytecode::{self, ValueMeta}, bytecode_interpreter::{Interpreter, InterpreterError}, values::value::{self, Type}, StepResult};

pub fn op_constant(vm: &mut Interpreter, operand: u32, _: u32) -> StepResult<(), InterpreterError> {
    let idx = operand as usize;
    let constant = vm.read_constant(idx);
    vm.stack.push(constant);
    let meta = vm.read_constant_meta(idx);
    vm.stack_meta.push(meta);
    StepResult::Ok(())
}

pub fn op_null(vm: &mut Interpreter, _: u32, _: u32) -> StepResult<(), InterpreterError> {
    vm.stack.push(value::Value::Null);
    vm.stack_meta.push(ValueMeta {
        is_public: true,
        is_mutable: true,
        value_type: Type::Null
    });
    StepResult::Ok(())
}

pub fn op_pop(vm: &mut Interpreter, _: u32, _: u32) -> StepResult<(), InterpreterError> {
    vm.pop_stack();
    vm.pop_stack_meta();
    StepResult::Ok(())
}

pub fn op_define_global(
    vm: &mut Interpreter,
    operand: u32,
    _: u32,
) -> StepResult<(), InterpreterError> {
    let (is_mutable, idx) = bytecode::unpack_one_flag(operand);
    if let value::Value::String(name_id) = vm.read_constant(idx) {
        vm.set_constant_meta(
            idx,
            ValueMeta {
                is_public: false,
                is_mutable,
                value_type: Type::String
            },
        );
        let val = vm.pop_stack();
        let _ = vm.pop_stack_meta();
        vm.globals.insert(vm.get_str(name_id).clone(), val);
    } else {
        panic!(
            "expected string when defining global, found {}",
            value::type_of(&vm.read_constant(idx))
        );
    }
    StepResult::Ok(())
}

pub fn op_get_global(vm: &mut Interpreter, operand: u32, lineno: u32) -> StepResult<(), InterpreterError> {
    let idx = operand as usize;
    if let value::Value::String(name_id) = vm.read_constant(idx) {
        let meta = vm.read_constant_meta(idx);
        match vm.globals.get(vm.get_str(name_id)) {
            Some(val) => {
                vm.stack.push(val.clone());
                vm.stack_meta.push(meta);
            }
            None => {
                return StepResult::Err(InterpreterError::Runtime(format!(
                    "Undefined variable '{}' at line {}.",
                    vm.get_str(name_id),
                    lineno
                )));
            }
        }
    } else {
        panic!(
            "expected string when defining global, found {}",
            value::type_of(&vm.read_constant(idx))
        );
    }

    StepResult::Ok(())
}

pub fn op_set_global(
    vm: &mut Interpreter,
    operand: u32,
    lineno: u32
) -> StepResult<(), InterpreterError> {
    let idx = operand as usize;
    if let value::Value::String(name_id) = vm.read_constant(idx) {
        let meta = vm.read_constant_meta(idx);
        let name_str = vm.get_str(name_id).clone();
        let val = vm.peek().clone();
        let stored_val = vm.globals.entry(name_str.clone());
        if let std::collections::hash_map::Entry::Occupied(mut e) = stored_val {
            if !meta.is_mutable {
                return StepResult::Err(InterpreterError::Runtime(format!(
                    "This variable {} is immutable but you tried to insert a value at line {}.",
                    name_str, lineno
                )));
            } else {
                e.insert(val);
            }
        } else {
            return StepResult::Err(InterpreterError::Runtime(format!(
                "Use of undefined variable {} in setitem expression at line {}.",
                name_str, lineno
            )));
        }
    } else {
        panic!(
            "expected string when setting global, found {}",
            value::type_of(&vm.read_constant(idx))
        );
    }
    StepResult::Ok(())
}
