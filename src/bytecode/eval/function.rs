use crate::bytecode::{bytecode::{self, ValueMeta}, bytecode_interpreter::{Interpreter, InterpreterError}, values::value::{self, Type}, StepResult};

pub fn op_get_local(vm: &mut Interpreter, operand: usize, _: usize) -> StepResult<(), InterpreterError> {
    let idx = operand as usize;
    let slots_offset = vm.frame().slots_offset;
    let val = vm.stack[slots_offset + idx - 1].clone();
    let meta = vm.stack_meta[slots_offset + idx - 1].clone();
    vm.stack.push(val);
    vm.stack_meta.push(meta);
    StepResult::Ok(())
}

pub fn op_define_local(vm: &mut Interpreter, operand: usize, _: usize) -> StepResult<(), InterpreterError> {
    let (is_mutable, idx) = bytecode::unpack_one_flag(operand);
    let slots_offset = vm.frame().slots_offset;
    let old_val = vm.stack[slots_offset + idx - 1].clone();
    let mut old_meta = vm.stack_meta[slots_offset + idx - 1].clone();
    old_meta.is_mutable = is_mutable;
    vm.stack[slots_offset + idx - 1] = old_val;
    vm.stack_meta[slots_offset + idx - 1] = old_meta;
    StepResult::Ok(())
}

pub fn op_set_local(vm: &mut Interpreter, operand: usize, lineno: usize) -> StepResult<(), InterpreterError> {
    let idx = operand as usize;
    let val = vm.peek().clone();
    //let _ = self.peek_meta().clone();
    let slots_offset = vm.frame().slots_offset;
    let meta = vm.stack_meta[slots_offset + idx - 1];
    if !meta.is_mutable {
        return StepResult::Err(InterpreterError::Runtime(format!(
            "This variable is immutable but you tried to insert a value at line {}.",
            lineno
        )));
    }
    vm.stack[slots_offset + idx - 1] = val;
    StepResult::Ok(())
}

pub fn op_closure(vm: &mut Interpreter, operand: usize, _lineno: usize) -> StepResult<(), InterpreterError> {
    let (is_public, idx) = bytecode::unpack_one_flag(operand);
    let constant = vm.read_constant(idx);

    if let value::Value::Function(closure_handle) = constant {
        let closure = vm.get_closure(closure_handle).clone();

        vm.stack
            .push(value::Value::Function(vm.heap.manage_closure(
                value::Closure {
                    function: closure.function,
                },
            )));
        vm.stack_meta.push(ValueMeta { is_public, is_mutable: true, value_type: value::Type::Function});
    } else {
        return StepResult::Err(InterpreterError::Runtime(format!(
            "When interpreting bytecode::Opcode::Closure, expected function, found {}",
            value::type_of(&constant)
        )))
    }
    StepResult::Ok(())
}

pub fn op_call(vm: &mut Interpreter, operand: usize, _lineno: usize) -> StepResult<(), InterpreterError> {
    let arg_count = operand;
    return match vm.call_value(vm.peek_by(arg_count.into()).clone(), arg_count) {
        Ok(_) => StepResult::Ok(()),
        Err(e) => StepResult::Err(e),
    };
}

pub fn op_return(vm: &mut Interpreter, _: usize, _: usize) -> StepResult<(), InterpreterError> {
    let result = vm.pop_stack();
    let _ = vm.pop_stack_meta();

    if vm.frames.len() <= 1 {
        vm.frames.pop();
        return StepResult::OkReturn(());
    }

    // pop function also, so we plus 1 here.
    let num_to_pop = usize::from(vm.frame().closure.function.locals_size + 1);
    vm.frames.pop();

    vm.pop_stack_n_times(num_to_pop);
    vm.pop_stack_meta_n_times(num_to_pop);

    vm.stack.push(result.clone());
    vm.stack_meta.push(ValueMeta {
        is_public: true,
        is_mutable: true,
        value_type: result.get_type()
    });
    StepResult::Ok(())
}

pub fn op_end_of_scope(vm: &mut Interpreter, _: usize, _: usize) -> StepResult<(), InterpreterError> {
    vm.stack.push(value::Value::Null);
    vm.stack_meta.push(ValueMeta {
        is_public: true,
        is_mutable: true,
        value_type: Type::Null
    });

    let result = vm.pop_stack();
    let _ = vm.pop_stack_meta();

    if vm.frames.len() <= 1 {
        vm.frames.pop();
        return StepResult::OkReturn(());
    }

    // pop function also, so we plus 1 here.
    let num_to_pop = usize::from(vm.frame().closure.function.locals_size + 1);
    vm.frames.pop();

    vm.pop_stack_n_times(num_to_pop);
    vm.pop_stack_meta_n_times(num_to_pop);

    vm.stack.push(result.clone());
    vm.stack_meta.push(ValueMeta {
        is_public: true,
        is_mutable: true,
        value_type: result.get_type()
    });
    StepResult::Ok(())
}