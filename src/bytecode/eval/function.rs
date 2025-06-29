use crate::bytecode::{bytecode, bytecode_interpreter::{Interpreter, InterpreterError}, StepResult};

pub fn op_get_local(vm: &mut Interpreter, operand: u32, _: u32) -> StepResult<(), InterpreterError> {
    let idx = operand as usize;
    let slots_offset = vm.frame().slots_offset;
    let val = vm.stack[slots_offset + idx - 1].clone();
    let meta = vm.stack_meta[slots_offset + idx - 1].clone();
    vm.stack.push(val);
    vm.stack_meta.push(meta);
    StepResult::Ok(())
}

pub fn op_define_local(vm: &mut Interpreter, operand: u32, _: u32) -> StepResult<(), InterpreterError> {
    let (is_mutable, idx) = bytecode::unpack_one_flag(operand);
    let slots_offset = vm.frame().slots_offset;
    let old_val = vm.stack[slots_offset + idx - 1].clone();
    let mut old_meta = vm.stack_meta[slots_offset + idx - 1].clone();
    old_meta.is_mutable = is_mutable;
    vm.stack[slots_offset + idx - 1] = old_val;
    vm.stack_meta[slots_offset + idx - 1] = old_meta;
    StepResult::Ok(())
}

pub fn op_set_local(vm: &mut Interpreter, operand: u32, lineno: u32) -> StepResult<(), InterpreterError> {
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
