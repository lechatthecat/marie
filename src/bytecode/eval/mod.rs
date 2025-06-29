pub mod calculation;
pub mod logical;
pub mod frame;
pub mod function;

use crate::bytecode::{bytecode::{self, ValueMeta}, bytecode_interpreter::{Interpreter, InterpreterError}, frames::call_frame::CallFrame, values::{value, Binop}, StepResult};

pub fn op_return(vm: &mut Interpreter, _: u32, _: u32) -> StepResult<(), InterpreterError> {
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

    vm.stack.push(result);
    vm.stack_meta.push(ValueMeta {
        is_public: true,
        is_mutable: true,
    });
    StepResult::Ok(())
}


pub fn op_print(vm: &mut Interpreter, _: u32, _: u32) -> StepResult<(), InterpreterError> {
    let to_print = vm.peek().clone();
    vm.print_val(&to_print);
    vm.pop_stack();
    vm.pop_stack_meta();
    StepResult::Ok(())
}

pub fn op_add_string(vm: &mut Interpreter, _: u32, lineno: u32) -> StepResult<(), InterpreterError> {

    let val1 = vm.peek_by(0).clone();
    let val2 = vm.peek_by(1).clone();

    match (&val1, &val2) {
        (value::Value::String(s1), value::Value::String(s2)) => {
            vm.pop_stack_and_stackmeta();
            vm.pop_stack_and_stackmeta();
            vm.stack
                .push(value::Value::String(vm.heap.manage_str(format!(
                    "{}{}",
                    vm.get_str(*s2),
                    vm.get_str(*s1),
                ))));
            vm.stack_meta.push(ValueMeta {
                is_public: true,
                is_mutable: true,
            });
        }
        (value::Value::Number(s1), value::Value::String(s2)) => {
            vm.pop_stack_and_stackmeta();
            vm.pop_stack_and_stackmeta();
            vm.stack
                .push(value::Value::String(vm.heap.manage_str(format!(
                    "{}{}",
                    vm.get_str(*s2),
                    s1.to_string()
                ))));
            vm.stack_meta.push(ValueMeta {
                is_public: true,
                is_mutable: true,
            });
        }
        (value::Value::String(s1), value::Value::Number(s2)) => {
            vm.pop_stack_and_stackmeta();
            vm.pop_stack_and_stackmeta();
            vm.stack
                .push(value::Value::String(vm.heap.manage_str(format!(
                    "{}{}",
                    s2.to_string(),
                    vm.get_str(*s1),
                ))));
            vm.stack_meta.push(ValueMeta {
                is_public: true,
                is_mutable: true,
            });
        }
        (value::Value::Number(s1), value::Value::Number(s2)) => {
            vm.pop_stack_and_stackmeta();
            vm.pop_stack_and_stackmeta();
            vm.stack
                .push(value::Value::String(vm.heap.manage_str(format!(
                    "{}{}",
                    s2.to_string(),
                    s1.to_string()
                ))));
            vm.stack_meta.push(ValueMeta {
                is_public: true,
                is_mutable: true,
            });
        }
        _ => {
            return StepResult::Err(InterpreterError::Runtime(format!(
            "invalid operands of type {} and {} in string concatination expression: \
                        both operands must be string (line={})",
            value::type_of(&val1),
            value::type_of(&val2),
            lineno
        )))
        }
    }

    StepResult::Ok(())
}

pub fn op_loop(vm: &mut Interpreter, operand: u32, _: u32) -> StepResult<(), InterpreterError> {
    let offset = operand as usize;
    vm.frame_mut().instruction_pointer -= offset;
    StepResult::Ok(())
}

pub fn op_build_list(vm: &mut Interpreter, operand: u32, _: u32) -> StepResult<(), InterpreterError> {
    let size = operand as usize;
    let mut list_elements = Vec::new();
    for _ in 0..size {
        list_elements.push(vm.pop_stack());
        vm.pop_stack_meta();
    }
    list_elements.reverse();
    vm.stack_meta.push(ValueMeta { is_public: true, is_mutable: true, });
    vm.stack
        .push(value::Value::List(vm.heap.manage_list(list_elements)));
    StepResult::Ok(())
}

pub fn op_start_include(vm: &mut Interpreter, operand: u32, _: u32) -> StepResult<(), InterpreterError> {
    let (idx, _localsize) = bytecode::unpack_start_include(operand);
    let idx = idx  as usize;
    let constant = vm.read_constant(idx);
    if let value::Value::Function(closure_handle) = constant {
        let meta = vm.read_constant_meta(idx);
        let closure = vm.get_closure(closure_handle).clone();
        let mut call_frame = CallFrame::default();
        call_frame.is_include_file = true;
        vm.frames.push(call_frame);
        let frame = vm.frames.last_mut().unwrap();
        frame.closure = closure;
        frame.slots_offset = vm.stack.len();
        frame.invoked_method_id = Some(closure_handle);
        vm.stack.push(value::Value::Function(vm.heap.manage_closure(
            value::Closure {
                function: frame.closure.function.clone(),
                upvalues: Vec::new(),
            },
        )));
        vm.stack_meta.push(meta);
    } else {
        panic!(
            "When interpreting bytecode::Op::Closure, expected function, found {}",
            value::type_of(&constant)
        );
    }
    StepResult::Ok(())
}

