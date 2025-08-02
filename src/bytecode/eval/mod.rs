pub mod calculation;
pub mod frame;
pub mod function;
pub mod logical;

use crate::bytecode::{
    bytecode::{self, ValueMeta},
    bytecode_interpreter::{Interpreter, InterpreterError},
    frames::call_frame::CallFrame,
    values::value,
    StepResult,
};

pub fn op_print(vm: &mut Interpreter, _: usize, _: usize) -> StepResult<(), InterpreterError> {
    let to_print = vm.peek().clone();
    vm.print_val(&to_print);
    vm.pop_stack();
    vm.pop_stack_meta();
    StepResult::Ok(())
}

pub fn op_add_string(
    vm: &mut Interpreter,
    _: usize,
    lineno: usize,
) -> StepResult<(), InterpreterError> {
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
                value_type: value::Type::String,
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
                value_type: value::Type::String,
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
                value_type: value::Type::String,
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
                value_type: value::Type::String,
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

pub fn op_loop(vm: &mut Interpreter, operand: usize, _: usize) -> StepResult<(), InterpreterError> {
    let offset = operand as usize;
    vm.frame_mut().instruction_pointer -= offset;
    StepResult::Ok(())
}

pub fn op_build_list(
    vm: &mut Interpreter,
    operand: usize,
    _: usize,
) -> StepResult<(), InterpreterError> {
    let size = operand as usize;
    let mut list_elements = Vec::new();
    for _ in 0..size {
        list_elements.push(vm.pop_stack());
        vm.pop_stack_meta();
    }
    list_elements.reverse();
    vm.stack_meta.push(ValueMeta {
        is_public: true,
        is_mutable: true,
        value_type: value::Type::List,
    });
    vm.stack
        .push(value::Value::List(vm.heap.manage_list(list_elements)));
    StepResult::Ok(())
}

pub fn op_list_subscript(
    vm: &mut Interpreter,
    _: usize,
    lineno: usize,
) -> StepResult<(), InterpreterError> {
    let subscript = vm.pop_stack();
    let _subcript_meta = vm.pop_stack_meta();
    let value_to_subscript = vm.pop_stack();
    let value_to_subscript_meta = vm.pop_stack_meta();
    match subscription(vm, value_to_subscript, subscript, lineno) {
        Ok(res) => {
            vm.stack.push(res);
            vm.stack_meta.push(value_to_subscript_meta);
        }
        Err(err) => {
            return StepResult::Err(err);
        }
    }
    StepResult::Ok(())
}

fn subscription(
    vm: &mut Interpreter,
    value: value::Value,
    subscript: value::Value,
    lineno: usize,
) -> Result<value::Value, InterpreterError> {
    if let value::Value::List(id) = value {
        if let value::Value::Number(index_float) = subscript {
            let elements = vm.get_list_elements(id);
            match subscript_to_inbound_index(elements.len(), index_float, lineno) {
                Ok(index_int) => Ok(elements[index_int].clone()),
                Err(err) => Err(InterpreterError::Runtime(err)),
            }
        } else {
            Err(InterpreterError::Runtime(format!(
                "Invalid subscript of type {} in subscript expression",
                value::type_of(&value)
            )))
        }
    } else {
        Err(InterpreterError::Runtime(format!(
            "Invalid value of type {} in subscript expression",
            value::type_of(&value)
        )))
    }
}

fn subscript_to_inbound_index(
    list_len: usize,
    index_float: f64,
    lineno: usize,
) -> Result<usize, String> {
    let index_int = index_float as i64;
    if 0 <= index_int && index_int < list_len as i64 {
        return Ok(index_int as usize);
    }
    if index_int < 0 && -index_int <= list_len as i64 {
        return Ok((list_len as i64 + index_int) as usize);
    }
    Err(format!("List subscript index out of range at {}", lineno))
}

pub fn op_start_include(
    vm: &mut Interpreter,
    operand: usize,
    _: usize,
) -> StepResult<(), InterpreterError> {
    let (idx, _localsize) = bytecode::unpack_start_include(operand);
    let idx = idx as usize;
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
            },
        )));
        vm.stack_meta.push(meta);
    } else {
        return StepResult::Err(InterpreterError::Runtime(format!(
            "When interpreting bytecode::Opcode::StartInclude, expected function, found {}",
            value::type_of(&constant)
        )));
    }
    StepResult::Ok(())
}

pub fn op_do_nothing(_: &mut Interpreter, _: usize, _: usize) -> StepResult<(), InterpreterError> {
    StepResult::Ok(())
}
