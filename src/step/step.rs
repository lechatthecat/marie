use std::{cell::RefCell, rc::Rc, collections::HashMap, mem};

use cranelift::prelude::{InstBuilder, AbiParam, Variable, EntityRef, types, FunctionBuilder};
use cranelift_module::{Linkage, Module};

use crate::{bytecode_interpreter::{InterpreterError, Interpreter, Binop, CallFrame}, bytecode, value::{self, MarieValue, PropertyKey}, gc};

use super::jit_step::FunctionTranslator;

pub trait StepFunction {
    fn step(&mut self) -> Result<(), InterpreterError>;
}

impl StepFunction for Interpreter {
    fn step(&mut self) -> Result<(), InterpreterError> {
        let op = self.next_op_and_advance();

        if self.heap.should_collect() {
            self.collect_garbage();
        }

        match op {
            // Return is used in global "ste@" too, not only in function "ste@".
            (bytecode::Op::Return, _) => {
                let result = self.pop_stack();

                for idx in self.frame().slots_offset..self.stack.len() {
                    self.close_upvalues(idx);
                }

                if self.frames.len() <= 1 {
                    self.frames.pop();
                    return Ok(());
                }

                let num_to_pop = usize::from(self.frame().closure.function.locals_size) + 1;

                self.frames.pop();

                self.pop_stack_n_times(num_to_pop);

                self.stack.push(result.clone());
            }
            (bytecode::Op::Closure(is_public, idx, function_type, upvals), _) => {
                let constant = self.read_constant(idx);
                if let value::Value::Function(closure_handle) = constant {
                    let mut closure = self.get_closure(closure_handle).clone();
                    let closure_cloned = closure.clone();
                    let arg_count = closure.function.arity;
                    for _i in 0..arg_count {
                        self.jit.ctx.func.signature.params.push(AbiParam::new(types::I64));
                        self.stack.push(MarieValue { 
                            is_public: true,
                            is_mutable: true,
                            val: value::Value::Number(0f64),
                            jit_value: None,
                            jit_type: None,
                        });
                    }
                    let func_name = closure.function.name.clone();

                    let upvalues = upvals
                        .iter()
                        .map(|upval| match upval {
                            bytecode::UpvalueLoc::Upvalue(idx) => {
                                self.frame().closure.upvalues[*idx].clone()
                            }
                            bytecode::UpvalueLoc::Local(idx) => {
                                if let Some(upval) = self.find_open_uval(*idx) {
                                    upval
                                } else {
                                    let index = self.frame().slots_offset + *idx - 1;
                                    let upval = Rc::new(RefCell::new(value::Upvalue::Open(index)));
                                    self.upvalues.push(upval.clone());
                                    upval
                                }
                            }
                        })
                        .collect();

                    // Our language currently only supports one return value, though
                    // Cranelift is designed to support more.
                    self.jit.ctx.func.signature.returns.push(AbiParam::new(types::I64));

                    let mut builder = FunctionBuilder::new(&mut self.jit.ctx.func, &mut self.jit.builder_context);
                    
                    // Create the entry block, to start emitting code in.
                    let entry_block = builder.create_block();
    
                    // Since this is the entry block, add block parameters corresponding to
                    // the function's parameters.
                    builder.append_block_params_for_function_params(entry_block);
    
                    // Tell the builder to emit code in this block.
                    builder.switch_to_block(entry_block);
    
                    // And, tell the builder that this block will have no further
                    // predecessors. Since it's the entry block, it won't have any
                    // predecessors.
                    builder.seal_block(entry_block);
                    
                    // Call function once to jit compile it
                    self.frames.push(CallFrame::default());
                    let mut frame = self.frames.last_mut().unwrap();
                    frame.closure = closure_cloned;
                    frame.slots_offset = self.stack.len() - usize::from(arg_count);
                    frame.invoked_method_id = Some(closure_handle);

                    let mut entry_blocks = Vec::new();
                    entry_blocks.push(entry_block);
                    // Now translate the statements of the function body.
                    let mut trans = FunctionTranslator {
                        val_type: types::I64,
                        builder,
                        stack: &mut self.stack,
                        output: &mut self.output,
                        module: &mut self.jit.module,
                        frames: &mut self.frames,
                        heap: &mut self.heap,
                        upvalues: &mut self.upvalues,
                        entry_blocks,
                        is_done: false,
                        funcs: Vec::new(),
                        function_type,
                    };
                    trans.run()?;

                    // Next, declare the function to jit. Functions must be declared
                    // before they can be called, or defined.
                    let maybe_func_id = self
                    .jit
                    .module
                    .declare_function(&func_name, Linkage::Export, &self.jit.ctx.func.signature)
                    .map_err(|e| e.to_string());

                    let funcid = match maybe_func_id {
                        Ok(id) => id,
                        Err(err) => {
                            panic!("{}", err);
                        }
                    };

                    // Define the function to jit. This finishes compilation, although
                    // there may be outstanding relocations to perform. Currently, jit
                    // cannot finish relocations until all functions to be called are
                    // defined. For this toy demo for now, we'll just finalize the
                    // function below.
                    let jitresult = self.jit.module
                        .define_function(
                            funcid,
                            &mut self.jit.ctx,
                        )
                        .map_err(|e| e.to_string());

                    match jitresult {
                        Ok(_) => {},
                        Err(err) => {
                            panic!("{}", err);
                        }
                    };

                    // Now that compilation is finished, we can clear out the context state.
                    self.jit.module.clear_context(&mut self.jit.ctx);

                    // Finalize the functions which we just defined, which resolves any
                    // outstanding relocations (patching in addresses, now that they're
                    // available).
                    self.jit.module.finalize_definitions();

                    // We can now retrieve a pointer to the machine code.
                    let fn_code = self.jit.module.get_finalized_function(funcid);
                    
                    closure.function.function_pointer = Some(fn_code);
                    let mut is_returned = false;
                    closure.function.chunk.code.retain(|u|{
                        if bytecode::Op::Return == u.0 && !is_returned {
                            is_returned = true;
                            true
                        } else {
                            false
                        }
                    });
                    closure.function.chunk.constants.clear();
                    closure.function.locals_size = 0;
                    
                    self.stack
                        .push(
                            MarieValue {
                                is_public: is_public,
                                is_mutable: true,
                                val: value::Value::Function(
                                    self.heap.manage_closure(
                                        value::Closure {
                                            function: closure.function,
                                            upvalues,
                                        },
                                    )
                                ),
                                jit_value: None,
                                jit_type: None
                            }
                        );
                } else {
                    panic!(
                        "When interpreting bytecode::Op::Closure, expected function, found {:?}",
                        value::type_of(&constant)
                    );
                }
            }
            (bytecode::Op::Constant(idx), _) => {
                let constant = self.read_constant(idx);
                self.stack.push(
                    MarieValue { 
                        is_public: true,
                        is_mutable: true,
                        val: constant,
                        jit_value: None,
                        jit_type: None,
                    }
                );
            }
            (bytecode::Op::Nil, _) => {
                self.stack.push(
                    MarieValue {
                        is_public: true,
                        is_mutable: true,
                        val: value::Value::Nil,
                        jit_value: None,
                        jit_type: None,
                    }
                );
            }
            (bytecode::Op::True, _) => {
                self.stack.push(
                    MarieValue {
                        is_public: true,
                        is_mutable: true,
                        val: value::Value::Bool(true),
                        jit_value: None,
                        jit_type: None,
                    }
                );
            }
            (bytecode::Op::False, _) => {
                self.stack.push(
                    MarieValue {
                        is_public: true,
                        is_mutable: true,
                        val: value::Value::Bool(false),
                        jit_value: None,
                        jit_type: None,
                    }
                );
            }
            (bytecode::Op::Negate, lineno) => {
                let top_stack = &self.peek().val;
                let maybe_number = Interpreter::extract_number(top_stack);

                match maybe_number {
                        Some(to_negate) => {
                            self.pop_stack();
                            self.stack.push(
                                MarieValue {
                                    is_public: true,
                                    is_mutable: true,
                                    val: value::Value::Number(-to_negate),
                                    jit_value: None,
                                    jit_type: None,
                                }
                            );
                        }
                        None => {
                            return Err(InterpreterError::Runtime(format!(
                                "invalid operand to unary op negate. Expected number, found {:?} at line {}",
                                value::type_of(top_stack), lineno.value
                            )))
                        }
                    }
            }
            (bytecode::Op::AddString, lineno) => {
                let val1 = self.peek_by(0).clone().val;
                let val2 = self.peek_by(1).clone().val;

                match (&val1, &val2) {
                    (value::Value::String(s1), value::Value::String(s2)) => {
                        self.pop_stack();
                        self.pop_stack();
                        self.stack
                            .push(
                                MarieValue {
                                    is_public: true,
                                    is_mutable: true,
                                    val: value::Value::String(self.heap.manage_str(format!(
                                        "{}{}",
                                        self.get_str(*s2),
                                        self.get_str(*s1)
                                    ))),
                                    jit_value: None,
                                    jit_type: None,
                                }
                            );
                    }
                    (value::Value::Number(s1), value::Value::Number(s2)) => {
                        self.pop_stack();
                        self.pop_stack();
                        self.stack
                            .push(
                                MarieValue {
                                    is_public: true,
                                    is_mutable: true,
                                    val: value::Value::String(self.heap.manage_str(format!(
                                        "{}{}",
                                        s2.to_string(),
                                        s1.to_string()
                                    ))),
                                    jit_value: None,
                                    jit_type: None,
                                }
                            );
                    }
                    (value::Value::String(s1), value::Value::Number(s2)) => {
                        self.pop_stack();
                        self.pop_stack();
                        self.stack
                            .push(
                                MarieValue {
                                    is_public: true,
                                    is_mutable: true,
                                    val: value::Value::String(self.heap.manage_str(format!(
                                        "{}{}",
                                        s2.to_string(),
                                        self.get_str(*s1)
                                    ))),
                                    jit_value: None,
                                    jit_type: None,
                                }
                            );
                    }
                    (value::Value::Number(s1), value::Value::String(s2)) => {
                        self.pop_stack();
                        self.pop_stack();
                        self.stack
                            .push(
                                MarieValue {
                                    is_public: true,
                                    is_mutable: true,
                                    val: value::Value::String(self.heap.manage_str(format!(
                                        "{}{}",
                                        self.get_str(*s2),
                                        s1.to_string()
                                    ))),
                                    jit_value: None,
                                    jit_type: None,
                                }
                        );
                    }
                    _ => {
                        return Err(InterpreterError::Runtime(format!(
                            "invalid operands of type {:?} and {:?} in string concatination expression: \
                                 both operands must be string (line={})",
                            value::type_of(&val1),
                            value::type_of(&val2),
                            lineno.value
                        )))
                    }
                }
            }
            (bytecode::Op::Add, lineno) => {
                let val1 = self.peek_by(0).clone().val;
                let val2 = self.peek_by(1).clone().val;

                match (&val1, &val2) {
                    (value::Value::Number(_), value::Value::Number(_)) => {
                        self.numeric_binop(Binop::Add, lineno)?
                    }
                    (value::Value::String(_), value::Value::Number(_)) => {
                        self.numeric_binop(Binop::Add, lineno)?
                    }
                    (value::Value::Number(_), value::Value::String(_)) => {
                        self.numeric_binop(Binop::Add, lineno)?
                    }
                    (value::Value::String(_), value::Value::String(_)) => {
                        self.numeric_binop(Binop::Add, lineno)?
                    }
                    (value::Value::List(id1), value::Value::List(id2)) => {
                        self.pop_stack();
                        self.pop_stack();
                        let mut res = self.get_list_elements(*id2).clone();
                        res.extend(self.get_list_elements(*id1).clone());
                        self.stack
                            .push(
                                MarieValue {
                                    is_public: true,
                                    is_mutable: true,
                                    val: value::Value::List(self.heap.manage_list(res)),
                                    jit_value: None,
                                    jit_type: None,
                                }
                            );
                    }
                    _ => {
                        return Err(InterpreterError::Runtime(format!(
                            "invalid operands of type {:?} and {:?} in add expression: \
                                 both operands must be number or list (line={})",
                            value::type_of(&val1),
                            value::type_of(&val2),
                            lineno.value
                        )))
                    }
                }
            }
            (bytecode::Op::Subtract, lineno) => match self.numeric_binop(Binop::Sub, lineno) {
                Ok(()) => {}
                Err(err) => return Err(err),
            },
            (bytecode::Op::Multiply, lineno) => match self.numeric_binop(Binop::Mul, lineno) {
                Ok(()) => {}
                Err(err) => return Err(err),
            },
            (bytecode::Op::Divide, lineno) => match self.numeric_binop(Binop::Div, lineno) {
                Ok(()) => {}
                Err(err) => return Err(err),
            },
            (bytecode::Op::Not, lineno) => {
                let top_stack = &self.peek().val;
                let maybe_bool = Interpreter::extract_bool(top_stack);

                match maybe_bool {
                        Some(b) => {
                            self.pop_stack();
                            self.stack.push(
                                MarieValue {
                                    is_public: true,
                                    is_mutable: true,
                                    val: value::Value::Bool(!b),
                                    jit_value: None,
                                    jit_type: None,
                                }
                            );
                        }
                        None => {
                            return Err(InterpreterError::Runtime(format!(
                                "invalid operand in not expression. Expected boolean, found {:?} at line {}",
                                value::type_of(top_stack), lineno.value)))
                        }
                    }
            }
            (bytecode::Op::Equal, _) => {
                let val1 = self.pop_stack();
                let val2 = self.pop_stack();
                self.stack
                    .push(
                        MarieValue {
                            is_public: true,
                            is_mutable: true,
                            val: value::Value::Bool(self.values_equal(&val1.val, &val2.val)),
                            jit_value: None,
                            jit_type: None,
                        }
                    );
            }
            (bytecode::Op::Greater, lineno) => {
                let val1 = self.peek_by(0).clone().val;
                let val2 = self.peek_by(1).clone().val;

                match (&val1, &val2) {
                        (value::Value::Number(n1), value::Value::Number(n2)) => {
                            self.pop_stack();
                            self.pop_stack();

                            self.stack.push(
                                MarieValue {
                                    is_public: true,
                                    is_mutable: true,
                                    val: value::Value::Bool(n2 > n1),
                                    jit_value: None,
                                    jit_type: None,
                                }
                            );
                        }
                        _ => return Err(InterpreterError::Runtime(format!(
                            "invalid operands in Greater expression. Expected numbers, found {:?} and {:?} at line {}",
                            value::type_of(&val1), value::type_of(&val2), lineno.value)))

                    }
            }
            (bytecode::Op::Less, lineno) => {
                let val1 = self.peek_by(0).clone().val;
                let val2 = self.peek_by(1).clone().val;

                match (&val1, &val2) {
                        (value::Value::Number(n1), value::Value::Number(n2)) => {
                            self.pop_stack();
                            self.pop_stack();
                            self.stack.push(
                                MarieValue {
                                    is_public: true,
                                    is_mutable: true,
                                    val: value::Value::Bool(n2 < n1),
                                    jit_value: None,
                                    jit_type: None,
                                }
                            );
                        }
                        _ => return Err(InterpreterError::Runtime(format!(
                            "invalid operands in Less expression. Expected numbers, found {:?} and {:?} at line {}",
                            value::type_of(&val1), value::type_of(&val2), lineno.value)))

                    }
            }
            (bytecode::Op::Print, _) => {
                let to_print = self.peek().clone();
                self.print_val(&to_print.val);
                self.pop_stack();
            }
            (bytecode::Op::Pop, _) => {
                self.pop_stack();
            }
            (bytecode::Op::EndScope, _) => {}
            (bytecode::Op::DefineGlobal(is_mutable, idx), _) => {
                if let value::Value::String(name_id) = self.read_constant(idx) {
                    let mut val = self.pop_stack();
                    val.is_mutable = is_mutable;
                    self.globals.insert(self.get_str(name_id).clone(), val);
                } else {
                    panic!(
                        "expected string when defining global, found {:?}",
                        value::type_of(&self.read_constant(idx))
                    );
                }
            }
            (bytecode::Op::GetGlobal(idx), lineno) => {
                if let value::Value::String(name_id) = self.read_constant(idx) {
                    match self.globals.get(self.get_str(name_id)) {
                        Some(val) => {
                            self.stack.push(val.clone());
                        }
                        None => {
                            return Err(InterpreterError::Runtime(format!(
                                "Undefined variable '{}' at line {}.",
                                self.get_str(name_id),
                                lineno.value
                            )));
                        }
                    }
                } else {
                    panic!(
                        "expected string when defining global, found {:?}",
                        value::type_of(&self.read_constant(idx))
                    );
                }
            }
            (bytecode::Op::SetGlobal(idx), lineno) => {
                if let value::Value::String(name_id) = self.read_constant(idx) {
                    let name_str = self.get_str(name_id).clone();
                    let mut val = self.peek().clone();
                    let stored_val = self.globals.entry(name_str.clone());
                    if let std::collections::hash_map::Entry::Occupied(mut e) = stored_val
                    {
                        let foundval = e.get();
                        if !foundval.is_mutable {
                            return Err(InterpreterError::Runtime(format!(
                                "This variable {} is immutable but you tried to insert a value at line {}.",
                                name_str, lineno.value
                            )));
                        } else if value::type_of(&val.val) != value::type_of(&foundval.val) {
                            // TODO nilの場合は代入可能な値に変換するように
                            // TODO String->Numberの場合は代入可能な値に変換するように。不可能な場合はエラーに。
                            return Err(InterpreterError::Runtime(format!(
                                "You tried to insert a value to a variable whose type doesn't match with the value's type at line {}. Variable: {}, Value: {}",
                                lineno.value,
                                value::type_of(&foundval.val),
                                value::type_of(&val.val)
                            )));
                        } else {
                            val.is_mutable = foundval.is_mutable;
                            e.insert(val);
                        }
                    } else {
                        return Err(InterpreterError::Runtime(format!(
                            "Use of undefined variable {} in setitem expression at line {}.",
                            name_str, lineno.value
                        )));
                    }
                } else {
                    panic!(
                        "expected string when setting global, found {:?}",
                        value::type_of(&self.read_constant(idx))
                    );
                }
            }
            (bytecode::Op::GetLocal(idx), _) => {
                let slots_offset = self.frame().slots_offset;
                let val = self.stack[slots_offset + idx - 1].clone();
                self.stack.push(val);
            }
            (bytecode::Op::DefineLocal(is_mutable, idx), _) => {
                let slots_offset = self.frame().slots_offset;
                let mut old_val = self.stack[slots_offset + idx - 1].clone();
                old_val.is_mutable = is_mutable;
                self.stack[slots_offset + idx - 1] = old_val;
            }
            (bytecode::Op::DefineParamLocal(is_mutable, _parameter_type, idx), _) => {
                let slots_offset = self.frame().slots_offset;
                let mut old_val = self.stack[slots_offset + idx - 1].clone();
                old_val.is_mutable = is_mutable;
                self.stack[slots_offset + idx - 1] = old_val;
            }
            (bytecode::Op::SetLocal(idx), lineno) => {
                let mut val = self.peek().clone();
                let slots_offset = self.frame().slots_offset;
                let old_val = self.stack[slots_offset + idx - 1].clone();
                if !old_val.is_mutable {
                    return Err(InterpreterError::Runtime(format!(
                        "This variable is immutable but you tried to insert a value at line {}.",
                        lineno.value
                    )));
                }
                if value::type_of(&old_val.val) != value::type_of(&val.val) {
                    // TODO nilの場合は代入可能な値に変換するように
                    // TODO String->Numberの場合は代入可能な値に変換するように。不可能な場合はエラーに。
                    return Err(InterpreterError::Runtime(format!(
                        "You tried to insert a value to a variable whose type doesn't match with the value's type at line {}. Variable: {}, Value: {}",
                        lineno.value,
                        value::type_of(&old_val.val),
                        value::type_of(&val.val)
                    )));
                }
                val.is_mutable = old_val.is_mutable;
                self.stack[slots_offset + idx - 1] = val
            }
            (bytecode::Op::GetUpval(idx), _) => {
                let upvalue = self.frame().closure.upvalues[idx].clone();
                let mut is_mutable = true;
                let val = match &*upvalue.borrow() {
                    value::Upvalue::Closed(value) => value.clone(),
                    value::Upvalue::Open(stack_index) => {
                        let val = self.stack[*stack_index].clone();
                        is_mutable = val.is_mutable;
                        val.val                        
                    },
                };
                self.stack.push(
                    MarieValue{
                        is_mutable,
                        is_public: true,
                        val,
                        jit_value: None,
                        jit_type: None,
                    }
                );
            }
            (bytecode::Op::SetUpval(idx), _) => {
                let new_value = self.peek().clone();
                let upvalue = self.frame().closure.upvalues[idx].clone();
                match &mut *upvalue.borrow_mut() {
                    value::Upvalue::Closed(value) => *value = new_value.val,
                    value::Upvalue::Open(stack_index) => self.stack[*stack_index] = new_value,
                };
            }
            (bytecode::Op::JumpIfFalse(offset), _) => {
                if self.is_falsey(&self.peek().val) {
                    self.frame_mut().ip += offset;
                }
            }
            (bytecode::Op::Jump(offset), _) => {
                self.frame_mut().ip += offset;
            }
            (bytecode::Op::Loop(offset), _) => {
                self.frame_mut().ip -= offset;
            }
            (bytecode::Op::Call(arg_count), _) => {
                self.call_value(self.peek_by(arg_count.into()).clone(), arg_count)?;
            }
            (bytecode::Op::StartUse(idx, _, file_name_idx), _) => {
                let constant = self.read_constant(idx);
                let file_name = if let value::Value::String(name_id) = self.read_constant(file_name_idx) {
                    self.get_str(name_id).clone()
                } else {
                    panic!(
                        "Expected string for file path, found {:?}",
                        value::type_of(&constant)
                    );
                };
                if let value::Value::Function(closure_handle) = constant {
                    let closure = self.get_closure(closure_handle).clone();
                    let mut call_frame = CallFrame::default();
                    call_frame.is_file = true;
                    self.frames.push(call_frame);
                    let mut frame = self.frames.last_mut().unwrap();
                    frame.closure.function.name = file_name;
                    frame.closure = closure;
                    frame.slots_offset = self.stack.len();
                    frame.invoked_method_id = Some(closure_handle);
                    self.stack.push(
                        MarieValue {
                            is_public: false,
                            is_mutable: false,
                            val: value::Value::Function(self.heap.manage_closure(
                                value::Closure {
                                    function: frame.closure.function.clone(),
                                    upvalues: Vec::new(),
                                },
                            )),
                            jit_value: None,
                            jit_type: None
                        }
                    );
                } else {
                    panic!(
                        "When interpreting bytecode::Op::Closure, expected function, found {:?}",
                        value::type_of(&constant)
                    );
                }
            }
            (bytecode::Op::CreateInstance(arg_count), _) => {
                self.create_instance_val(self.peek_by(arg_count.into()).clone().val, arg_count)?;
            }
            (bytecode::Op::CloseUpvalue, _) => {
                let idx = self.stack.len() - 1;
                self.close_upvalues(idx);
                self.stack.pop();
            }
            (bytecode::Op::Class(idx), _) => {
                if let value::Value::String(name_id) = self.read_constant(idx) {
                    let name = self.get_str(name_id).clone();
                    self.stack
                        .push(
                            MarieValue{
                                is_mutable: true,
                                is_public: true,
                                val:value::Value::Class(
                                    self.heap.manage_class(
                                        value::Class {
                                            name,
                                            properties: HashMap::new(),
                                        }
                                    )
                                ),
                                jit_value: None,
                                jit_type: None,
                            }
                        );
                } else {
                    panic!(
                        "expected string when defining class, found {:?}",
                        value::type_of(&self.read_constant(idx))
                    );
                }
            }
            (bytecode::Op::DefineProperty(is_mutable, is_public, idx), _) => {
                if let value::Value::String(property_name_id) = self.read_constant(idx) {
                    let property_name = self.heap.get_str(property_name_id).clone();
                    let maybe_class = self.peek_by(1).clone().val;
                    match maybe_class {
                        value::Value::Class(class_id) => {
                            let mut val = self.pop_stack();
                            let class = self.heap.get_class_mut_and_set_class_id(class_id, property_name_id);
                            val.is_public = is_public;
                            val.is_mutable = is_mutable;
                            if let Some(_already_defined) = class.properties.get(
                                &PropertyKey{ name: property_name.clone(), id: class_id }
                            ) {
                                return Err(InterpreterError::Runtime(format!(
                                    "This attribute is already defined in this class: {:?}",
                                    &property_name
                                )));
                            }
                            class.properties.insert(PropertyKey{ name: property_name, id: class_id }, val);
                        }
                        _ => {
                            panic!(
                                "should only define methods and attributes on a class! tried on {:?}",
                                self.format_val(&maybe_class)
                            );
                        }
                    }
                } else {
                    panic!("expected string when defining a property.");
                }
            }
            (bytecode::Op::SetProperty(idx), _) => {
                if let value::Value::String(attr_id) = self.read_constant(idx) {
                    let val = self.pop_stack();
                    let instance = self.pop_stack();
                    self.setattr(instance, val.clone(), attr_id)?;
                    self.stack.push(val);
                } else {
                    panic!(
                        "expected string when setting property, found {:?}",
                        value::type_of(&self.read_constant(idx))
                    )
                }
            }
            (bytecode::Op::GetProperty(idx), _) => {
                if let value::Value::String(attr_id) = self.read_constant(idx) {
                    let maybe_instance = self.peek().clone();

                    let (class_id, instance_id) = match maybe_instance.val {
                        value::Value::Instance(instance_id) => {
                            let instance = self.heap.get_instance(instance_id).clone();
                            (instance.class_id, instance_id)
                        }
                        _ => panic!(),
                    };

                    let class = self.heap.get_class(class_id).clone();
                    let mut val = maybe_instance.clone();
                    if let Some(attr) = self.getattr(val.val, attr_id, class_id)? {
                        self.pop_stack();
                        val.val = attr;
                        self.stack.push(val);
                    } else if !self.bind_method(instance_id, class, attr_id)? {
                        return Err(InterpreterError::Runtime(format!(
                            "value {} has no attribute {}.",
                            self.format_val(&maybe_instance.val),
                            self.get_str(attr_id)
                        )));
                    }
                } else {
                    panic!(
                        "expected string when setting property, found {:?}",
                        value::type_of(&self.read_constant(idx))
                    )
                }
            }
            (bytecode::Op::Method(is_public, idx), _) => {
                if let value::Value::String(method_name_id) = self.read_constant(idx) {
                    let method_name = self.heap.get_str(method_name_id).clone();
                    let maybe_method = self.peek_by(0).clone().val;
                    let maybe_method_id = gc::Heap::extract_id(&maybe_method).unwrap();
                    let maybe_class = self.peek_by(1).clone().val;
                    match maybe_class {
                        value::Value::Class(class_id) => {
                            let class = self.heap.get_class_mut_and_set_class_id(class_id, maybe_method_id);
                            class.properties.insert(
                                PropertyKey{ name: method_name, id: class_id }, 
                                MarieValue {
                                    val: value::Value::Function(maybe_method_id),
                                    is_mutable: false,
                                    is_public: is_public,
                                    jit_value: None,
                                    jit_type: None,
                                } 
                            );
                            self.pop_stack();
                        }
                        _ => {
                            panic!(
                                "should only define methods and attributes on a class! tried on {:?}",
                                self.format_val(&maybe_class)
                            );
                        }
                    }
                } else {
                    panic!("expected string when defining a method.");
                }
            }
            (bytecode::Op::Invoke(method_name, arg_count), _) => {
                self.invoke(&method_name, arg_count)?;
            }
            (bytecode::Op::Inherit, lineno) => {
                {
                    let (superclass_id, subclass_id) = match (&self.peek_by(1).val, &self.peek().val) {
                        (value::Value::Class(superclass_id), value::Value::Class(subclass_id)) => {
                            (*superclass_id, *subclass_id)
                        }
                        (not_a_class, value::Value::Class(_)) => {
                            return Err(InterpreterError::Runtime(format!(
                                "Superclass must be a class, found {:?} at lineno={:?}",
                                value::type_of(not_a_class),
                                lineno
                            )));
                        }
                        _ => panic!("expected classes when interpreting Inherit!"),
                    };

                    let superclass_properties = self.get_class(superclass_id).properties.clone();
                    let subclass = self.get_class_mut(subclass_id);
                    
                    subclass.properties.extend(superclass_properties);
                }
                self.pop_stack(); //subclass
            }
            (bytecode::Op::GetSuper(idx), _) => {
                let method_id = if let value::Value::String(method_id) = self.read_constant(idx) {
                    method_id
                } else {
                    panic!();
                };

                let maybe_superclass = self.pop_stack();
                let superclass = match maybe_superclass.val {
                    value::Value::Class(class_id) => {
                        self.get_class(class_id).clone()
                    },
                    _ => panic!(),
                };

                let maybe_instance = &self.peek().val;
                let instance_id = match maybe_instance {
                    value::Value::Instance(instance_id) => *instance_id,
                    _ => panic!(),
                };

                self.bindattr(instance_id, superclass.properties.clone());

                if !self.bind_method(instance_id, superclass, method_id)? {
                    return Err(InterpreterError::Runtime(format!(
                        "superclass {} has no function or attribute: {}.",
                        self.format_val(&maybe_superclass.val),
                        self.get_str(method_id)
                    )));
                }
            }
            (bytecode::Op::SuperInvoke(method_name, arg_count), _) => {
                let maybe_superclass = self.pop_stack();
                let superclass_id = match maybe_superclass.val {
                    value::Value::Class(class_id) => class_id,
                    _ => panic!("{}", self.format_val(&maybe_superclass.val)),
                };
                self.invoke_from_class(superclass_id, &method_name, arg_count)?;
            }
            (bytecode::Op::BuildList(size), _) => {
                let mut list_elements = Vec::new();
                for _ in 0..size {
                    list_elements.push(self.pop_stack())
                }
                list_elements.reverse();
                self.stack.push(
                    MarieValue{
                        is_mutable: true,
                        is_public: true,
                        val: value::Value::List(self.heap.manage_list(list_elements)),
                        jit_value: None,
                        jit_type: None,
                    }
                );
            }
            (bytecode::Op::Subscr, lineno) => {
                let subscript = self.pop_stack();
                let value_to_subscript = self.pop_stack();
                let res = self.subscript(value_to_subscript, subscript, lineno)?;
                self.stack.push(res);
            }
            (bytecode::Op::SetItem, lineno) => {
                let rhs = self.pop_stack();
                let subscript = self.pop_stack();
                let lhs = self.pop_stack();
                self.setitem(lhs, subscript, rhs.clone(), lineno)?;
                self.stack.push(rhs);
            }
            _ => {}
        }
        Ok(())
    }
}

