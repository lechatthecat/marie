use core::slice;
use std::{cell::RefCell, rc::Rc, collections::HashMap, mem};

use cranelift::prelude::{InstBuilder, AbiParam, Variable, EntityRef, types};
use cranelift_module::{Linkage, Module};

use crate::{bytecode_interpreter::{InterpreterError, Interpreter, Binop}, bytecode, value::{self, MarieValue, PropertyKey}, gc};

pub trait JitStepFunction {
    fn jit_step(&mut self) -> Result<(), InterpreterError>;
}

impl JitStepFunction for Interpreter {
    fn jit_step(&mut self) -> Result<(), InterpreterError> {
        let op = self.next_op_and_advance();

        if self.heap.should_collect() {
            self.collect_garbage();
        }

        match op {
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

                if self.is_in_function {
                    let func_name = self.frame().closure.function.name.clone();

                    //let return_variable = Variable::new(id);
                    //self.builder.declare_var(return_variable, self.module.target_config().pointer_type());

                    let mut val = self.builder.ins().f64const(0);
                    if let Some(jitval) = result.jit_value {
                        val = jitval;
                    } else if let Some(jitvar) = result.jit_variable {
                        val = self.builder.use_var(jitvar);
                    }
                    val = self.f64_to_bits(val);
                    // self.builder.def_var(var, val);
                    //let return_value = self.builder.use_var(return_variable);
                    // Emit the return instruction.
                    self.builder.ins().return_(&[val]);

                    // Tell the builder we're done with this function.
                    self.builder.finalize();
                    // Next, declare the function to jit. Functions must be declared
                    // before they can be called, or defined.
                    let maybe_func_id = self
                        .module
                        .declare_function(&func_name, Linkage::Export, &self.ctx.func.signature)
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
                    let jitresult = self.module
                        .define_function(
                            funcid,
                            &mut self.ctx,
                        )
                        .map_err(|e| e.to_string());

                    match jitresult {
                        Ok(_) => {},
                        Err(err) => {
                            panic!("{}", err);
                        }
                    };

                    // Now that compilation is finished, we can clear out the context state.
                    self.module.clear_context(&mut self.ctx);

                    // Finalize the functions which we just defined, which resolves any
                    // outstanding relocations (patching in addresses, now that they're
                    // available).
                    self.module.finalize_definitions();

                    // We can now retrieve a pointer to the machine code.
                    let fn_code = self.module.get_finalized_function(funcid);

                    // test
                    let a = (4 as f64).to_bits() as i64; 
                    let b = (5 as f64).to_bits() as i64; 
                    let c = (6 as f64).to_bits() as i64; 
                    let result: Result<*const u8, String> = unsafe {
                        self.run_code(fn_code, 
                            (
                                a,
                                b,
                                c
                            )
                        )
                    };
                    match result {
                        Ok(result_val) => {
                            println!("{}", f64::from_bits(result_val as u64))
                        },
                        Err(err) => {
                            panic!("{}", err);
                        }
                    };
                    
                    self.entry_blocks.clear();

                    self.is_in_function = false;
                }
            }
            (bytecode::Op::Closure(_, _, _), _) => {}
            (bytecode::Op::Constant(idx), _) => {
                let constant = self.read_constant(idx);
                self.stack.push(
                    MarieValue { 
                        is_public: true,
                        is_mutable: true,
                        val: constant,
                        jit_value: None,
                        jit_variable: None,
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
                        jit_variable: None
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
                        jit_variable: None
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
                        jit_variable: None
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
                                    jit_variable: None
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
                                    jit_variable: None
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
                                    jit_variable: None
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
                                    jit_variable: None
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
                                    jit_variable: None
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
                let marie_val1 = self.peek_by(0).clone();
                let marie_val2 = self.peek_by(1).clone();
                let mut val1 = marie_val1.val;
                let mut val2 = marie_val2.val;

                match (&mut val1, &mut val2) {
                    (value::Value::Number(num1), value::Value::Number(num2)) => {
                        let jit_value1 = if marie_val1.jit_variable.is_some() {
                            let var1 = marie_val1.jit_variable.unwrap();
                            self.builder.use_var(var1)
                        } else {
                            self.builder.ins().f64const(*num1)
                        };
                        //self.print_jitval(jit_value1);
                        let jit_value2 = if marie_val2.jit_variable.is_some() {
                            let var2 = marie_val2.jit_variable.unwrap();
                            self.builder.use_var(var2)
                        } else {
                            self.builder.ins().f64const(*num2)
                        };
                        //self.print_jitval(jit_value2);
                        let added = self.builder.ins().fadd(jit_value1, jit_value2);
                        self.print_jitval(added);
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
                                    jit_variable: None
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
                                    jit_variable: None
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
                            jit_variable: None
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
                                    jit_variable: None
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
                                    jit_variable: None
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
                let val = old_val.val.clone();
                match val {
                    value::Value::Number(v) => {
                        let variable = Variable::new(idx);
                        self.builder.declare_var(variable, types::F64);
                        // Set up the return variable of the function. Above, we declared a
                        // variable to hold the return value. Here, we just do a use of that
                        // variable.
                        let val = self.builder.ins().f64const(v);
                        self.builder.def_var(variable, val);
                        old_val.jit_variable = Some(variable);
                    },
                    value::Value::Bool(v) => {},
                    value::Value::String(v) => {},
                    value::Value::Function(v) => {},
                    value::Value::Instance(v) => {},
                    value::Value::BoundMethod(v) => {},
                    value::Value::Class(v) => {},
                    value::Value::NativeFunction(v) => {},
                    value::Value::Nil => {},
                    value::Value::List(v) => {},
                    value::Value::Errored => {}
                }
                old_val.is_mutable = is_mutable;
                self.stack[slots_offset + idx - 1] = old_val;
            }
            (bytecode::Op::DefineParamLocal(is_mutable, idx), _) => {
                let slots_offset = self.frame().slots_offset;
                let mut old_val = self.stack[slots_offset + idx - 1].clone();
                let val = old_val.val.clone();
                match val {
                    value::Value::Number(v) => {
                        let entry_block = self.entry_blocks.last().unwrap();
                        let val = self.builder.block_params(*entry_block)[idx-1];
                        let f64val =  self.bits_to_f64(val);
                        //let f64val =  self.builder.ins().f64const(v);
                        //let f64val = self.bits_to_f64(val);
                        let variable = Variable::new(idx-1);
                        self.builder.declare_var(variable, types::F64);
                        // Set up the return variable of the function. Above, we declared a
                        // variable to hold the return value. Here, we just do a use of that
                        // variable.
                        self.builder.def_var(variable, f64val);
                        let a = self.builder.use_var(variable);
                        self.print_jitval(a);
                        old_val.jit_variable = Some(variable);
                    },
                    value::Value::Bool(v) => {},
                    value::Value::String(v) => {},
                    value::Value::Function(v) => {},
                    value::Value::Instance(v) => {},
                    value::Value::BoundMethod(v) => {},
                    value::Value::Class(v) => {},
                    value::Value::NativeFunction(v) => {},
                    value::Value::Nil => {},
                    value::Value::List(v) => {},
                    value::Value::Errored => {}
                }
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
                        jit_variable: None
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
            (bytecode::Op::StartUse(idx), _) => {
                let constant = self.read_constant(idx);

                if let value::Value::Function(closure_handle) = constant {
                    self.prepare_call(closure_handle, 0)?;
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
                                jit_variable: None
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
                                    jit_variable: None
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
                        jit_variable: None
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
        }
        Ok(())
    }
}

