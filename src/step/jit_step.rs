use core::slice;
use std::{cell::RefCell, rc::Rc, collections::HashMap, mem};

use cranelift::{prelude::{InstBuilder, AbiParam, Variable, EntityRef, types, FunctionBuilder, Block, StackSlotData, StackSlotKind, IntCC, FunctionBuilderContext}, codegen::{ir::{FuncRef, ArgumentPurpose, immediates::Offset32}, self}};
use cranelift_jit::JITModule;
use cranelift_module::{Linkage, Module, FuncId, DataContext};
use cranelift::prelude::Value;
use crate::{jit, value::JitParameter};
use crate::{bytecode_interpreter::{InterpreterError, Interpreter, Binop, CallFrame}, bytecode, value::{self, MarieValue, PropertyKey, JitValue}, gc, foreign};

/// A collection of state used for translating from toy-language AST nodes
/// into Cranelift IR.
pub struct FunctionTranslator<'a> {
    pub val_type: types::Type,
    pub builder: FunctionBuilder<'a>,
    pub data_ctx: &'a mut DataContext,
    pub stack: &'a mut Vec<value::MarieValue>,
    pub output: &'a mut Vec<String>,
    pub module: &'a mut JITModule,
    pub frames: &'a mut Vec<CallFrame>,
    pub heap: &'a mut gc::Heap,
    pub upvalues: &'a mut Vec<Rc<RefCell<value::Upvalue>>>,
    pub entry_blocks: Vec<Block>,
    pub funcs: Vec<FuncRef>,
    pub is_done: bool,
    pub function_type: usize,
    pub globals: &'a mut HashMap<String, value::MarieValue>,
}

impl<'a> FunctionTranslator<'a> {
    pub fn run(&mut self) -> Result<(), InterpreterError> {
        self.define_bits_to_f64();
        self.define_f64_to_bits();
        self.define_print_jitval();
        self.define_is_f64();
        self.define_f64_to_jitval();
        self.define_string_to_jitval();
        self.define_print_string_jitval();
        self.define_marieval_to_jitval();
        self.define_make_err_val_type();
        self.define_printtest();
        self.define_marieval_to_jittype();

        loop {
            if self.is_done() {
                return Ok(());
            }

            if let Err(err) = self.step() {
                return Err(err);
            }
        }
    }

    fn step(&mut self) -> Result<(), InterpreterError> {
        let op = self.next_op_and_advance();

        // TODO Garbage collector
        // if self.heap.should_collect() {
        //     self.collect_garbage();
        // }

        match op {
            (bytecode::Op::Return, lineno) => {
                let result = self.pop_stack();
                if value::type_id_of(&result.val) != self.function_type {
                    return Err(InterpreterError::Runtime(format!(
                        "This function was expected to return value type of {}, but it is returning {} (line={})",
                        value::type_id_to_string(self.function_type),
                        value::type_of(&result.val),
                        lineno.value
                    )))
                }

                for idx in self.frame().slots_offset..self.stack.len() {
                    self.close_upvalues(idx);
                }
                self.frames.pop();

                let mut val = self.get_jit_value(&result);
                let val_type = self.builder.func.dfg.value_type(val);
                let is_float = val_type.is_float(); 
                if is_float {
                    val = self.call_f64_to_jitval(val);
                } else {
                    val = self.call_string_to_jitval(val);
                }

                // Emit the return instruction.
                self.builder.ins().return_(&[val]);

                // Tell the builder we're done with this function.
                self.builder.finalize();
                
                self.entry_blocks.clear();

                self.is_done = true;
            }
            (bytecode::Op::Constant(idx), _) => {
                let constant = self.read_constant(idx);
                match constant {
                    value::Value::Number(v) => {
                        self.stack.push(
                            MarieValue { 
                                is_public: true,
                                is_mutable: true,
                                val: constant,
                                jit_value: Some(JitValue::Value(self.builder.ins().f64const(v))),
                            }
                        );
                    },
                    value::Value::String(v) => {
                        self.stack.push(
                            MarieValue { 
                                is_public: true,
                                is_mutable: true,
                                val: constant,
                                jit_value: Some(JitValue::Value(self.builder.ins().iconst(types::I64,v as i64))),
                            }
                        );
                    },
                    _ => {
                        self.stack.push(
                            MarieValue { 
                                is_public: true,
                                is_mutable: true,
                                val: constant,
                                jit_value: Some(JitValue::Value(self.builder.ins().iconst(types::I64, -1))),
                            }
                        );
                    }
                    // value::Value::Bool(v) => {},
                    // value::Value::Function(v) => {},
                    // value::Value::Instance(v) => {},
                    // value::Value::BoundMethod(v) => {},
                    // value::Value::Class(v) => {},
                    // value::Value::NativeFunction(v) => {},
                    // value::Value::Nil => {},
                    // value::Value::List(v) => {},
                }
            }
            (bytecode::Op::Nil, _) => {
                self.stack.push(
                    MarieValue {
                        is_public: true,
                        is_mutable: true,
                        val: value::Value::Nil,
                        jit_value: Some(JitValue::Value(self.builder.ins().iconst(types::I64, -1))),
                    }
                );
            }
            (bytecode::Op::Add, lineno) => {
                let marie_val1 = self.pop_stack();
                let marie_val2 = self.pop_stack();
                
                let mut val1 = marie_val1.val;
                let mut val2 = marie_val2.val;

                let jit_value1 = self.to_jit_value(marie_val1.jit_value.unwrap());
                let jit_value2 = self.to_jit_value(marie_val2.jit_value.unwrap());

                match (&mut val1, &mut val2) {
                    (value::Value::Number(num1), value::Value::Number(num2)) => {
                        let ans = self.builder.ins().fadd(jit_value2, jit_value1);
                        self.stack
                            .push(
                                MarieValue {
                                    is_mutable: true,
                                    is_public: true,
                                    val: value::Value::Number(*num2 + *num1), // note the order!
                                    jit_value: Some(JitValue::Value(ans)),
                                }
                            );
                    }
                    (value::Value::String(_), value::Value::Number(_)) => {
                        //self.numeric_binop(Binop::Add, lineno)?
                    }
                    (value::Value::Number(_), value::Value::String(_)) => {
                        //self.numeric_binop(Binop::Add, lineno)?
                    }
                    (value::Value::String(_), value::Value::String(_)) => {
                        //self.numeric_binop(Binop::Add, lineno)?
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
            (bytecode::Op::Subtract, lineno) => {
                let marie_val1 = self.pop_stack();
                let marie_val2 = self.pop_stack();
                
                let mut val1 = marie_val1.val;
                let mut val2 = marie_val2.val;

                let jit_value1 = self.to_jit_value(marie_val1.jit_value.unwrap());
                let jit_value2 = self.to_jit_value(marie_val2.jit_value.unwrap());

                match (&mut val1, &mut val2) {
                    (value::Value::Number(num1), value::Value::Number(num2)) => {
                        let ans = self.builder.ins().fsub(jit_value2, jit_value1);
                        self.stack
                            .push(
                                MarieValue {
                                    is_mutable: true,
                                    is_public: true,
                                    val: value::Value::Number(*num2 - *num1), // note the order!
                                    jit_value: Some(JitValue::Value(ans)),
                                }
                            );
                    }
                    (value::Value::String(_), value::Value::Number(_)) => {
                        //self.numeric_binop(Binop::Add, lineno)?
                    }
                    (value::Value::Number(_), value::Value::String(_)) => {
                        //self.numeric_binop(Binop::Add, lineno)?
                    }
                    (value::Value::String(_), value::Value::String(_)) => {
                        //self.numeric_binop(Binop::Add, lineno)?
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
            (bytecode::Op::Multiply, lineno) => {
                let marie_val1 = self.pop_stack();
                let marie_val2 = self.pop_stack();
                
                let mut val1 = marie_val1.val;
                let mut val2 = marie_val2.val;

                let jit_value1 = self.to_jit_value(marie_val1.jit_value.unwrap());
                let jit_value2 = self.to_jit_value(marie_val2.jit_value.unwrap());

                match (&mut val1, &mut val2) {
                    (value::Value::Number(num1), value::Value::Number(num2)) => {
                        let ans = self.builder.ins().fmul(jit_value2, jit_value1);
                        self.stack
                            .push(
                                MarieValue {
                                    is_mutable: true,
                                    is_public: true,
                                    val: value::Value::Number(*num2 * *num1), // note the order!
                                    jit_value: Some(JitValue::Value(ans)),
                                }
                            );
                    }
                    (value::Value::String(_), value::Value::Number(_)) => {
                        //self.numeric_binop(Binop::Add, lineno)?
                    }
                    (value::Value::Number(_), value::Value::String(_)) => {
                        //self.numeric_binop(Binop::Add, lineno)?
                    }
                    (value::Value::String(_), value::Value::String(_)) => {
                        //self.numeric_binop(Binop::Add, lineno)?
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
            },
            (bytecode::Op::Divide, lineno) => {
                let marie_val1 = self.pop_stack();
                let marie_val2 = self.pop_stack();
                
                let mut val1 = marie_val1.val;
                let mut val2 = marie_val2.val;

                let jit_value1 = self.to_jit_value(marie_val1.jit_value.unwrap());
                let jit_value2 = self.to_jit_value(marie_val2.jit_value.unwrap());

                match (&mut val1, &mut val2) {
                    (value::Value::Number(num1), value::Value::Number(num2)) => {
                        let ans = self.builder.ins().fdiv(jit_value2, jit_value1);
                        self.stack
                            .push(
                                MarieValue {
                                    is_mutable: true,
                                    is_public: true,
                                    val: value::Value::Number(*num2 / *num1), // note the order!
                                    jit_value: Some(JitValue::Value(ans)),
                                }
                            );
                    }
                    (value::Value::String(_), value::Value::Number(_)) => {
                        //self.numeric_binop(Binop::Add, lineno)?
                    }
                    (value::Value::Number(_), value::Value::String(_)) => {
                        //self.numeric_binop(Binop::Add, lineno)?
                    }
                    (value::Value::String(_), value::Value::String(_)) => {
                        //self.numeric_binop(Binop::Add, lineno)?
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
            },
            (bytecode::Op::Print, _) => {
                let mut to_print = self.peek().clone();
                match to_print.val {
                    value::Value::Number(_) => {
                        self.num_print_val(&to_print);
                    },
                    value::Value::String(_) => {
                        self.string_print_val(&mut to_print);
                    }
                    _ => {}
                }
                self.pop_stack();
            }
            (bytecode::Op::Pop, _) => {
                self.pop_stack();
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
            (bytecode::Op::DefineParamLocal(is_mutable, parameter_type, idx), lineno) => {
                /*
                型がわからない値はデコード方法がそもそも不明（f64としてデコードするのかStringとしてデコードするのか）
                そのため型がわからない値の型の情報を取るために事前にデコードすることは不可能。
                つまりusize(i64)の数字の羅列からその値がもとはどんな型なのかを取得しかないがそんなことは不可能。
                数値の頭に型を示す数値を挿入したりするとf64のビット列がi64の数値に入り切らなくなる。
                MaeriValueごと一律heapのアドレスにしてi64にしておくって一律MarieValue型として復元する場合、
                String idからStringを復元することができない。
                
                MarieValueじゃない専用のstructをつくってstringをidにせずそのままわたせばいけるかもしれない。

                そうしない場合はjit world側で型の判断をするのはそもそも不可能。
                型の判断とは関係ないRun timeエラーなら投げられるかも。
                */
                let slots_offset = self.frame().slots_offset;
                let mut arg_val = self.stack[slots_offset + idx - 1].clone();

                let entry_block = self.entry_blocks.last().unwrap();
                let parameter_val = self.builder.block_params(*entry_block)[idx - 1];
                let val = self.call_marieval_to_jitval(parameter_val);
                let jitval = val.0;
                let jittype = val.1;
                match parameter_type {
                    1 => { // Number
                        let f64val =  self.call_bits_to_f64(jitval);
                        let variable = Variable::new(slots_offset + idx - 1);
                        self.builder.declare_var(variable, types::F64);
                        self.builder.def_var(variable, f64val);
                        arg_val.jit_value = Some(JitValue::Variable(variable));
                    },
                    2 => {}, // Bool
                    3 => { // String
                        let variable = Variable::new(slots_offset + idx - 1);
                        self.builder.declare_var(variable, types::I64);
                        self.builder.def_var(variable, jitval);
                        arg_val.jit_value = Some(JitValue::Variable(variable));
                    },
                    4 => {}, // Funcation
                    8 => {}, // Instance
                    9 => {}, // Nil
                    10 => {}, // List
                    // value::Value::Err(_) => panic!("Unexpected value type."),
                    _ => panic!("Unexpected value type."),
                }

                let expected_type = self.builder.ins().iconst(types::I64, parameter_type as i64);
                let condition_value = self.builder.ins().icmp(IntCC::NotEqual, jittype, expected_type);

                let then_block = self.builder.create_block();
                let merge_block = self.builder.create_block();
                // Test the if condition and conditionally branch.
                self.builder.ins().brz(condition_value, merge_block, &[]);
                // Fall through to then block.
                self.builder.ins().jump(then_block, &[]);
        
                self.builder.switch_to_block(then_block);
                self.builder.seal_block(then_block);
                let val = self.call_make_err_val_type(expected_type, jittype);
                self.builder.ins().return_(&[val]);

                // Switch to the merge block for subsequent statements.
                self.builder.switch_to_block(merge_block);
        
                // We've now seen all the predecessors of the merge block.
                self.builder.seal_block(merge_block);
        
                // Read the value of the if-else by reading the merge block
                // parameter.
                self.builder.block_params(merge_block);

                arg_val.is_mutable = is_mutable;
                let slots_offset = self.frame().slots_offset;
                self.stack[slots_offset + idx - 1] = arg_val;
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
                self.stack[slots_offset + idx - 1] = val;
            }
            (bytecode::Op::Call(arg_count), _) => {
                let a = self.call_value(self.peek_by(arg_count.into()).clone(), arg_count);
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
            _ => {
                println!("{:?}", op)
            }
        }
        Ok(())
    }

    pub fn call_value(
        &mut self,
        val_to_call: MarieValue,
        arg_count: u8,
    ) -> Result<(), InterpreterError> {
        match val_to_call.val {
            value::Value::Function(closure_handle) => {
                let closure = self.get_closure(closure_handle).clone();
                let func = &closure.function;
                if arg_count != func.arity {
                    return Err(InterpreterError::Runtime(format!(
                        "Expected {} arguments but found {}.",
                        func.arity, arg_count
                    )));
                }


                //-------------
                let arg_count = closure.function.arity;
                let mut arguments = Vec::new();
                let mut jit = jit::JIT::default();
                for i in 0..arg_count {
                    jit.ctx.func.signature.params.push(AbiParam::new(types::I64));
                    let arg = self.peek_by(i as usize);
                    match arg.val {
                        value::Value::Number(arg_val) => {
                            let a = JitParameter {
                                value: arg_val.to_bits() as i64,
                                value_type: 1,
                            };
                            arguments.push(self.builder.ins().iconst(types::I64,Box::into_raw(Box::new(a)) as i64));
                        }
                        value::Value::String(string_id) => {
                            let string_arg = self.get_str(string_id);
                            let a = JitParameter {
                                value: Box::into_raw(Box::new(string_arg.to_string())) as i64,
                                value_type: 3,
                            };
                            arguments.push(self.builder.ins().iconst(types::I64,Box::into_raw(Box::new(a)) as i64));
                        }
                        _ => {
                            println!("wrong type of argument!!! {}", arg.val);
                        }
                    }
                }
                arguments.reverse();
                // Our language currently only supports one return value, though
                // Cranelift is designed to support more.
                jit.ctx.func.signature.returns.push(AbiParam::new(types::I64));

                let mut builder = FunctionBuilder::new(&mut jit.ctx.func, &mut jit.builder_context);

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

                // prepare_callの内容--
                self.frames.push(CallFrame::default());
                let mut frame = self.frames.last_mut().unwrap();
                frame.closure = closure.clone();
                frame.slots_offset = self.stack.len() - usize::from(arg_count);
                frame.invoked_method_id = Some(closure_handle);
                // ---

                let mut entry_blocks = Vec::new();
                entry_blocks.push(entry_block);

                // Now translate the statements of the function body.
                let mut trans = FunctionTranslator {
                    val_type: types::I64,
                    builder,
                    data_ctx: &mut self.data_ctx,
                    stack: &mut self.stack,
                    output: &mut self.output,
                    module: &mut self.module,
                    frames: &mut self.frames,
                    heap: &mut self.heap,
                    upvalues: &mut self.upvalues,
                    entry_blocks,
                    is_done: false,
                    funcs: Vec::new(),
                    function_type: closure.function_type,
                    globals: &mut self.globals,
                };
                trans.run()?;

                // Next, declare the function to jit. Functions must be declared
                // before they can be called, or defined.
                let func_name = closure.function.name.clone();
                let maybe_func_id = self
                    .module
                    .declare_function(&func_name, Linkage::Export, &jit.ctx.func.signature)
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
                        &mut jit.ctx,
                    )
                    .map_err(|e| e.to_string());

                match jitresult {
                    Ok(_) => {},
                    Err(err) => {
                        panic!("{}", err);
                    }
                };

                //-------------

                // Now that compilation is finished, we can clear out the context state.
                self.module.clear_context(&mut jit.ctx);

                // Finalize the functions which we just defined, which resolves any
                // outstanding relocations (patching in addresses, now that they're
                // available).
                self.module.finalize_definitions();

                // We can now retrieve a pointer to the machine code.
                // let fn_code = self.module.get_finalized_function(funcid);
                
                let funcref = self.module.declare_func_in_func(funcid, &mut self.builder.func);
                let call = self.builder.ins().call(funcref, &arguments);
                let result = self.builder.inst_results(call)[0];

                self.stack
                    .push(
                        MarieValue {
                            is_mutable: true,
                            is_public: true,
                            val: value::Value::Nil, // TODO
                            jit_value: Some(JitValue::Value(result)),
                        }
                    );
                Ok(())
            }
            value::Value::NativeFunction(native_func) => {
                //self.call_native_func(native_func, arg_count)?;
                Ok(())
            }
            value::Value::BoundMethod(method_id) => {
                //self.call_bound_method(method_id, arg_count)?;
                Ok(())
            }
            _ => Err(InterpreterError::Runtime(format!(
                "attempted to call non-callable value of type: {:?}.",
                value::type_of(&val_to_call.val),
            ))),
        }
    }

    pub fn get_str(&self, str_handle: gc::HeapId) -> &String {
        self.heap.get_str(str_handle)
    }

    /*
    Set up a few call frame so that on the next interpreter step we'll start executing code inside the function.
     */
    pub fn prepare_call(
        &mut self,
        closure_handle: gc::HeapId,
        arg_count: u8,
    ) -> Result<(), InterpreterError> {
        let closure = self.get_closure(closure_handle).clone();
        let func = &closure.function;
        if arg_count != func.arity {
            return Err(InterpreterError::Runtime(format!(
                "Expected {} arguments but found {}.",
                func.arity, arg_count
            )));
        }

        self.frames.push(CallFrame::default());
        let mut frame = self.frames.last_mut().unwrap();
        frame.closure = closure;
        frame.slots_offset = self.stack.len() - usize::from(arg_count);
        frame.invoked_method_id = Some(closure_handle);
        Ok(())
    }

    pub fn get_closure(&self, closure_handle: gc::HeapId) -> &value::Closure {
        self.heap.get_closure(closure_handle)
    }

    pub fn get_jit_value(&mut self, val: &MarieValue) -> Value {
        if let Some(jit_value) = val.jit_value {
            match jit_value {
                value::JitValue::Value(val) => val,
                value::JitValue::Variable(variable) => {
                    self.builder.use_var(variable)
                },
            }
        } else {
            self.builder.ins().iconst(types::I64, -1)
        }
    }

    pub fn to_jit_value(&mut self, jit_value: JitValue) -> Value {
        match jit_value {
            value::JitValue::Value(val) => val,
            value::JitValue::Variable(variable) => {
                self.builder.use_var(variable)
            },
        }
    }

    pub fn string_print_val(&mut self, val: &mut MarieValue) {
        // TODO メモリリーク
        //let output = self.format_val(val);
        let jit_value = self.get_jit_value(val);
        val.jit_value = Some(value::JitValue::Value(self.call_print_string_jitval(jit_value)));
        //self.output.push(output);
    }

    pub fn num_print_val(&mut self, val: &MarieValue) {
        //let output = self.format_val(val);
        let jit_value = self.get_jit_value(val);
        self.call_print_jitval(jit_value);
        //self.output.push(output);
    }

    pub fn pop_stack(&mut self) -> value::MarieValue {
        match self.stack.pop() {
            Some(val) => val,
            None => panic!("attempted to pop empty stack!"),
        }
    }

    pub fn pop_stack_n_times(&mut self, num_to_pop: usize) {
        for _ in 0..num_to_pop {
            self.pop_stack();
        }
    }

    pub fn close_upvalues(&mut self, index: usize) {
        let value = &self.stack[index];
        for upval in self.upvalues.into_iter() {
            if upval.borrow().is_open_with_index(index) {
                upval.replace(value::Upvalue::Closed(value.clone().val));
            }
        }

        self.upvalues.retain(|u| u.borrow().is_open());
    }

    pub fn is_done(&self) -> bool {
        self.is_done || self.frames.is_empty() || self.frame().ip >= self.frame().closure.function.chunk.code.len()
    }

    pub fn next_op_and_advance(&mut self) -> (bytecode::Op, bytecode::Lineno) {
        self.frame_mut().next_op_and_advance()
    }

    pub fn frame_mut(&mut self) -> &mut CallFrame {
        let frames_len = self.frames.len();
        &mut self.frames[frames_len - 1]
    }

    pub fn maybe_frame(&self) -> Option<&CallFrame> {
        self.frames.last()
    }

    pub fn frame(&self) -> &CallFrame {
        self.maybe_frame().unwrap()
    }

    pub fn peek(&self) -> &value::MarieValue {
        self.peek_by(0)
    }

    pub fn peek_by(&self, n: usize) -> &value::MarieValue {
        &self.stack[self.stack.len() - n - 1]
    }

    pub fn read_constant(&mut self, idx: usize) -> value::Value {
        let constant = self.frame().read_constant(idx);
        match constant {
            bytecode::Constant::Number(num) => value::Value::Number(num),
            bytecode::Constant::String(s) => value::Value::String(self.heap.manage_str(s)),
            bytecode::Constant::Function(f) => {
                value::Value::Function(self.heap.manage_closure(value::Closure {
                    function: f.function,
                    upvalues: Vec::new(),
                    is_compiled: true,
                    use_compiled: false,
                    function_type: 0,
                }))
            }
        }
    }

    fn define_bits_to_f64(&mut self) {
        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::F64));
    
        let callee = self.module
            .declare_function("bits_to_f64", cranelift_module::Linkage::Import, &sig)
            .map_err(|e| e.to_string()).unwrap();
    
        let local_callee = self.module
            .declare_func_in_func(callee, &mut self.builder.func);
    
        self.funcs.push(local_callee);
    } 

    fn call_bits_to_f64(&mut self, number: cranelift::prelude::Value) -> cranelift::prelude::Value {
        let args = vec![number];
    
        let local_callee = self.funcs[0];
        let call = self.builder.ins().call(local_callee, &args);
        self.builder.inst_results(call)[0]
    } 

    fn define_f64_to_bits(&mut self) {
        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(types::F64));
        sig.returns.push(AbiParam::new(types::I64));
    
        let callee = self.module
            .declare_function("f64_to_bits", cranelift_module::Linkage::Import, &sig)
            .map_err(|e| e.to_string()).unwrap();
    
        let local_callee = self.module
            .declare_func_in_func(callee, &mut self.builder.func);

        self.funcs.push(local_callee);
    } 

    fn call_f64_to_bits(&mut self, number: cranelift::prelude::Value) -> cranelift::prelude::Value {
        let local_callee = self.funcs[1];
    
        let args = vec![number];
    
        let call = self.builder.ins().call(local_callee, &args);
        self.builder.inst_results(call)[0]
    }

    fn define_print_jitval(&mut self) {
        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(types::F64));
    
        let callee = self.module
            .declare_function("print_jitval", cranelift_module::Linkage::Import, &sig)
            .map_err(|e| e.to_string()).unwrap();
    
        let local_callee = self.module
            .declare_func_in_func(callee, &mut self.builder.func);
    
        self.funcs.push(local_callee);
    }

    fn call_print_jitval(
        &mut self,
        val1: cranelift::prelude::Value,
    ) {
        let local_callee = self.funcs[2];
    
        let args = vec![val1];
    
        let call = self.builder.ins().call(local_callee, &args);
        self.builder.inst_results(call);
    }

    fn define_is_f64(&mut self) {
        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::B1));
    
        let callee = self.module
            .declare_function("is_f64", cranelift_module::Linkage::Import, &sig)
            .map_err(|e| e.to_string()).unwrap();
    
        let local_callee = self.module
            .declare_func_in_func(callee, &mut self.builder.func);
    
        self.funcs.push(local_callee);
    }

    fn call_is_f64(
        &mut self,
        val1: cranelift::prelude::Value,
    ) -> cranelift::prelude::Value {
        let local_callee = self.funcs[3];
    
        let args = vec![val1];
    
        let call = self.builder.ins().call(local_callee, &args);
        let cmp = self.builder.inst_results(call)[0];
        self.builder.ins().bint(self.val_type, cmp)
    }

    fn define_f64_to_jitval(&mut self) {
        let mut sig = self.module.make_signature();

        sig.returns.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::F64));
        let callee = self.module
            .declare_function("f64_to_jit_val", cranelift_module::Linkage::Import, &sig)
            .map_err(|e| e.to_string()).unwrap();
    
        let local_callee = self.module
            .declare_func_in_func(callee, &mut self.builder.func);

        self.funcs.push(local_callee);
    }

    fn call_f64_to_jitval(&mut self, val1: cranelift::prelude::Value) -> cranelift::prelude::Value {
        let local_callee = self.funcs[4];

        let args = vec![val1];
    
        let call = self.builder.ins().call(local_callee, &args);
        self.builder.inst_results(call)[0]
    }

    fn define_string_to_jitval(&mut self) {
        let mut sig = self.module.make_signature();

        sig.returns.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        let callee = self.module
            .declare_function("string_to_jit_val", cranelift_module::Linkage::Import, &sig)
            .map_err(|e| e.to_string()).unwrap();
    
        let local_callee = self.module
            .declare_func_in_func(callee, &mut self.builder.func);

        self.funcs.push(local_callee);
    }

    fn call_string_to_jitval(&mut self, val1: cranelift::prelude::Value) -> cranelift::prelude::Value {
        let local_callee = self.funcs[5];

        let args = vec![val1];
    
        let call = self.builder.ins().call(local_callee, &args);
        self.builder.inst_results(call)[0]
    }

    fn define_print_string_jitval(&mut self) {
        let mut sig = self.module.make_signature();

        sig.returns.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        let callee = self.module
            .declare_function("print_string_jitval", cranelift_module::Linkage::Import, &sig)
            .map_err(|e| e.to_string()).unwrap();
    
        let local_callee = self.module
            .declare_func_in_func(callee, &mut self.builder.func);

        self.funcs.push(local_callee);
    }

    fn call_print_string_jitval(&mut self, val1: cranelift::prelude::Value) -> cranelift::prelude::Value {
        let local_callee = self.funcs[6];

        let args = vec![val1];
    
        let call = self.builder.ins().call(local_callee, &args);
        self.builder.inst_results(call)[0]
    }

    fn define_marieval_to_jitval(&mut self) {
        let mut sig = self.module.make_signature();

        sig.returns.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        let callee = self.module
            .declare_function("marieval_to_jitval", cranelift_module::Linkage::Import, &sig)
            .map_err(|e| e.to_string()).unwrap();
    
        let local_callee = self.module
            .declare_func_in_func(callee, &mut self.builder.func);

        self.funcs.push(local_callee);
    }

    fn call_marieval_to_jitval(
        &mut self,
        val1: cranelift::prelude::Value 
    ) -> (cranelift::prelude::Value, cranelift::prelude::Value)
    {
        let local_callee = self.funcs[7];

        let args = vec![val1];
    
        let call = self.builder.ins().call(local_callee, &args);
        (self.builder.inst_results(call)[0], self.builder.inst_results(call)[1])
    }

    fn define_make_err_val_type(&mut self) {
        let mut sig = self.module.make_signature();

        sig.returns.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        let callee = self.module
            .declare_function("make_err_val_type", cranelift_module::Linkage::Import, &sig)
            .map_err(|e| e.to_string()).unwrap();
    
        let local_callee = self.module
            .declare_func_in_func(callee, &mut self.builder.func);

        self.funcs.push(local_callee);
    }

    fn call_make_err_val_type(
        &mut self,
        val1: cranelift::prelude::Value,
        val2: cranelift::prelude::Value,
    ) -> cranelift::prelude::Value 
    {
        let local_callee = self.funcs[8];

        let args = vec![val1, val2];
    
        let call = self.builder.ins().call(local_callee, &args);
        self.builder.inst_results(call)[0]
    }

    fn define_printtest(&mut self) {
        let mut sig = self.module.make_signature();

        sig.returns.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        let callee = self.module
            .declare_function("printtest", cranelift_module::Linkage::Import, &sig)
            .map_err(|e| e.to_string()).unwrap();
    
        let local_callee = self.module
            .declare_func_in_func(callee, &mut self.builder.func);

        self.funcs.push(local_callee);
    }

    fn call_printtest(
        &mut self,
        val1: cranelift::prelude::Value,
    )
    {
        let local_callee = self.funcs[9];

        let args = vec![val1];
    
        let call = self.builder.ins().call(local_callee, &args);
        self.builder.inst_results(call);
    }

    fn define_marieval_to_jittype(&mut self) {
        let mut sig = self.module.make_signature();

        sig.returns.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        let callee = self.module
            .declare_function("marieval_to_jittype", cranelift_module::Linkage::Import, &sig)
            .map_err(|e| e.to_string()).unwrap();
    
        let local_callee = self.module
            .declare_func_in_func(callee, &mut self.builder.func);

        self.funcs.push(local_callee);
    }

    fn call_marieval_to_jittype(
        &mut self,
        val1: cranelift::prelude::Value,
    ) -> cranelift::prelude::Value 
    {
        let local_callee = self.funcs[10];

        let args = vec![val1];
    
        let call = self.builder.ins().call(local_callee, &args);
        self.builder.inst_results(call)[0]
    }
}
