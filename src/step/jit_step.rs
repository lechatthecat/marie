use core::slice;
use std::{cell::RefCell, rc::Rc, collections::HashMap, mem};

use cranelift::{prelude::{InstBuilder, AbiParam, Variable, EntityRef, types, FunctionBuilder, Block, StackSlotData, StackSlotKind}, codegen::ir::{FuncRef, ArgumentPurpose, immediates::Offset32}};
use cranelift_jit::JITModule;
use cranelift_module::{Linkage, Module, FuncId, DataContext};
use cranelift::prelude::Value;

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
    pub function_type: usize
}

impl<'a>  FunctionTranslator<'a> {
    pub fn run(&mut self) -> Result<(), InterpreterError> {
        self.define_bits_to_f64();
        self.define_f64_to_bits();
        self.define_print_jitval();
        self.define_is_f64();
        self.define_f64_to_jit_val();
        self.define_string_to_jit_val();

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

                if self.frames.len() <= 1 {
                    self.frames.pop();
                    return Ok(());
                }

                let num_to_pop = usize::from(self.frame().closure.function.locals_size) + 1;

                self.frames.pop();

                self.pop_stack_n_times(num_to_pop);

                self.stack.push(result.clone());

                let mut val = self.get_jit_value(&result);
                let val_type = self.builder.func.dfg.value_type(val);
                let is_float = val_type.is_float(); 
                if is_float {
                    val = self.call_f64_to_jit_val(val);
                } else {
                    val = self.call_string_to_jit_val(val);
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
                let to_print = self.peek().clone();
                self.print_val(&to_print);
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
                let slots_offset = self.frame().slots_offset;
                // if value::type_id_of(&val) != parameter_type {
                //     return Err(InterpreterError::Runtime(format!(
                //         "This parameter was expected to be value type of {}, but the given parameter's type is {} (line={})",
                //         value::type_id_to_string(parameter_type),
                //         value::type_of(&val),
                //         lineno.value
                //     )))
                // }
                let mut marie_val = MarieValue { 
                    is_public: true,
                    is_mutable: true,
                    val: value::Value::Nil,
                    jit_value: None,
                };
                match parameter_type {
                    1 => { // Number
                        let entry_block = self.entry_blocks.last().unwrap();
                        let val = self.builder.block_params(*entry_block)[idx - 1];
                        let f64val =  self.call_bits_to_f64(val);
                        let variable = Variable::new(slots_offset + idx - 1);
                        self.builder.declare_var(variable, types::F64);
                        self.builder.def_var(variable, f64val);
                        marie_val.val = value::Value::Number(0f64);
                        marie_val.jit_value = Some(JitValue::Variable(variable));
                    },
                    2 => {}, // Bool
                    3 => { // String
                        let entry_block = self.entry_blocks.last().unwrap();
                        let val = self.builder.block_params(*entry_block)[idx - 1];
                        let variable = Variable::new(slots_offset + idx - 1);
                        self.builder.declare_var(variable, types::I64);
                        self.builder.def_var(variable, val);
                        marie_val.val = value::Value::String(self.heap.manage_str("".to_string()));
                        marie_val.jit_value = Some(JitValue::Variable(variable));
                    },
                    4 => {}, // Funcation
                    8 => {}, // Instance
                    9 => {}, // Nil
                    10 => {}, // List
                    // value::Value::Err(_) => panic!("Unexpected value type."),
                    _ => panic!("Unexpected value type."),
                }
                marie_val.is_mutable = is_mutable;
                self.stack.push(marie_val);
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
            (bytecode::Op::Call(arg_count), _) => {
                let a = 1;
                //self.call_value(self.peek_by(arg_count.into()).clone(), arg_count)?;
            }
            _ => {}
        }
        Ok(())
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

    pub fn print_val(&mut self, val: &MarieValue) {
        // TODO
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

    fn define_f64_to_jit_val(&mut self) {
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

    fn call_f64_to_jit_val(&mut self, val1: cranelift::prelude::Value) -> cranelift::prelude::Value {
        let local_callee = self.funcs[4];

        let args = vec![val1];
    
        let call = self.builder.ins().call(local_callee, &args);
        self.builder.inst_results(call)[0]
    }

    fn define_string_to_jit_val(&mut self) {
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

    fn call_string_to_jit_val(&mut self, val1: cranelift::prelude::Value) -> cranelift::prelude::Value {
        let local_callee = self.funcs[5];

        let args = vec![val1];
    
        let call = self.builder.ins().call(local_callee, &args);
        self.builder.inst_results(call)[0]
    }

    fn define_test(&mut self) {
        let mut sig = self.module.make_signature();

        sig.returns.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        let callee = self.module
            .declare_function("test1", cranelift_module::Linkage::Import, &sig)
            .map_err(|e| e.to_string()).unwrap();
    
        let local_callee = self.module
            .declare_func_in_func(callee, &mut self.builder.func);

        self.funcs.push(local_callee);

        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(types::I64));
        //sig.returns.push(AbiParam::new(types::I64));
    
        let callee = self.module
            .declare_function("test2", cranelift_module::Linkage::Import, &sig)
            .map_err(|e| e.to_string()).unwrap();
    
        let local_callee = self.module
            .declare_func_in_func(callee, &mut self.builder.func);

        self.funcs.push(local_callee);

        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(types::I64));
        //sig.returns.push(AbiParam::new(types::I64));
    
        let callee = self.module
            .declare_function("test3", cranelift_module::Linkage::Import, &sig)
            .map_err(|e| e.to_string()).unwrap();
    
        let local_callee = self.module
            .declare_func_in_func(callee, &mut self.builder.func);

        self.funcs.push(local_callee);
    } 

    fn call_test(&mut self) {
        let aa =  MarieValue {
            is_public: false,
            is_mutable: false,
            val: value::Value::Number(1000f64),
            jit_value: None,
        };
        let a = Box::into_raw(Box::new(aa));
        let aaa = a as i64;

        let val1 = self.builder.ins().iconst(self.val_type, aaa);
        let local_callee = self.funcs[4];
    
        let args = vec![val1];
    
        let call = self.builder.ins().call(local_callee, &args);
        let new_pointer = self.builder.inst_results(call)[0];


        let local_callee = self.funcs[5];
    
        let args = vec![new_pointer];
    
        let call = self.builder.ins().call(local_callee, &args);
        self.builder.inst_results(call);

        let local_callee = self.funcs[6];
        let aa = "My name is Shu!".to_string(); 
        let a = Box::into_raw(Box::new(aa));
        let aaa = a as i64;
        let arg1 = self.builder.ins().iconst(self.val_type, aaa);
        let args = vec![arg1];
    
        let call = self.builder.ins().call(local_callee, &args);
        self.builder.inst_results(call);

        let a = 1;
    }
}
