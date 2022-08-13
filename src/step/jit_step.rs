use core::slice;
use std::{cell::RefCell, rc::Rc, collections::HashMap, mem};

use cranelift::prelude::{InstBuilder, AbiParam, Variable, EntityRef, types, FunctionBuilder, Block};
use cranelift_jit::JITModule;
use cranelift_module::{Linkage, Module};
use cranelift::prelude::Value;

use crate::{bytecode_interpreter::{InterpreterError, Interpreter, Binop, CallFrame}, bytecode, value::{self, MarieValue, PropertyKey, JitValue}, gc, foreign};

/// A collection of state used for translating from toy-language AST nodes
/// into Cranelift IR.
pub struct FunctionTranslator<'a> {
    pub val_type: types::Type,
    pub builder: FunctionBuilder<'a>,
    pub stack: &'a mut Vec<value::MarieValue>,
    pub output: &'a mut Vec<String>,
    pub module: &'a mut JITModule,
    pub frames: &'a mut Vec<CallFrame>,
    pub heap: &'a mut gc::Heap,
    pub upvalues: &'a mut Vec<Rc<RefCell<value::Upvalue>>>,
    pub entry_blocks: Vec<Block>
}

impl<'a>  FunctionTranslator<'a> {
    pub fn run(&mut self) -> Result<(), InterpreterError> {
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

        // maybe not necessary??
        // if self.heap.should_collect() {
        //     self.collect_garbage();
        // }

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

                let mut val = self.get_jit_value(&result);
                val = self.f64_to_bits(val);

                // Emit the return instruction.
                self.builder.ins().return_(&[val]);

                // Tell the builder we're done with this function.
                self.builder.finalize();
                
                self.entry_blocks.clear();
                
            }
            (bytecode::Op::Constant(idx), _) => {
                let constant = self.read_constant(idx);
                self.stack.push(
                    MarieValue { 
                        is_public: true,
                        is_mutable: true,
                        val: constant,
                        jit_value: Some(JitValue::Value(self.builder.ins().iconst(types::I64, 0))),
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
                    }
                );
            }
            (bytecode::Op::Add, lineno) => {
                let marie_val1 = self.pop_stack();
                let marie_val2 = self.pop_stack();
                
                let mut val1 = marie_val1.val;
                let mut val2 = marie_val2.val;

                match (&mut val1, &mut val2) {
                    (value::Value::Number(num1), value::Value::Number(num2)) => {
                        let jit_value1 = self.to_jit_value(marie_val1.jit_value.unwrap());
                        let jit_value2 = self.to_jit_value(marie_val2.jit_value.unwrap());
                        self.print_jitval(jit_value1);
                        self.print_jitval(jit_value2);
                        let added = self.builder.ins().fadd(jit_value2, jit_value1);
                        self.stack
                            .push(
                                MarieValue {
                                    is_mutable: true,
                                    is_public: true,
                                    val: value::Value::Number(*num2 + *num1), // note the order!
                                    jit_value: Some(JitValue::Value(added))
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
            (bytecode::Op::DefineParamLocal(is_mutable, idx), _) => {
                let slots_offset = self.frame().slots_offset;
                let mut old_val = self.stack[slots_offset + idx - 1].clone();
                let val = old_val.val.clone();
                match val {
                    value::Value::Number(v) => {
                        let entry_block = self.entry_blocks.last().unwrap();
                        let val = self.builder.block_params(*entry_block)[idx - 1];
                        let f64val =  self.bits_to_f64(val);
                        let variable = Variable::new(slots_offset + idx - 1);
                        self.builder.declare_var(variable, types::F64);
                        self.builder.def_var(variable, f64val);
                        old_val.jit_value = Some(JitValue::Variable(variable));
                    },
                    value::Value::Bool(v) => {},
                    value::Value::String(v) => {},
                    value::Value::Function(v) => {
                        let a = v;
                    },
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
            (bytecode::Op::Call(arg_count), _) => {
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
            self.builder.ins().iconst(types::I64, 0)
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
        self.print_jitval(jit_value);
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
        self.frames.is_empty() || self.frame().ip >= self.frame().closure.function.chunk.code.len()
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
    
    pub fn bits_to_f64(&mut self, number: cranelift::prelude::Value) -> cranelift::prelude::Value {
        // TODO
        // 関数のDefineとCallを分ける
        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::F64));
    
        let callee = self.module
            .declare_function("bits_to_f64", cranelift_module::Linkage::Import, &sig)
            .map_err(|e| e.to_string()).unwrap();
    
        let local_callee = self.module
            .declare_func_in_func(callee, &mut self.builder.func);
    
        let args = vec![number];
    
        let call = self.builder.ins().call(local_callee, &args);
        self.builder.inst_results(call)[0]
    } 

    pub fn f64_to_bits(&mut self, number: cranelift::prelude::Value) -> cranelift::prelude::Value {
        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(types::F64));
        sig.returns.push(AbiParam::new(types::I64));
    
        let callee = self.module
            .declare_function("f64_to_bits", cranelift_module::Linkage::Import, &sig)
            .map_err(|e| e.to_string()).unwrap();
    
        let local_callee = self.module
            .declare_func_in_func(callee, &mut self.builder.func);
    
        let args = vec![number];
    
        let call = self.builder.ins().call(local_callee, &args);
        self.builder.inst_results(call)[0]
    } 

    pub fn test(&mut self, val: cranelift::prelude::Value) -> cranelift::prelude::Value {
        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::F64));
    
        let callee = self.module
            .declare_function("test", cranelift_module::Linkage::Import, &sig)
            .map_err(|e| e.to_string()).unwrap();
    
        let local_callee = self.module
            .declare_func_in_func(callee, &mut self.builder.func);
    
        let args = vec![val];
    
        let call = self.builder.ins().call(local_callee, &args);
        self.builder.inst_results(call)[0]
    } 

    pub fn try_add(
        &mut self,
        val1: cranelift::prelude::Value,
        val2: cranelift::prelude::Value,
        val3: cranelift::prelude::Value,
    ) -> Value {
        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(self.module.target_config().pointer_type()));
        sig.params.push(AbiParam::new(self.module.target_config().pointer_type()));
        sig.params.push(AbiParam::new(self.module.target_config().pointer_type()));
        sig.returns.push(AbiParam::new(self.module.target_config().pointer_type()));
    
        let callee = self.module
            .declare_function("try_add", cranelift_module::Linkage::Import, &sig)
            .map_err(|e| e.to_string()).unwrap();
    
        let local_callee = self.module
            .declare_func_in_func(callee, &mut self.builder.func);
    
        let args = vec![val1, val2, val3];
    
        let call = self.builder.ins().call(local_callee, &args);
        self.builder.inst_results(call)[0]
    }

    pub fn print_jitval(
        &mut self,
        val1: cranelift::prelude::Value,
    ) {
        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(types::F64));
    
        let callee = self.module
            .declare_function("print_jitval", cranelift_module::Linkage::Import, &sig)
            .map_err(|e| e.to_string()).unwrap();
    
        let local_callee = self.module
            .declare_func_in_func(callee, &mut self.builder.func);
    
        let args = vec![val1];
    
        let call = self.builder.ins().call(local_callee, &args);
        self.builder.inst_results(call);
    }

    pub fn ref_to_jitval (&mut self, val: &mut value::Value) -> Value {
        let boxed = unsafe { value::any_as_u8_slice(val) };
        self.builder.ins().iconst(self.module.target_config().pointer_type(), boxed.as_mut_ptr() as *mut MarieValue as i64)
    }
}
