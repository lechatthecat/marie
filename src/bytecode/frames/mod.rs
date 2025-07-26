pub mod call_frame;
use crate::bytecode::bytecode::{Chunk, Function, ValueMeta};
use crate::bytecode::frames::call_frame::CallFrame;
use crate::bytecode::jit::call_func_pointer::CallFuncPointer;
use crate::bytecode::jit::{unpack_meta, pack_meta};
use crate::bytecode::jit::jitcache::TagVec;

use crate::bytecode::values::value::CompileType;
use crate::{
    bytecode::{
        bytecode,
        bytecode_interpreter::{Interpreter, InterpreterError},
        values::value,
    },
    gc::gc,
};

impl Interpreter {
    pub fn frame(&self) -> &CallFrame {
        self.maybe_frame().unwrap()
    }

    pub fn frame_mut(&mut self) -> &mut CallFrame {
        let frames_len = self.frames.len();
        &mut self.frames[frames_len - 1]
    }

    pub fn maybe_frame(&self) -> Option<&CallFrame> {
        self.frames.last()
    }

    pub fn call_value(
        &mut self,
        val_to_call: value::Value,
        arg_count: u8,
    ) -> Result<(), InterpreterError> {
        match val_to_call {
            value::Value::Function(func) => {
                self.push_frame_prepare_call(func, arg_count)?;
                Ok(())
            }
            _ => Err(InterpreterError::Runtime(format!(
                "attempted to call non-callable value of type {}.",
                value::type_of(&val_to_call)
            ))),
        }
    }

    /*
    Set up a few call frame so that on the next interpreter step we'll start executing code inside the function.
     */
    fn push_frame_prepare_call(
        &mut self,
        closure_handle: gc::HeapId,
        arg_count: u8,
    ) -> Result<(), InterpreterError> {
        let closure = self.get_closure(closure_handle).clone();
        // for item in closure.function.chunk.code.clone() {
        //     println!("{:?}", item);
        // }
        let func: &Function = &closure.function;
        if arg_count != func.arity {
            return Err(InterpreterError::Runtime(format!(
                "Expected {} arguments but found {}.",
                func.arity, arg_count
            )));
        }
        let mut arguments = Vec::new();
        let mut argument_metas = Vec::new();
        for i in 0..arg_count {
            let arg = self.peek_by(i as usize);
            match arg {
                value::Value::Number(arg_val) => {
                    arguments.push(arg_val.to_bits() as i64);
                    argument_metas.push(pack_meta(self.peek_meta()));
                }
                value::Value::Bool(arg_val) => {
                    arguments.push(*arg_val as i64);
                    argument_metas.push(pack_meta(self.peek_meta()));
                }
                value::Value::String(string_id) => {
                    arguments.push(*string_id as i64);
                    argument_metas.push(pack_meta(self.peek_meta()));
                }
                _ => {
                    return Err(InterpreterError::Runtime(format!(
                        "wrong type of argument: {}",
                        arg
                    )));
                }
            }
        }

        let compiled = self.ensure_jit_compiled(func, closure_handle, &argument_metas)?;

        self.frames.push(CallFrame {
            closure,
            instruction_pointer: Default::default(),
            slots_offset: self.stack.len() - usize::from(arg_count),
            invoked_method_id: Some(closure_handle),
            is_include_file: false,
            is_function: true
        });

        if let (Some(ptr), compile_type) = compiled {
            match compile_type {
                CompileType::compiled => {
                    let closure = self.get_closure(closure_handle);
                    let num_to_pop = usize::from(arg_count) + usize::from(closure.function.locals_size)+1;
                    self.pop_stack_n_times(num_to_pop);
                    self.pop_stack_meta_n_times(num_to_pop);
                    self.frames.pop();
                    argument_metas.reverse(); //TODO  i think it is better to reverse the arg inside call native by asigining last ... 3, 2, 1, 0
                    arguments.reverse(); //TODO  i think it is better to reverse the arg inside call native by asigining last ... 3, 2, 1, 0
                    let returned = self.call_native(ptr, &arguments, &argument_metas)?;
                    match returned.1.value_type {
                        value::Type::Number => {
                            let num = f64::from_bits(returned.0 as u64);
                            self.stack.push(value::Value::Number(num));
                            self.stack_meta.push(returned.1);
                        },
                        value::Type::String => {
                            self.stack.push(value::Value::String(returned.0.try_into().unwrap()));
                            self.stack_meta.push(returned.1);
                        },
                        value::Type::Bool => todo!(),
                        value::Type::Function => todo!(),
                        value::Type::NativeFunction => todo!(),
                        value::Type::Class => todo!(),
                        value::Type::BoundMethod => todo!(),
                        value::Type::Instance => todo!(),
                        value::Type::Null => {
                            self.stack.push(value::Value::Null);
                            self.stack_meta.push(returned.1);
                        }
                        value::Type::List => todo!(),
                    }
                },
                CompileType::uncompiled => {
                    let closure = self.get_closure(closure_handle);
                    // The following is necessary to avoid executing the function codes (stored in "Closure")
                    // after compiling the function by cranlift. After compiling, we just need to run the compiled cranelift code
                    // along with "DefineParamLocal" and "Return".
                    let num_to_pop = usize::from(arg_count) + usize::from(closure.function.locals_size)+1;
                    self.pop_stack_n_times(num_to_pop);
                    self.pop_stack_meta_n_times(num_to_pop);
                    self.frames.pop();
                    argument_metas.reverse(); // TODO
                    arguments.reverse(); // TODO i think it is better to reverse the arg inside call native by asigining last ... 3, 2, 1, 0
                    let returned = self.call_native(ptr, &arguments, &argument_metas)?;
                    match returned.1.value_type {
                        value::Type::Number => {
                            let num = f64::from_bits(returned.0 as u64);
                            self.stack.push(value::Value::Number(num));
                            self.stack_meta.push(returned.1);
                        },
                        value::Type::String => {
                            self.stack.push(value::Value::String(returned.0.try_into().unwrap()));
                            self.stack_meta.push(returned.1);
                        },
                        value::Type::Bool => {
                            self.stack.push(value::Value::Bool(returned.0 == 1));
                            self.stack_meta.push(returned.1);
                        },
                        value::Type::Function => todo!(),
                        value::Type::NativeFunction => todo!(),
                        value::Type::Class => todo!(),
                        value::Type::BoundMethod => todo!(),
                        value::Type::Instance => todo!(),
                        value::Type::Null => {
                            self.stack.push(value::Value::Null);
                            self.stack_meta.push(returned.1);
                        },
                        value::Type::List => todo!(),
                    }
                },
                CompileType::wontcompile => unreachable!()

            }
        }

        Ok(())
    }

    pub fn ensure_jit_compiled (
        &mut self,
        func: &Function,
        closure_handle: gc::HeapId,
        argument_metas: &Vec<u8>
    ) -> Result<(Option<*const u8>, CompileType), InterpreterError> {
        let tag_vec: TagVec = argument_metas.iter().copied().collect();
        let count = {
            let count = self.compiled.inc_hot(closure_handle, &tag_vec);
            count
        };

        if let Some(ptr) = self.compiled.get(closure_handle, &tag_vec) {
            Ok((Some(ptr), CompileType::compiled))
        } else if count >= 0 {  // 0回呼ばれたら JIT する例 When this function is dropped by GC, for debugging
            // I think hot couter and compied function must be dropped by gc too
            let ptr = self.jit_compile(closure_handle, &func.chunk.clone(), func.arity, argument_metas)?;
            Ok((Some(ptr), CompileType::uncompiled))
        } else {
            Ok((None, CompileType::wontcompile))
        }
    }
    
    pub fn call_native(&mut self, ptr: *const u8, arguments: &[i64], argument_meta: &[u8]) -> Result<(i64, ValueMeta), InterpreterError> {
        // Provide the correct arguments for call_func_pointer (update these as needed)
        // For example, if it expects (ptr: *const u8, arg: i64), provide appropriate values
        let ret = self.call_func_pointer(ptr, arguments, argument_meta);
        match ret {
            Ok(v) => {
                let meta = unpack_meta(v.meta_byte);
                Ok((v.value_bits, meta))
            }
            Err(e) => {
                Err(InterpreterError::Runtime(e))
            }
        }
    }

    fn jit_compile(&mut self, func_id: gc::HeapId, chunk: &Chunk, arity: u8, argument_metas: &Vec<u8>) -> Result<*const u8, InterpreterError> {
        let tag_vec: TagVec = argument_metas.iter().copied().collect();
        let name  = format!("fn_{func_id}_{tag_vec}");
        let slots_offset = self.frame().slots_offset;
        let id  = self.jit.compile_chunk(
            func_id,
            &name,
            arity,
            &chunk,
            slots_offset,
            &mut self.stack,
            &mut self.stack_meta,
            &mut self.heap,
        )?;

        let tag_vec: TagVec = argument_metas.iter().copied().collect();
        
        let ptr = self.jit.module.get_finalized_function(id);
        self.compiled.insert(func_id, tag_vec, ptr);
        Ok(ptr)
    }

    pub fn _is_constants_empty(&self) -> bool {
        self.frame()._is_constants_empty()
    }
}
