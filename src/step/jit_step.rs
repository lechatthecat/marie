use core::slice;
use std::{cell::RefCell, rc::Rc, collections::HashMap, mem};

use cranelift::{prelude::{InstBuilder, AbiParam, Variable, EntityRef, types, FunctionBuilder, Block, StackSlotData, StackSlotKind, IntCC, FunctionBuilderContext, FloatCC, Signature}, codegen::{ir::{FuncRef, ArgumentPurpose, immediates::Offset32}, self}};
use cranelift_jit::JITModule;
use cranelift_module::{Linkage, Module, FuncId, DataContext};
use cranelift::prelude::Value;
use crate::{jit, value::JitParameter, bytecode::JumpType, compiler::FunctionType};
use crate::{bytecode_interpreter::{InterpreterError, Interpreter, Binop, CallFrame}, bytecode, value::{self, MarieValue, PropertyKey, JitValue}, gc, foreign};

#[derive(Clone, Copy)]
pub struct IfElse {
    block: Block,
    end_block: Block,
}

/// A collection of state used for translating from toy-language AST nodes
/// into Cranelift IR.
pub struct FunctionTranslator<'a> {
    pub my_func_ref: FuncRef,
    pub my_func_name: String,
    pub val_type: types::Type,
    pub builder: FunctionBuilder<'a>,
    pub data_ctx: &'a mut DataContext,
    pub func_stack: &'a mut Vec<value::MarieValue>,
    pub output: &'a mut Vec<String>,
    pub module: &'a mut JITModule,
    pub frames: &'a mut Vec<CallFrame>,
    pub heap: &'a mut gc::Heap,
    pub upvalues: &'a mut Vec<Rc<RefCell<value::Upvalue>>>,
    pub funcs: Vec<FuncRef>,
    pub entry_blocks: Vec<Block>,
    pub elseifs: Vec<IfElse>,
    pub is_done: bool,
    pub is_returned: bool,
    pub is_in_if: bool,
    pub function_type: usize,
    pub globals: &'a mut HashMap<String, value::MarieValue>,
    pub is_initializer : bool 
}

impl<'a> FunctionTranslator<'a> {
    pub fn run(&mut self) -> Result<(), InterpreterError> {
        self.define_bits_to_f64();
        self.define_print_jitval();
        self.define_f64_to_jitval();
        self.define_string_to_jitval();
        self.define_print_string_jitval();
        self.define_printtest();
        self.define_print_bool_jitval();
        self.define_i64_to_bool();
        self.define_nil_to_jitval();
        self.define_bool_to_jitval();
        self.define_marieval_to_f64();
        self.define_marieval_to_heap_string();
        self.define_marieval_to_bool();
        self.define_negate();
        self.define_bool_not();
        self.define_compare_strings();
        self.define_f64_to_bits();
        self.define_bool_to_bits();

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
            (bytecode::Op::EndFunction, lineno) => {
                for idx in self.frame().slots_offset..self.func_stack.len() {
                    self.close_upvalues(idx);
                }
                // emit_returnで、initializerでない場合はnilがstackに入っているはず
                // if !self.is_initializer {
                //     self.pop_stack(); // TODO
                // };

                // println!("{}", self.peek_by(0));
                // println!("{}", self.peek_by(1));
                // println!("{}", self.peek_by(2));

                // let num_to_pop = usize::from(self.frame().closure.function.arity)+1;

                // self.pop_stack_n_times(num_to_pop);

                // println!("{}", self.peek_by(0));
                // println!("{}", self.peek_by(1));
                // println!("{}", self.peek_by(2));

                self.frames.pop();

                if !self.is_returned {
                    let val = self.call_nil_to_jitval();
                    // Emit the return instruction.
                    self.builder.ins().return_(&[val]);
                }

                // Tell the builder we're done with this function.
                self.builder.finalize();
                
                self.elseifs.clear();
                self.entry_blocks.clear();

                self.is_done = true;
            }
            (bytecode::Op::Return, lineno) => {
                let result = self.pop_stack();
                // println!("{}", result);
                // println!("{}", self.peek_by(0));
                // println!("{}", self.peek_by(1));
                if value::type_id_of(&result.val) != self.function_type {
                    return Err(InterpreterError::Runtime(format!(
                        "This function was expected to return value type of {}, but it is returning {} (line={})",
                        value::type_id_to_string(self.function_type),
                        value::type_of(&result.val),
                        lineno.value
                    )))
                }
                // emit_returnで、initializerでない場合はnilがstackに入っているはず
                // if !self.is_initializer {
                //     self.pop_stack(); // TODO
                // };

                // println!("----- reutrn1 -------");
                // println!("{}", self.peek_by(0));
                // println!("{}", self.peek_by(1));
                // println!("----- ^reutrn1 -------");

                // println!("----- reutrn1* -------");
                // println!("{}", self.peek_by(0));
                // println!("{}", self.peek_by(1));
                // println!("----- ^reutrn1* -------");

                let mut val = self.get_jit_value(&result);

                match value::type_id_of(&result.val) {
                    1 => { // Number
                        val = self.call_f64_to_jitval(val);
                    },
                    2 => { // Bool
                        val = self.call_bool_to_jitval(val);
                    }, 
                    3 => { // String
                        val = self.call_string_to_jitval(val);
                    },
                    4 => {}, // Funcation
                    8 => {}, // Instance
                    9 => {
                        val = self.call_nil_to_jitval();
                    }, // Nil
                    10 => {}, // List
                    // value::Value::Err(_) => panic!("Unexpected value type."),
                    _ => panic!("Unexpected value type."),
                }

                // Emit the return instruction.
                self.builder.ins().return_(&[val]);
                if !self.is_in_if {
                    self.is_returned = true;
                    let num_to_pop = usize::from(self.frame().closure.function.arity)+1;
                    self.pop_stack_n_times(num_to_pop);
                }
                self.func_stack.push(result);
            }
            (bytecode::Op::Constant(idx), _) => {
                let constant = self.read_constant(idx);
                match constant {
                    value::Value::Number(v) => {
                        self.func_stack.push(
                            MarieValue { 
                                is_public: true,
                                is_mutable: true,
                                val: constant,
                                jit_value: Some(JitValue::Value(self.builder.ins().f64const(v))),
                            }
                        );
                    },
                    value::Value::String(string_id) => {
                        let string_arg = self.get_str(string_id);
                        let memoryloc = Box::into_raw(Box::new(string_arg.to_string())) as i64;
                        self.func_stack.push(
                            MarieValue { 
                                is_public: true,
                                is_mutable: true,
                                val: constant,
                                jit_value: Some(JitValue::Value(self.builder.ins().iconst(types::I64, memoryloc))),
                            }
                        );
                    },
                    value::Value::Bool(v) => {
                        self.func_stack.push(
                            MarieValue { 
                                is_public: true,
                                is_mutable: true,
                                val: constant,
                                jit_value: Some(JitValue::Value(self.builder.ins().iconst(types::B1,v as i64))),
                            }
                        );
                    },
                    _ => {
                        self.func_stack.push(
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
            (bytecode::Op::EndJump(jumptype, _has_else, has_return), _) => {
                match jumptype {
                    JumpType::IfElse => {
                        // Switch to the end block for subsequent statements.
                        let elseif = self.elseifs.remove(0);
                        let end_block = elseif.end_block;

                        if !has_return {
                            self.builder.ins().jump(end_block, &[]); // 早期returnする場合はjumpいらない
                        }
                        
                        // Switch to the end block for subsequent statements.
                        self.builder.switch_to_block(end_block);
                        self.builder.seal_block(end_block);
                        self.is_in_if = false;
                    }
                    _ => {}
                }
            }
            (bytecode::Op::Jump(jumptype, is_last, has_else, has_return,  _offset), _) => {
                match jumptype {
                    JumpType::IfElse => {
                        if is_last && has_else{
                            // let condition = self.peek().jit_value.unwrap();
                            // let jit_condition = self.to_jit_value(condition);
                            let elseif = self.elseifs[0];
                            let end_block = elseif.end_block;
                            let else_block = elseif.block;
                            //println!("{}", self.ctx.func.display());
                            if !has_return {
                                 self.builder.ins().jump(end_block, &[]);
                            }

                            // Compile else block
                            self.builder.switch_to_block(else_block);
                            self.builder.seal_block(else_block);
                        }
                    },
                    _ => {}
                }
            }
            (bytecode::Op::StartElseIf(_has_else, has_return), _) => {
                self.is_in_if = true;
                let elseif = self.elseifs[0];
                let end_block = elseif.end_block;
                let else_if_block = elseif.block;

                if !has_return {
                    self.builder.ins().jump(end_block, &[]);
                }

                // Compile else-if block
                self.builder.switch_to_block(else_if_block);
                self.builder.seal_block(else_if_block);
            }
            (bytecode::Op::JumpIfFalse(jumptype, is_first, _offset, count, has_else, has_return), _) => {
                match jumptype {
                    JumpType::IfElse => {
                        self.is_in_if = true;
                        if is_first {
                            // Define blocks for if-else-if-else statement
                            let then_block = self.builder.create_block();
                            let mut blocks = Vec::new();
                            let end_block = self.builder.create_block();
                            for _i in 0..count {
                                let else_if_block = self.builder.create_block();
                                //self.builder.append_block_param(else_if_block, types::I64);
                                blocks.push(self::IfElse { block: else_if_block, end_block}); // elseif block
                                let else_if_then_block = self.builder.create_block(); // elseif then block
                                blocks.push(self::IfElse { block: else_if_then_block, end_block});
                            }
                            if has_else {
                                let else_block = self.builder.create_block();
                                blocks.push(self::IfElse { block: else_block, end_block});
                            } else {
                                blocks.push(self::IfElse { block: end_block, end_block});
                            }
                            let first_else_if = if count > 0 || has_else {
                                blocks[0].block
                            } else {
                                end_block
                            };
                            
                            //self.elseifs.extend(blocks);
                            let elseifs  = self.elseifs.clone();
                            self.elseifs = [blocks, elseifs].concat();

                            // Test the if condition and conditionally branch to if or else-if block
                            let condition = self.pop_stack();
                            let condition_jit_value = self.to_jit_value(condition.jit_value.unwrap());
                            //self.builder.ins().brz(condition_jit_value, first_else_if, &[condition_jit_value]);
                            self.builder.ins().brz(condition_jit_value, first_else_if, &[]);
                            self.builder.ins().jump(then_block, &[]);

                            // Compile if block
                            self.builder.switch_to_block(then_block);
                            self.builder.seal_block(then_block);
                        } else {
                            let marie_condition = self.pop_stack();
                            let condition = marie_condition.jit_value.unwrap();
                            let jit_condition = self.to_jit_value(condition);
                            let elseif = self.elseifs.remove(0);
                            let _end_block = elseif.end_block;
                            let _else_if_block = elseif.block;
                            let else_if_then_block = self.elseifs.remove(0).block;
                            let next_else_if_block = self.elseifs[0].clone().block;

                            //let phi = self.builder.block_params(else_if_block)[0];
                            //self.builder.ins().brz(phi, next_else_if_block, &[jit_condition]);
                            self.builder.ins().brz(jit_condition, next_else_if_block, &[]);
                            self.builder.ins().jump(else_if_then_block, &[]);
        
                            // Compile else-if-then block
                            self.builder.switch_to_block(else_if_then_block);
                            self.builder.seal_block(else_if_then_block);
                        }
                    }
                    _ => {}
                }
            }
            (bytecode::Op::Nil, _) => {
                self.func_stack.push(
                    MarieValue {
                        is_public: true,
                        is_mutable: true,
                        val: value::Value::Nil,
                        jit_value: Some(JitValue::Value(self.builder.ins().iconst(types::I64, -1))),
                    }
                );
            }
            (bytecode::Op::Add, lineno) => {
                // println!("--- add ----");
                // println!("{}", self.peek_by(0));
                // println!("{}", self.peek_by(1));
                // println!("{}", self.peek_by(2));
                // println!("--- ^add ----");
                let marie_val1 = self.pop_stack();
                let marie_val2 = self.pop_stack();
                // self.num_print_val(&MarieValue {
                //     is_mutable: true,
                //     is_public: true,
                //     val: value::Value::Number(0.0),
                //     jit_value: Some(JitValue::Value(ans)),
                // });
                let mut val1 = marie_val1.val;
                let mut val2 = marie_val2.val;

                let jit_value1 = self.to_jit_value(marie_val1.jit_value.unwrap());
                let jit_value2 = self.to_jit_value(marie_val2.jit_value.unwrap());

                match (&mut val1, &mut val2) {
                    (value::Value::Number(num1), value::Value::Number(num2)) => {
                        let ans = self.builder.ins().fadd(jit_value2, jit_value1);
                        self.func_stack
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
                // println!("{}", self.peek_by(0));
                // println!("{}", self.peek_by(1));
                let marie_val1 = self.pop_stack();
                let marie_val2 = self.pop_stack();

                let mut val1 = marie_val1.val;
                let mut val2 = marie_val2.val;

                let jit_value1 = self.to_jit_value(marie_val1.jit_value.unwrap());
                let jit_value2 = self.to_jit_value(marie_val2.jit_value.unwrap());

                match (&mut val1, &mut val2) {
                    (value::Value::Number(num1), value::Value::Number(num2)) => {
                        let ans = self.builder.ins().fsub(jit_value2, jit_value1);
                        self.func_stack
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
                        self.func_stack
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
                        self.func_stack
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
                let mut to_print = self.pop_stack();
                match to_print.val {
                    value::Value::Number(_) => {
                        self.num_print_val(&to_print);
                    },
                    value::Value::String(_) => {
                        self.string_print_val(&mut to_print);
                    }
                    value::Value::Bool(_) => {
                        self.bool_print_val(&mut to_print);
                    },
                    _ => {}
                }
            }
            (bytecode::Op::Pop, _) => {
                self.pop_stack();
            }
            (bytecode::Op::GetLocal(idx), _) => {
                let slots_offset = self.frame().slots_offset;
                let val = self.func_stack[slots_offset + idx - 1].clone();
                self.func_stack.push(val);
            }
            (bytecode::Op::DefineLocal(is_mutable, idx), _) => {
                let slots_offset = self.frame().slots_offset;
                let mut old_val = self.func_stack[slots_offset + idx - 1].clone();
                old_val.is_mutable = is_mutable;
                self.func_stack[slots_offset + idx - 1] = old_val;
            }
            (bytecode::Op::DefineParamLocal(is_mutable, parameter_type, idx), _) => {
                let slots_offset = self.frame().slots_offset;
                let mut arg_val = self.func_stack[slots_offset + idx - 1].clone();
                if parameter_type != value::type_id_of(&arg_val.val) {
                    return Err(InterpreterError::Runtime(format!(
                        "Expected {:?} type. Found {:?}",
                        value::type_id_to_string(parameter_type),
                        value::type_of(&arg_val.val),
                    )));
                }

                let entry_block = self.entry_blocks.last().unwrap();
                let parameter_val = self.builder.block_params(*entry_block)[idx - 1];

                match parameter_type {
                    1 => { // Number
                        let f64val =  self.call_bits_to_f64(parameter_val);
                        let variable = Variable::new(slots_offset + idx - 1);
                        self.builder.declare_var(variable, types::F64);
                        self.builder.def_var(variable, f64val);
                        arg_val.jit_value = Some(JitValue::Variable(variable));
                    },
                    2 => { // Bool
                        let boolval = self.call_i64_to_bool(parameter_val);
                        let variable = Variable::new(slots_offset + idx - 1);
                        self.builder.declare_var(variable, types::B1);
                        self.builder.def_var(variable, boolval);
                        arg_val.jit_value = Some(JitValue::Variable(variable));
                    }, 
                    3 => { // String
                        let variable = Variable::new(slots_offset + idx - 1);
                        self.builder.declare_var(variable, types::I64);
                        self.builder.def_var(variable, parameter_val);
                        arg_val.jit_value = Some(JitValue::Variable(variable));
                    },
                    4 => {}, // Funcation
                    8 => {}, // Instance
                    9 => {}, // Nil
                    10 => {}, // List
                    // value::Value::Err(_) => panic!("Unexpected value type."),
                    _ => panic!("Unexpected value type."),
                }

                arg_val.is_mutable = is_mutable;
                let slots_offset = self.frame().slots_offset;
                self.func_stack[slots_offset + idx - 1] = arg_val;
            }
            (bytecode::Op::SetLocal(idx), lineno) => {
                let mut val = self.peek().clone();
                let slots_offset = self.frame().slots_offset;
                let old_val = self.func_stack[slots_offset + idx - 1].clone();
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
                self.func_stack[slots_offset + idx - 1] = val;
            }
            (bytecode::Op::Call(arg_count), _) => {
                self.call_value(self.peek_by(arg_count.into()).clone(), arg_count)?;
            }
            (bytecode::Op::True, _) => {
                let boolval = self.builder.ins().bconst(types::B1, true);
                self.func_stack.push(
                    MarieValue {
                        is_public: true,
                        is_mutable: true,
                        val: value::Value::Bool(true),
                        jit_value: Some(JitValue::Value(boolval)),
                    }
                );
            }
            (bytecode::Op::False, _) => {
                let boolval = self.builder.ins().bconst(types::B1, false);
                self.func_stack.push(
                    MarieValue {
                        is_public: true,
                        is_mutable: true,
                        val: value::Value::Bool(false),
                        jit_value: Some(JitValue::Value(boolval)),
                    }
                );
            }
            (bytecode::Op::Negate, lineno) => {
                let top_stack = &self.peek().val;
                let maybe_number = Interpreter::extract_number(top_stack);

                match maybe_number {
                        Some(to_negate) => {
                            let val = self.pop_stack();

                            let jit_value = self.to_jit_value(val.jit_value.unwrap());
                            let negated_value = self.call_negate(jit_value);

                            self.func_stack.push(
                                MarieValue {
                                    is_public: true,
                                    is_mutable: true,
                                    val: value::Value::Number(-to_negate),
                                    jit_value: Some(JitValue::Value(negated_value)),
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
            (bytecode::Op::Not, lineno) => {
                let top_stack = &self.peek().val;
                let maybe_bool = Interpreter::extract_bool(top_stack);

                match maybe_bool {
                        Some(b) => {
                            let val = self.pop_stack();

                            let jit_value = self.to_jit_value(val.jit_value.unwrap());
                            let not_value = self.call_bool_not(jit_value);

                            self.func_stack.push(
                                MarieValue {
                                    is_public: true,
                                    is_mutable: true,
                                    val: value::Value::Bool(!b),
                                    jit_value: Some(JitValue::Value(not_value)),
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

                let jit_value1 = self.to_jit_value(val1.jit_value.unwrap());
                let jit_value2 = self.to_jit_value(val2.jit_value.unwrap());

                let compare = match val2.val {
                    value::Value::Number(_) => {
                        match val1.val {
                            value::Value::Number(_) => {
                                let c = self.builder.ins().fcmp(FloatCC::Equal, jit_value2, jit_value1);
                                c
                            },
                            value::Value::Bool(_) => {
                                panic!("You cannot compare Number and Bool.");
                            },
                            value::Value::String(_) => {
                                panic!("You cannot compare Number and String.");
                            }
                            _ => {
                                panic!("Undefined comparison.");
                            }
                        }
                    },
                    value::Value::String(_) => {
                        match val1.val {
                            value::Value::Number(_) => {
                                panic!("You cannot compare String and Number.");
                            },
                            value::Value::String(_) => {
                                self.call_compare_strings(jit_value2, jit_value1)
                            }
                            value::Value::Bool(_) => {
                                panic!("You cannot compare String and Bool.");
                            },
                            _ => {
                                panic!("Undefined comparison.");
                            }
                        }
                    }
                    value::Value::Bool(_) => {
                        match val1.val {
                            value::Value::Number(_) => {
                                panic!("You cannot compare Bool and Number.");
                            },
                            value::Value::Bool(_) => {
                                let c = self.builder.ins().icmp(IntCC::Equal, jit_value2, jit_value1);
                                c
                            },
                            value::Value::String(_) => {
                                panic!("You cannot compare Number and String.");
                            }
                            _ => {
                                panic!("Undefined comparison.");
                            }
                        }
                    },
                    _ => {
                        panic!("Undefined comparison.");
                    }
                };

                self.func_stack
                    .push(
                        MarieValue {
                            is_public: true,
                            is_mutable: true,
                            val: value::Value::Bool(true),
                            jit_value: Some(JitValue::Value(compare)),
                        }
                    );
            }
            // (bytecode::Op::Greater, lineno) => {
            //     let val1 = self.peek_by(0).clone().val;
            //     let val2 = self.peek_by(1).clone().val;

            //     match (&val1, &val2) {
            //             (value::Value::Number(n1), value::Value::Number(n2)) => {
            //                 self.pop_stack();
            //                 self.pop_stack();

            //                 self.stack.push(
            //                     MarieValue {
            //                         is_public: true,
            //                         is_mutable: true,
            //                         val: value::Value::Bool(n2 > n1),
            //                         jit_value: None,
            //                     }
            //                 );
            //             }
            //             _ => return Err(InterpreterError::Runtime(format!(
            //                 "invalid operands in Greater expression. Expected numbers, found {:?} and {:?} at line {}",
            //                 value::type_of(&val1), value::type_of(&val2), lineno.value)))

            //         }
            // }
            // (bytecode::Op::Less, lineno) => {
            //     let val1 = self.peek_by(0).clone().val;
            //     let val2 = self.peek_by(1).clone().val;

            //     match (&val1, &val2) {
            //             (value::Value::Number(n1), value::Value::Number(n2)) => {
            //                 self.pop_stack();
            //                 self.pop_stack();
            //                 self.stack.push(
            //                     MarieValue {
            //                         is_public: true,
            //                         is_mutable: true,
            //                         val: value::Value::Bool(n2 < n1),
            //                         jit_value: None,
            //                     }
            //                 );
            //             }
            //             _ => return Err(InterpreterError::Runtime(format!(
            //                 "invalid operands in Less expression. Expected numbers, found {:?} and {:?} at line {}",
            //                 value::type_of(&val1), value::type_of(&val2), lineno.value)))

            //         }
            // }
            (bytecode::Op::GetGlobal(idx), lineno) => {
                if let value::Value::String(name_id) = self.read_constant(idx) {
                    match self.globals.get(self.get_str(name_id)) {
                        Some(val) => {
                            self.func_stack.push(val.clone());
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
                let mut result; 
                let closure = self.get_closure(closure_handle).clone();
                let func = &closure.function;
                if arg_count != func.arity {
                    return Err(InterpreterError::Runtime(format!(
                        "Expected {} arguments but found {}.",
                        func.arity, arg_count
                    )));
                }
                let mut arguments = Vec::new();
                let arg_count = closure.function.arity;
                let mut builder_context = FunctionBuilderContext::new();
                let mut ctx = self.module.make_context();
                let mut sig = self.module.make_signature(); 
                for i in 0..arg_count {
                    ctx.func.signature.params.push(AbiParam::new(types::I64));
                    sig.params.push(AbiParam::new(types::I64));
                    let arg = self.peek_by(i as usize);
                    match arg.val {
                        // わざわざiconstに統一する意味ないのでは？
                        value::Value::Number(arg_val) => {
                            if let Some(jitval) = arg.jit_value {
                                let jitval = self.to_jit_value(jitval);
                                let val = self.call_f64_to_bits(jitval);
                                arguments.push(val);
                            } else {
                                arguments.push(self.builder.ins().iconst(types::I64, arg_val.to_bits() as i64));
                            }
                        }
                        value::Value::Bool(arg_val) => {
                            if let Some(jitval) = arg.jit_value {
                                let jitval = self.to_jit_value(jitval);
                                let val = self.call_bool_to_bits(jitval);
                                arguments.push(val);
                            } else {
                                arguments.push(self.builder.ins().iconst(types::I64, arg_val as i64));
                            }
                        }
                        value::Value::String(string_id) => {
                            if let Some(jitval) = arg.jit_value {
                                let jitval = self.to_jit_value(jitval);
                                //self.call_print_string_jitval(jitval);
                                arguments.push(jitval);
                            } else {
                                let string_arg = self.get_str(string_id);
                                let string_val = Box::into_raw(Box::new(string_arg.to_string())) as i64;
                                arguments.push(self.builder.ins().iconst(types::I64,string_val));
                            }
                        }
                        _ => {
                            println!("wrong type of argument: {}", arg.val);
                        }
                    }
                }
                arguments.reverse();
                //let a = self.builder.create_block();
                //self.builder.ins().call(a, &vec![]);
                let func_name = closure.function.name.clone();
                if self.my_func_name == func_name {
                    // recursive call
                    let call = self.builder.ins().call(self.my_func_ref, &arguments);
                    result = self.builder.inst_results(call)[0];
                    let num_to_pop = usize::from(self.frame().closure.function.arity)+1;
                    self.pop_stack_n_times(num_to_pop);
                } else if !closure.is_compiled && closure.use_compiled {
                    // first function call
                    //-------------
                    // Our language currently only supports one return value, though
                    // Cranelift is designed to support more.
                    ctx.func.signature.returns.push(AbiParam::new(types::I64));
                    sig.returns.push(AbiParam::new(types::I64));

                    let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_context);

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
                    frame.slots_offset = self.func_stack.len() - usize::from(arg_count);
                    frame.invoked_method_id = Some(closure_handle);
                    // ---

                    let mut entry_blocks = Vec::new();
                    entry_blocks.push(entry_block);

                    // Next, declare the function to jit. Functions must be declared
                    // before they can be called, or defined.
                    let maybe_func_id = self
                        .module
                        .declare_function(&func_name, Linkage::Export, &sig)
                        .map_err(|e| e.to_string());

                    let funcid = match maybe_func_id {
                        Ok(id) => id,
                        Err(err) => {
                            panic!("{}", err);
                        }
                    };

                    //let _func_external_name = ExternalName::user(0, funcid.as_u32());

                    let local_callee = self.module
                        .declare_func_in_func(funcid, &mut builder.func);

                    // Now translate the statements of the function body.
                    let mut trans = FunctionTranslator {
                        my_func_ref: local_callee,
                        my_func_name: func_name,
                        val_type: types::I64,
                        builder,
                        data_ctx: &mut self.data_ctx,
                        func_stack: &mut self.func_stack,
                        output: &mut self.output,
                        module: &mut self.module,
                        frames: &mut self.frames,
                        heap: &mut self.heap,
                        upvalues: &mut self.upvalues,
                        entry_blocks,
                        is_done: false,
                        is_returned: false,
                        is_in_if: false,
                        funcs: Vec::new(),
                        elseifs: Vec::new(),
                        function_type: closure.function_type,
                        globals: &mut self.globals,
                        is_initializer: false,
                    };
                    trans.run()?;

                    // Define the function to jit. This finishes compilation, although
                    // there may be outstanding relocations to perform. Currently, jit
                    // cannot finish relocations until all functions to be called are
                    // defined. For this toy demo for now, we'll just finalize the
                    // function below.
                    let jitresult = self.module
                        .define_function(
                            funcid,
                            &mut ctx,
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
                    self.module.clear_context(&mut ctx);

                    // Finalize the functions which we just defined, which resolves any
                    // outstanding relocations (patching in addresses, now that they're
                    // available).
                    self.module.finalize_definitions();

                    // We can now retrieve a pointer to the machine code.
                    // let fn_code = self.module.get_finalized_function(funcid);
                    let funcref = self.module.declare_func_in_func(funcid, &mut self.builder.func);
                    let mut old_closure = self.get_mut_closure(closure_handle);
                    old_closure.is_compiled = true;
                    old_closure.function.func_id = Some(funcid);
                    // 以下は関数コンパイル後に重複実行しないように必要
                    let mut is_returned = false;
                    old_closure.function.chunk.code.retain(|op|{
                        match op.0 {
                            bytecode::Op::Return => {
                                if !is_returned {
                                    is_returned = true;
                                    return true;
                                }
                                false
                            }
                            bytecode::Op::EndFunction => {
                                if !is_returned {
                                    is_returned = true;
                                    return true;
                                }
                                false
                            }
                            bytecode::Op::DefineParamLocal(_, _, _) => {
                                if !is_returned {
                                    return true;
                                }
                                false
                            }
                            _ => false
                        }
                    });
                    let call = self.builder.ins().call(funcref, &arguments);
                    result = self.builder.inst_results(call)[0];
                    //println!("{}", result);
                    // self.pop_stack(); // remove the "function call" from stack
                    //let arg = self.pop_stack();
                    //self.pop_stack();
                    // println!("--------- call 1--------");
                    // println!("{}", self.peek_by(0));
                    // println!("{}", self.peek_by(1));
                    // println!("{}", self.peek_by(2));
                    // println!("{}", self.peek_by(3));
                    // println!("--------- ^call 1--------");
                } else {
                    loop {
                        let op = self.next_op();
                        match op.0 {
                            bytecode::Op::DefineParamLocal(_, parameter_type, idx) => {
                                let slots_offset = self.frame().slots_offset;
                                let arg_val = self.func_stack[slots_offset + idx - 1].clone();
                                if parameter_type != value::type_id_of(&arg_val.val) {
                                    return Err(InterpreterError::Runtime(format!(
                                        "Expected {:?} type. Found {:?}",
                                        value::type_id_to_string(parameter_type),
                                        value::type_of(&arg_val.val),
                                    )));
                                }
                                self.advance();
                            }
                            _ => {
                                // println!("{:?}", op.0);
                                break
                            }
                        }
                    }
                    let func = &closure.function;
                    let funcid = func.func_id.unwrap();
                    let funcref = self.module.declare_func_in_func(funcid, &mut self.builder.func);
                    let call = self.builder.ins().call(funcref, &arguments);
                    result = self.builder.inst_results(call)[0];
                    //let funcresult = self.pop_stack();
                    let num_to_pop = usize::from(self.frame().closure.function.arity)+1;
                    self.pop_stack_n_times(num_to_pop);
                    //println!("{}", self.pop_stack()); // remove the "function call" from stack
                    //self.func_stack.push(funcresult);
                    // println!("--------- call 2--------");
                    // println!("{}", self.peek_by(0));
                    // println!("{}", self.peek_by(1));
                    // println!("{}", self.peek_by(2));
                    // println!("{}", self.peek_by(3));
                    // println!("--------- ^call 2--------");
                }
                match closure.function_type {
                    1 => { // Number
                        result = self.call_marieval_to_f64(result);
                    },
                    2 => { // Bool
                        result = self.call_marieval_to_bool(result);
                    }, 
                    3 => { // String
                        result = self.call_marieval_to_heap_string(result);
                    },
                    4 => {}, // Funcation
                    8 => {}, // Instance
                    9 => { // Nil
                        result = self.builder.ins().iconst(types::I64, 0);
                    },
                    10 => {}, // List
                    // value::Value::Err(_) => panic!("Unexpected value type."),
                    _ => panic!("Unexpected value type."),
                };
                let returned_value_type = value::type_id_to_value(closure.function_type);
                self.func_stack
                    .push(
                        MarieValue {
                            is_mutable: true,
                            is_public: true,
                            val: returned_value_type,
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

    pub fn bool_print_val(&mut self, val: &mut MarieValue) {
        //let output = self.format_val(val);
        let jit_value = self.get_jit_value(val);
        self.call_print_bool_jitval(jit_value);
        //self.output.push(output);
    }

    pub fn num_print_val(&mut self, val: &MarieValue) {
        //let output = self.format_val(val);
        let jit_value = self.get_jit_value(val);
        self.call_print_jitval(jit_value);
        //self.output.push(output);
    }

    pub fn pop_stack(&mut self) -> value::MarieValue {
        match self.func_stack.pop() {
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
        let value = &self.func_stack[index];
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

    pub fn get_mut_closure(&mut self, closure_handle: gc::HeapId) -> &mut value::Closure {
        self.heap.get_mut_closure(closure_handle)
    }

    pub fn next_op(&self) -> (bytecode::Op, bytecode::Lineno) {
        self.frame().next_op()
    }

    pub fn advance(&mut self) {
        self.frame_mut().advance();
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
        &self.func_stack[self.func_stack.len() - n - 1]
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
        let local_callee = self.funcs[1];
    
        let args = vec![val1];
    
        let call = self.builder.ins().call(local_callee, &args);
        self.builder.inst_results(call);
    }

    fn define_f64_to_jitval(&mut self) {
        let mut sig = self.module.make_signature();

        sig.returns.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::F64));
        let callee = self.module
            .declare_function("f64_to_jitval", cranelift_module::Linkage::Import, &sig)
            .map_err(|e| e.to_string()).unwrap();
    
        let local_callee = self.module
            .declare_func_in_func(callee, &mut self.builder.func);

        self.funcs.push(local_callee);
    }

    fn call_f64_to_jitval(&mut self, val1: cranelift::prelude::Value) -> cranelift::prelude::Value {
        let local_callee = self.funcs[2];

        let args = vec![val1];
    
        let call = self.builder.ins().call(local_callee, &args);
        self.builder.inst_results(call)[0]
    }

    fn define_string_to_jitval(&mut self) {
        let mut sig = self.module.make_signature();

        sig.returns.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        let callee = self.module
            .declare_function("string_to_jitval", cranelift_module::Linkage::Import, &sig)
            .map_err(|e| e.to_string()).unwrap();
    
        let local_callee = self.module
            .declare_func_in_func(callee, &mut self.builder.func);

        self.funcs.push(local_callee);
    }

    fn call_string_to_jitval(&mut self, val1: cranelift::prelude::Value) -> cranelift::prelude::Value {
        let local_callee = self.funcs[3];

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
        let local_callee = self.funcs[4];

        let args = vec![val1];
    
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
        let local_callee = self.funcs[5];

        let args = vec![val1];
    
        let call = self.builder.ins().call(local_callee, &args);
        self.builder.inst_results(call);
    }

    fn define_print_bool_jitval(&mut self) {
        let mut sig = self.module.make_signature();

        sig.params.push(AbiParam::new(types::B1));
        let callee = self.module
            .declare_function("print_bool_jitval", cranelift_module::Linkage::Import, &sig)
            .map_err(|e| e.to_string()).unwrap();
    
        let local_callee = self.module
            .declare_func_in_func(callee, &mut self.builder.func);

        self.funcs.push(local_callee);
    }

    fn call_print_bool_jitval(&mut self, val1: cranelift::prelude::Value) {
        let local_callee = self.funcs[6];

        let args = vec![val1];
    
        let call = self.builder.ins().call(local_callee, &args);
        self.builder.inst_results(call);
    }

    fn define_i64_to_bool(&mut self) {
        let mut sig = self.module.make_signature();

        sig.returns.push(AbiParam::new(types::B1));
        sig.params.push(AbiParam::new(types::I64));
        let callee = self.module
            .declare_function("i64_to_bool", cranelift_module::Linkage::Import, &sig)
            .map_err(|e| e.to_string()).unwrap();
    
        let local_callee = self.module
            .declare_func_in_func(callee, &mut self.builder.func);

        self.funcs.push(local_callee);
    }

    fn call_i64_to_bool(&mut self, val1: cranelift::prelude::Value) -> cranelift::prelude::Value {
        let local_callee = self.funcs[7];

        let args = vec![val1];
    
        let call = self.builder.ins().call(local_callee, &args);
        self.builder.inst_results(call)[0]
    }

    fn define_nil_to_jitval(&mut self) {
        let mut sig = self.module.make_signature();

        sig.returns.push(AbiParam::new(types::I64));
        let callee = self.module
            .declare_function("nil_to_jitval", cranelift_module::Linkage::Import, &sig)
            .map_err(|e| e.to_string()).unwrap();
    
        let local_callee = self.module
            .declare_func_in_func(callee, &mut self.builder.func);

        self.funcs.push(local_callee);
    }

    fn call_nil_to_jitval(&mut self) -> cranelift::prelude::Value {
        let local_callee = self.funcs[8];

        let args = vec![];
    
        let call = self.builder.ins().call(local_callee, &args);
        self.builder.inst_results(call)[0]
    }
    
    fn define_bool_to_jitval(&mut self) {
        let mut sig = self.module.make_signature();

        sig.returns.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::B1));
        let callee = self.module
            .declare_function("bool_to_jitval", cranelift_module::Linkage::Import, &sig)
            .map_err(|e| e.to_string()).unwrap();
    
        let local_callee = self.module
            .declare_func_in_func(callee, &mut self.builder.func);

        self.funcs.push(local_callee);
    }

    fn call_bool_to_jitval(&mut self, val1: cranelift::prelude::Value) -> cranelift::prelude::Value {
        let local_callee = self.funcs[9];

        let args = vec![val1];
    
        let call = self.builder.ins().call(local_callee, &args);
        self.builder.inst_results(call)[0]
    }

    fn define_marieval_to_f64(&mut self) {
        let mut sig = self.module.make_signature();

        sig.returns.push(AbiParam::new(types::F64));
        sig.params.push(AbiParam::new(types::I64));
        let callee = self.module
            .declare_function("marieval_to_f64", cranelift_module::Linkage::Import, &sig)
            .map_err(|e| e.to_string()).unwrap();
    
        let local_callee = self.module
            .declare_func_in_func(callee, &mut self.builder.func);

        self.funcs.push(local_callee);
    }

    fn call_marieval_to_f64(&mut self, val1: cranelift::prelude::Value) -> cranelift::prelude::Value {
        let local_callee = self.funcs[10];

        let args = vec![val1];
    
        let call = self.builder.ins().call(local_callee, &args);
        self.builder.inst_results(call)[0]
    }

    fn define_marieval_to_heap_string(&mut self) {
        let mut sig = self.module.make_signature();

        sig.returns.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        let callee = self.module
            .declare_function("marieval_to_heap_string", cranelift_module::Linkage::Import, &sig)
            .map_err(|e| e.to_string()).unwrap();
    
        let local_callee = self.module
            .declare_func_in_func(callee, &mut self.builder.func);

        self.funcs.push(local_callee);
    }

    fn call_marieval_to_heap_string(&mut self, val1: cranelift::prelude::Value) -> cranelift::prelude::Value {
        let local_callee = self.funcs[11];

        let args = vec![val1];
    
        let call = self.builder.ins().call(local_callee, &args);
        self.builder.inst_results(call)[0]
    }

    fn define_marieval_to_bool(&mut self) {
        let mut sig = self.module.make_signature();

        sig.returns.push(AbiParam::new(types::B1));
        sig.params.push(AbiParam::new(types::I64));
        let callee = self.module
            .declare_function("marieval_to_bool", cranelift_module::Linkage::Import, &sig)
            .map_err(|e| e.to_string()).unwrap();
    
        let local_callee = self.module
            .declare_func_in_func(callee, &mut self.builder.func);

        self.funcs.push(local_callee);
    }

    fn call_marieval_to_bool(&mut self, val1: cranelift::prelude::Value) -> cranelift::prelude::Value {
        let local_callee = self.funcs[12];

        let args = vec![val1];
    
        let call = self.builder.ins().call(local_callee, &args);
        self.builder.inst_results(call)[0]
    }


    fn define_negate(&mut self) {
        let mut sig = self.module.make_signature();

        sig.returns.push(AbiParam::new(types::F64));
        sig.params.push(AbiParam::new(types::F64));
        let callee = self.module
            .declare_function("negate", cranelift_module::Linkage::Import, &sig)
            .map_err(|e| e.to_string()).unwrap();
    
        let local_callee = self.module
            .declare_func_in_func(callee, &mut self.builder.func);

        self.funcs.push(local_callee);
    }

    fn call_negate(&mut self, val: cranelift::prelude::Value) -> cranelift::prelude::Value {
        let local_callee = self.funcs[13];

        let args = vec![val];
    
        let call = self.builder.ins().call(local_callee, &args);
        self.builder.inst_results(call)[0]
    }

    fn define_bool_not(&mut self) {
        let mut sig = self.module.make_signature();

        sig.returns.push(AbiParam::new(types::B1));
        sig.params.push(AbiParam::new(types::B1));
        let callee = self.module
            .declare_function("bool_not", cranelift_module::Linkage::Import, &sig)
            .map_err(|e| e.to_string()).unwrap();
    
        let local_callee = self.module
            .declare_func_in_func(callee, &mut self.builder.func);

        self.funcs.push(local_callee);
    }

    fn call_bool_not(&mut self, val: cranelift::prelude::Value) -> cranelift::prelude::Value {
        let local_callee = self.funcs[14];

        let args = vec![val];
    
        let call = self.builder.ins().call(local_callee, &args);
        self.builder.inst_results(call)[0]
    }

    fn define_compare_strings(&mut self) {
        let mut sig = self.module.make_signature();

        sig.returns.push(AbiParam::new(types::B1));
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        let callee = self.module
            .declare_function("compare_strings", cranelift_module::Linkage::Import, &sig)
            .map_err(|e| e.to_string()).unwrap();
    
        let local_callee = self.module
            .declare_func_in_func(callee, &mut self.builder.func);

        self.funcs.push(local_callee);
    }

    fn call_compare_strings(&mut self, val1: cranelift::prelude::Value, val2: cranelift::prelude::Value) -> cranelift::prelude::Value {
        let local_callee = self.funcs[15];

        let args = vec![val1, val2];
    
        let call = self.builder.ins().call(local_callee, &args);
        self.builder.inst_results(call)[0]
    }

    fn define_f64_to_bits(&mut self) {
        let mut sig = self.module.make_signature();

        sig.returns.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::F64));
        let callee = self.module
            .declare_function("f64_to_bits", cranelift_module::Linkage::Import, &sig)
            .map_err(|e| e.to_string()).unwrap();
    
        let local_callee = self.module
            .declare_func_in_func(callee, &mut self.builder.func);

        self.funcs.push(local_callee);
    }

    fn call_f64_to_bits(&mut self, val1: cranelift::prelude::Value) -> cranelift::prelude::Value {
        let local_callee = self.funcs[16];

        let args = vec![val1];
    
        let call = self.builder.ins().call(local_callee, &args);
        self.builder.inst_results(call)[0]
    }

    fn define_bool_to_bits(&mut self) {
        let mut sig = self.module.make_signature();

        sig.returns.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::B1));
        let callee = self.module
            .declare_function("bool_to_bits", cranelift_module::Linkage::Import, &sig)
            .map_err(|e| e.to_string()).unwrap();
    
        let local_callee = self.module
            .declare_func_in_func(callee, &mut self.builder.func);

        self.funcs.push(local_callee);
    }

    fn call_bool_to_bits(&mut self, val1: cranelift::prelude::Value) -> cranelift::prelude::Value {
        let local_callee = self.funcs[17];

        let args = vec![val1];
    
        let call = self.builder.ins().call(local_callee, &args);
        self.builder.inst_results(call)[0]
    }
}
