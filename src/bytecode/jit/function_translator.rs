use cranelift::{codegen::ir::InstructionData, prelude::types};
use cranelift_frontend::FunctionBuilder;
use cranelift_jit::JITModule;

use std::collections::HashMap;

use bytecode::bytecode_interpreter::InterpreterError::Runtime;

use crate::bytecode::bytecode_interpreter::InterpreterError;
use crate::bytecode::jit::pack_meta;
use crate::bytecode::values::value::Type as MvalueType;
use crate::bytecode::{self, StepResult};
use crate::{
    bytecode::{
        bytecode::{unpack_one_flag, Chunk, Constant, Opcode, ValueMeta},
        values::{
            value::{self, Value as Marieval},
            Binop,
        },
    },
    gc::gc,
};
use cranelift::{
    codegen::ir::{ExternalName, FuncRef, Inst},
    prelude::*,
};
use cranelift_module::{Linkage, Module};

#[derive(Clone, Copy)]
pub struct IfElseBlock {
    if_id: usize,
    block: Block,
    first_if_id: usize,
    end_block_id: usize,
    next_if_id: usize,
    has_else: bool,
    has_elseif: bool,
}

pub struct FunctionTranslator<'fb, 'mval, 'h> {
    pub builder: FunctionBuilder<'fb>,
    pub operand_stack: Vec<Value>,
    pub operand_meta_stack: Vec<ValueMeta>,
    pub locals: std::collections::HashMap<usize, Variable>,
    pub local_meta: std::collections::HashMap<usize, ValueMeta>,
    pub stack: &'mval mut Vec<Marieval>,
    pub stack_meta: &'mval mut Vec<ValueMeta>,
    pub heap: &'h mut gc::Heap,
    pub slots_offset: usize,
    pub if_block_stack: Vec<IfElseBlock>,
    pub entry_block: Block,
}

impl<'fb, 'mval, 'h> FunctionTranslator<'fb, 'mval, 'h> {
    /// High-level entry point the JIT calls.
    /// Consumes `self`, walks the byte-code, fills `builder.func`,
    /// and then finalises the IR.
    pub fn translate(mut self, chunk: &Chunk) -> StepResult<(), InterpreterError> {
        let meta = self.lower_chunk(chunk);
        match meta {
            StepResult::Err(err) => {
                return StepResult::Err(err);
            }
            StepResult::Ok(_) => {
                self.builder.finalize();
                return StepResult::Ok(());
            }
            StepResult::OkReturn(_) => {
                self.builder.finalize();
                return StepResult::OkReturn(());
            }
        }
    }

    // -----------------------------------------------------------------
    // Private helpers
    // -----------------------------------------------------------------
    fn lower_chunk(&mut self, chunk: &Chunk) -> StepResult<(), InterpreterError> {
        // init all variables
        let mut i = 1;
        for inst in &chunk.code {
            //println!("{:?}", inst);
            match inst.opcode {
                Opcode::DefineArgumentLocal => {
                    let param = self.builder.block_params(self.entry_block)[i];
                    let meta_param = self.builder.block_params(self.entry_block)[i + 1];
                    let slot = inst.operand;
                    self.emit_init_define_argument(slot, param, meta_param, i);
                    i = i + 2;
                }
                // 他の opcode は後で
                _ => {}
            }
        }

        self.builder.seal_block(self.entry_block);
        let mut if_vec = Vec::<usize>::new();
        for inst in &chunk.code {
            //println!("{:?}", inst);
            match inst.opcode {
                Opcode::DefineLocal => {
                    let slot = inst.operand;
                    self.emit_init_define_local(slot)
                }
                Opcode::BeginIf => {
                    // if 本体の then
                    let end_block = self.builder.create_block();
                    let then_block = self.builder.create_block();

                    // else/elseif 連鎖の出口は最初だけ確保、
                    if_vec.push(if_vec.len());
                    let (has_else_if, has_else) = unpack_one_flag(inst.operand);
                    let has_else = has_else == 1;
                    let end_block_id = self.if_block_stack.len();
                    let first_if_id = self.if_block_stack.len()+1;

                    self.if_block_stack.push(
                        IfElseBlock {
                            if_id: *if_vec.last().unwrap(),
                            first_if_id,
                            end_block_id,
                            next_if_id: end_block_id,
                            block: end_block,
                            has_else: has_else,
                            has_elseif: has_else_if,
                        },
                    );
                    self.if_block_stack.push(
                        IfElseBlock {
                            if_id: *if_vec.last().unwrap(),
                            first_if_id,
                            end_block_id,
                            next_if_id: end_block_id,
                            block: then_block,
                            has_else: has_else,
                            has_elseif: has_else_if,
                        },
                    );
                }
                Opcode::BeginElseIf => {
                    let if_condition_block = self.builder.create_block();
                    // else-if 判定ブロック
                    let cond_block = self.builder.create_block();
                    // else-if の then ブロック
                    let then_block = self.builder.create_block();

                    let mut last_item = self.if_block_stack[self.if_block_stack.len() - 1];
                    last_item.next_if_id = self.if_block_stack.len();
                    let vec_length = self.if_block_stack.len();
                    self.if_block_stack[vec_length - 1] = last_item;
                    self.if_block_stack.push(
                        IfElseBlock {
                            if_id: *if_vec.last().unwrap(),
                            block: if_condition_block,
                            first_if_id: last_item.first_if_id,
                            end_block_id: last_item.end_block_id,
                            next_if_id: last_item.end_block_id,
                            has_else: inst.operand == 1,
                            has_elseif: true,
                        },
                    );

                    self.if_block_stack.push(
                        IfElseBlock {
                            if_id: *if_vec.last().unwrap(),
                            block: cond_block,
                            first_if_id: last_item.first_if_id,
                            end_block_id: last_item.end_block_id,
                            next_if_id: last_item.end_block_id,
                            has_else: inst.operand == 1,
                            has_elseif: true,
                        },
                    );

                    self.if_block_stack.push(
                        IfElseBlock {
                            if_id: *if_vec.last().unwrap(),
                            block: then_block,
                            first_if_id: last_item.first_if_id,
                            end_block_id: last_item.end_block_id,
                            next_if_id: last_item.end_block_id,
                            has_else: inst.operand == 1,
                            has_elseif: true,
                        },
                    );
                }
                Opcode::BeginElse => {
                    let mut last_item = self.if_block_stack.pop().unwrap();
                    last_item.next_if_id = self.if_block_stack.len()+1;
                    self.if_block_stack.push(last_item);

                    let else_block = self.builder.create_block();
                    self.if_block_stack.push(
                            IfElseBlock {
                            if_id: *if_vec.last().unwrap(),
                            block: else_block,
                            first_if_id: 0,
                            end_block_id: 0,
                            next_if_id: 0,
                            has_else: true,
                            has_elseif: false,
                        },
                    );
                }
                Opcode::EndIf => {}
                Opcode::EndAllIf => {
                    if_vec.pop();
                }
                // 他の opcode は後で
                _ => {}
            }
        }

        let mut is_returned = false;
        // 2. run code
        for inst in &chunk.code {
            //println!("{:?}", inst);
            match inst.opcode {
                Opcode::Add => match self.emit_binop(Binop::Add) {
                    StepResult::Err(err) => {
                        return StepResult::Err(Runtime(err));
                    }
                    _ => {}
                },
                Opcode::Subtract => match self.emit_binop(Binop::Sub) {
                    StepResult::Err(err) => {
                        return StepResult::Err(Runtime(err));
                    }
                    _ => {}
                },
                Opcode::Multiply => match self.emit_binop(Binop::Mul) {
                    StepResult::Err(err) => {
                        return StepResult::Err(Runtime(err));
                    }
                    _ => {}
                },
                Opcode::Divide => match self.emit_binop(Binop::Div) {
                    StepResult::Err(err) => {
                        return StepResult::Err(Runtime(err));
                    }
                    _ => {}
                },
                Opcode::Modulus => match self.emit_binop(Binop::Modulus) {
                    StepResult::Err(err) => {
                        return StepResult::Err(Runtime(err));
                    }
                    _ => {}
                },
                Opcode::Pow => match self.emit_binop(Binop::Pow) {
                    StepResult::Err(err) => {
                        return StepResult::Err(Runtime(err));
                    }
                    _ => {}
                },
                Opcode::Equal => {
                    let (value1, meta1) = self.pop();
                    let (value2, meta2) = self.pop();
                    // TODO

                    let comparison = self.builder.ins().fcmp(FloatCC::Equal, value2, value1);
                    self.operand_stack.push(comparison);
                    self.operand_meta_stack.push(ValueMeta {
                        is_public: true,
                        is_mutable: true,
                        value_type: MvalueType::Bool,
                    });
                }
                Opcode::DefineLocal => {
                    let slot = inst.operand;
                    self.emit_define_local(slot, false)
                }
                Opcode::GetLocal => {
                    let slot = inst.operand;
                    self.emit_get_local(slot);
                }
                Opcode::SetLocal => {
                    let slot = inst.operand;
                    self.emit_set_local(slot);
                }
                Opcode::Null => self.emit_null(),
                Opcode::Return => {
                    is_returned = true;
                    self.emit_return();
                }
                Opcode::InsideIfReturn => {
                    self.emit_return();
                }
                Opcode::Pop => {
                    self.pop();
                }
                Opcode::JitIgnoredPop => {}
                Opcode::Constant => {
                    self.constant(inst.operand, chunk);
                }
                Opcode::True => {
                    self.emit_bool(true);
                }
                Opcode::False => {
                    self.emit_bool(false);
                }
                Opcode::DefineArgumentLocal => {}
                Opcode::EndOfScope => {
                    if !is_returned {
                        self.close_scope_without_return();
                    }
                }
                Opcode::BeginIf => {
                    let (condition, _) = self.pop();
                    let (_has_else_if, _has_else) = unpack_one_flag(inst.operand);

                    let _end_block = self
                        .if_block_stack[0];
                    let then_block = self
                        .if_block_stack[1];

                    //self.builder.ins().brz(condition_jit_value, first_else_if, &[condition_jit_value]);

                    // 0 is end block, 1 is then block
                    let next_else_if = self.if_block_stack[then_block.next_if_id];
                    self.builder.ins().brif(
                        condition,
                        then_block.block,
                        &[],
                        next_else_if.block,
                        &[],
                    );
                    self.builder.switch_to_block(then_block.block);
                }
                Opcode::EndIf => {
                    // previous if block
                    let previous_if_condition_block = self.if_block_stack.remove(1);
                    // then block
                    if previous_if_condition_block.has_else {
                        let _previous_if_block = self.if_block_stack.remove(0);
                    }
                }
                Opcode::PrepareElseIf => {
                    let next_if = self.if_block_stack.remove(0);
                    let condition_else_if_block = next_if.block;

                    // Compile else-if block
                    self.builder.switch_to_block(condition_else_if_block);
                }
                Opcode::BeginElseIf => {
                    let (condition, _) = self.pop();
                    let _has_else = inst.operand == 1;
                    let then_block = self
                        .if_block_stack[0];

                    // 0 is else if then block, 1 is next block
                    let next_else_if = self.if_block_stack[1];
                    self.builder.ins().brif(
                        condition,
                        then_block.block,
                        &[],
                        next_else_if.block,
                        &[],
                    );
                    self.builder.switch_to_block(then_block.block);
                }
                Opcode::EndElseIf => {
                    let _previous_if_block = self.if_block_stack.remove(0);
                }
                Opcode::BeginElse => {
                    let end_block = self.if_block_stack[0];
                    self.builder.switch_to_block(end_block.block);
                }
                Opcode::EndAllIf => {
                    let end_block = self
                        .if_block_stack
                        .remove(0);

                    if !is_returned {
                        self.builder.switch_to_block(end_block.block);
                        self.builder.seal_block(end_block.block);
                    }
                }
                Opcode::JumpIfFalse => {}
                Opcode::Jump => {}
                // 他の opcode は後で
                _ => unimplemented!("{:?}", inst.opcode),
            }
        }

        // ❸ 以降の IR は到達不能になるのでブロックを閉じておくと親切
        // （必須ではないが verifier が喜ぶ）
        self.builder.seal_all_blocks();
        return StepResult::Ok(());
    }

    fn constant(&mut self, idx: u32, chunk: &Chunk) {
        let val = &chunk.constants[idx as usize];

        match val {
            Constant::Number(f) => {
                let meta = ValueMeta {
                    is_public: true,
                    is_mutable: true,
                    value_type: MvalueType::Number,
                };
                let v = self.builder.ins().f64const(*f);
                self.operand_stack.push(v);
                self.operand_meta_stack.push(meta);
            },
            // TODO:
            other => unimplemented!("constant {:?} not lowered yet", other),
        };

    }

    fn emit_bool(&mut self, b: bool) {
        // 0 / 1 をそのまま I64 即値に
        let bits = if b { 1 } else { 0 };
        let v = self.builder.ins().iconst(types::I64, bits);

        self.operand_stack.push(v);
        self.operand_meta_stack.push(ValueMeta {
            is_public: true,
            is_mutable: true,
            value_type: MvalueType::Bool,
        });
    }

    // すでにある locals: HashMap<u32, Variable>
    fn emit_define_local(&mut self, packed: u32, init_from_stack: bool) {
        // ❶ is_mutable と idx を分離
        let (_is_mutable, idx) = unpack_one_flag(packed);

        // ❷ Variable を確保
        let var = *self.locals.get(&idx).expect("local variable not found");

        // ❸ 初期値を決める
        let init_val = if init_from_stack {
            let (val, _) = self.pop();
            val
        } else {
            self.builder.ins().iconst(types::I64, 0) // null
        };
        self.builder.def_var(var, init_val);
    }

    fn emit_init_define_argument(
        &mut self,
        packed: u32,
        param: Value,
        _meta_param: Value,
        slot: usize,
    ) {
        // ❶ is_mutable と idx を分離
        let (_is_mutable, idx) = unpack_one_flag(packed);

        // ❸ 初期値を決める
        let slots_offset = self.slots_offset;
        //let val = self.stack[slots_offset + idx].clone();
        let valmeta = self.stack_meta[slots_offset + idx].clone();

        let var = self.cast_value_by_meta(slot, &valmeta, param);

        self.locals.insert(idx, var);
        // ❹ mutability メタを記憶しておきたいなら
        self.local_meta.insert(idx, valmeta);
    }

    fn cast_value_by_meta (&mut self, slot: usize, valmeta: &ValueMeta, param: Value) -> Variable
    {
        let var = Variable::new(slot);
        match valmeta.value_type {
            MvalueType::Number => {
                self.builder.declare_var(var, types::F64);
                let value = self.builder.ins().bitcast(types::F64, MemFlags::new(), param);
                self.builder.def_var(var, value);
            }
            MvalueType::String => {
                self.builder.declare_var(var, types::I64);
                let value = self.builder.ins().bitcast(types::I64, MemFlags::new(), param);
                self.builder.def_var(var, value);
            }
            MvalueType::Bool => {
                self.builder.declare_var(var, types::I64);
                let value = self.builder.ins().bitcast(types::I64, MemFlags::new(), param);
                self.builder.def_var(var, value);
            }
            MvalueType::Null => {
                self.builder.declare_var(var, types::I64);
                let value = self.builder.ins().bitcast(types::I64, MemFlags::new(), param);
                self.builder.def_var(var, value);
            }
            _ => unimplemented!("argument type {:?} not supported", valmeta.value_type),
        }
        return var;
    }

    fn emit_init_define_local(&mut self, packed: u32) {
        // ❶ is_mutable と idx を分離
        let (_is_mutable, idx) = unpack_one_flag(packed);

        let slots_offset = self.slots_offset;
        let _val = self.stack[slots_offset + idx].clone();
        let valmeta = self.stack_meta[slots_offset + idx].clone();

        // ❷ Variable を確保
        self
            .locals
            .entry(idx.try_into().unwrap())
            .or_insert_with(|| {
                let v = Variable::new(idx);
                match valmeta.value_type {
                    MvalueType::Number => {
                        self.builder.declare_var(v, types::F64);
                        let init_val = self.builder.ins().f64const(0.0);
                        self.builder.def_var(v, init_val);
                    }
                    MvalueType::String => {
                        self.builder.declare_var(v, types::I64);
                        let init_val = self.builder.ins().iconst(types::I64, 0);
                        self.builder.def_var(v, init_val);
                    }
                    MvalueType::Bool => {
                        self.builder.declare_var(v, types::I64);
                        let init_val = self.builder.ins().iconst(types::I64, 0);
                        self.builder.def_var(v, init_val);
                    }
                    MvalueType::Null => {
                        self.builder.declare_var(v, types::I64);
                        let init_val = self.builder.ins().iconst(types::I64, 0);
                        self.builder.def_var(v, init_val);
                    }
                    _ => unimplemented!("argument type {:?} not supported", valmeta.value_type),
                }
                v
            });

        // ❹ mutability メタを記憶しておきたいなら
        self.local_meta.insert(idx, valmeta);
    }

    // ───────────────────────────────────────────────
    // BinOp helper
    // ───────────────────────────────────────────────
    fn emit_binop(&mut self, op: Binop) -> StepResult<(), String> {
        let rhs_meta = self
            .operand_meta_stack
            .pop()
            .expect("compile-time operand stack underflow");
        let rhs = self
            .operand_stack
            .pop()
            .expect("compile-time operand stack underflow");

        let lhs_meta = self
            .operand_meta_stack
            .pop()
            .expect("compile-time operand stack underflow");
        let lhs = self
            .operand_stack
            .pop()
            .expect("compile-time operand stack underflow");

        if lhs_meta.value_type != MvalueType::Number || rhs_meta.value_type != MvalueType::Number {
            return StepResult::Err(format!(
                "Values of calculation must be both numbers, got {:?} and {:?}",
                lhs_meta.value_type, rhs_meta.value_type
            ));
        }

        let val = match op {
            Binop::Add => self.builder.ins().fadd(lhs, rhs),
            Binop::Sub => self.builder.ins().fsub(lhs, rhs),
            Binop::Mul => self.builder.ins().fmul(lhs, rhs),
            Binop::Div => self.builder.ins().fdiv(lhs, rhs),
            Binop::Pow => todo!(),
            Binop::Modulus => todo!(),
        };
        
        self.operand_stack.push(val);
        self.operand_meta_stack.push(ValueMeta {
            is_public: true,
            is_mutable: true,
            value_type: value::Type::Number,
        });
        StepResult::Ok(())
    }

    /// push literal null onto the compile-time operand stack
    fn emit_null(&mut self) {
        let null_val = self.builder.ins().iconst(types::I64, 0); // ← Null タグ値
        self.operand_stack.push(null_val);
        self.operand_meta_stack.push(ValueMeta {
            is_public: true,
            is_mutable: true,
            value_type: value::Type::Null,
        });
    }

    fn emit_set_local(&mut self, packed: u32) {
        let (_is_mutable, idx) = unpack_one_flag(packed);
        // ここで可変チェック
        if let Some(value_meta) = self.local_meta.get(&idx) {
            if !value_meta.is_mutable {
                // 呼び出し側にエラーを報告するホスト関数を呼ぶ、など
                unimplemented!("attempt to assign to immutable local");
            }
        }
        let var = self.locals[&idx];
        let (val, _) = self.pop();
        self.builder.def_var(var, val);
    }

    fn emit_get_local(&mut self, slot: u32) {
        // 1. Variable を取得（まだ無ければ宣言して nil=0 を入れておく）
        let mut is_null = false;
        let var = *self
            .locals
            .entry(slot.try_into().unwrap())
            .or_insert_with(|| {
                // まだ値が定義されていないローカルは null 初期化
                let v = Variable::new(slot.try_into().unwrap());
                self.builder.declare_var(v, types::I64); // 64-bit tagged Value
                let nil = self.builder.ins().iconst(types::I64, 0);
                self.builder.def_var(v, nil);
                is_null = true;
                v
            });

        // 2. その値を SSA でロード
        let val = self.builder.use_var(var);

        // 3. コンパイル時オペランドスタックへ push
        self.operand_stack.push(val);

        if is_null {
            self.operand_meta_stack.push(ValueMeta {
                is_public: true,
                is_mutable: true,
                value_type: MvalueType::Null,
            });
        } else {
            let meta = self
                .local_meta
                .get(&(slot as usize))
                .expect("local_meta not found for slot");
            self.operand_meta_stack.push(*meta);
        }
    }

    fn emit_return(&mut self) -> ValueMeta {
        // ❶ 計算結果を compile-time スタックから pop
        let (ret_val, ret_val_meta) = self.pop();

        let val = match ret_val_meta.value_type {
            MvalueType::Number => {
                self.builder.ins().bitcast(types::I64, MemFlags::new(), ret_val)
            }
            _ => ret_val,
        };

        let meta_byte = self
            .builder
            .ins()
            .iconst(types::I8, pack_meta(&ret_val_meta) as i64);
        // ❷ そのままネイティブの戻り値として返す
        self.builder.ins().return_(&[val, meta_byte]);

        return ret_val_meta;
    }

    fn close_scope_without_return(&mut self) -> ValueMeta {
        self.emit_null();
        // ❶ 計算結果を compile-time スタックから pop
        let (ret_val, ret_val_meta) = self.pop();
        let val = match ret_val_meta.value_type {
            MvalueType::Number => {
                self.builder.ins().bitcast(types::I64, MemFlags::new(), ret_val)
            }
            _ => ret_val,
        };

        let meta_byte = self
            .builder
            .ins()
            .iconst(types::I8, pack_meta(&ret_val_meta) as i64);
        
        // ❷ そのままネイティブの戻り値として返す
        self.builder.ins().return_(&[val, meta_byte]);

        return ret_val_meta;
    }

    // ───────────────────────────────────────────────
    // 小さなユーティリティ
    // ───────────────────────────────────────────────
    fn pop(&mut self) -> (Value, ValueMeta) {
        let meta = self
            .operand_meta_stack
            .pop()
            .expect("compile-time operand stack underflow");
        let value = self
            .operand_stack
            .pop()
            .expect("compile-time operand stack underflow");
        (value, meta)
    }
    // you’ll add pop()/push(), emit_binop(), emit_call(), … here
}
