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
    then_block: Block,
    end_block: Block,
}

pub struct FunctionTranslator<'fb, 'mval, 'h> {
    pub builder: FunctionBuilder<'fb>,
    pub operand_stack: Vec<Value>,
    pub operand_meta_stack: Vec<ValueMeta>, // TODO
    pub locals: std::collections::HashMap<usize, Variable>,
    pub local_meta: std::collections::HashMap<usize, ValueMeta>,
    pub stack: &'mval mut Vec<Marieval>,
    pub stack_meta: &'mval mut Vec<ValueMeta>,
    pub heap: &'h mut gc::Heap,
    pub slots_offset: usize,
    pub if_block_stack: Vec<IfElseBlock>,
}

impl<'fb, 'mval, 'h> FunctionTranslator<'fb, 'mval, 'h> {
    /// High-level entry point the JIT calls.
    /// Consumes `self`, walks the byte-code, fills `builder.func`,
    /// and then finalises the IR.
    pub fn translate(mut self, chunk: &Chunk) -> StepResult<ValueMeta, InterpreterError> {
        let meta = self.lower_chunk(chunk);
        match meta {
            StepResult::Err(err) => {
                return StepResult::Err(err);
            }
            StepResult::Ok(m) => {
                self.builder.finalize();
                return StepResult::Ok(m);
            }
            StepResult::OkReturn(m) => {
                self.builder.finalize();
                return StepResult::OkReturn(m);
            }
        }
    }

    // -----------------------------------------------------------------
    // Private helpers
    // -----------------------------------------------------------------
    fn lower_chunk(&mut self, chunk: &Chunk) -> StepResult<ValueMeta, InterpreterError> {
        // 1. エントリブロック
        let entry = self.builder.create_block();
        self.builder.append_block_params_for_function_params(entry);
        self.builder.switch_to_block(entry);

        // init all variables
        let mut i = 1;
        for inst in &chunk.code {
            //println!("{:?}", inst);
            match inst.opcode {
                Opcode::DefineArgumentLocal => {
                    let param = self.builder.block_params(entry)[i];
                    let meta_param = self.builder.block_params(entry)[i + 1];
                    let slot = inst.operand;
                    self.emit_init_define_argument(slot, param, meta_param, i);
                    i = i + 2;
                }
                // 他の opcode は後で
                _ => {}
            }
        }

        self.builder.seal_block(entry);
        let mut current_end_block = self.builder.create_block();
        let mut is_first_if = true;
        for inst in &chunk.code {
            //println!("{:?}", inst);
            match inst.opcode {
                Opcode::DefineLocal => {
                    let slot = inst.operand;
                    self.emit_init_define_local(slot)
                }
                Opcode::BeginIf => {
                    // if 本体の then
                    let then_block = self.builder.create_block();

                    // else/elseif 連鎖の出口は最初だけ確保、
                    // 2 個目以降は流用
                    if !is_first_if {
                        current_end_block = self.builder.create_block();
                    }
                    is_first_if = false;

                    self.if_block_stack.insert(
                        self.if_block_stack.len(),
                        IfElseBlock {
                            then_block,
                            end_block: current_end_block,
                        },
                    );
                }
                Opcode::BeginElseIf => {
                    let if_condition_block = self.builder.create_block();
                    // else-if 判定ブロック
                    let cond_block = self.builder.create_block();
                    // else-if の then ブロック
                    let then_block = self.builder.create_block();

                    self.if_block_stack.insert(
                        self.if_block_stack.len(),
                        IfElseBlock {
                            then_block: if_condition_block,
                            end_block: current_end_block,
                        },
                    );

                    self.if_block_stack.insert(
                        self.if_block_stack.len(),
                        IfElseBlock {
                            then_block: cond_block,
                            end_block: current_end_block,
                        },
                    );

                    self.if_block_stack.insert(
                        self.if_block_stack.len(),
                        IfElseBlock {
                            then_block,
                            end_block: current_end_block,
                        },
                    );
                }
                Opcode::BeginElse => {
                    let else_block = self.builder.create_block();
                    self.if_block_stack.insert(
                            self.if_block_stack.len(),
                            IfElseBlock {
                            then_block: else_block,
                            end_block:  current_end_block,
                        },
                    );
                }
                Opcode::EndIf => {
                    if inst.operand == 0 /* has_else == false */ {
                        // else の代わりに end ブロックを queue に置いておく
                        self.if_block_stack.insert(
                            self.if_block_stack.len(),
                            IfElseBlock {
                                then_block: current_end_block,
                                end_block:  current_end_block,
                            },
                        );
                    }
                }
                // 他の opcode は後で
                _ => {}
            }
        }

        let mut meta = ValueMeta {
            is_public: true,
            is_mutable: true,
            value_type: MvalueType::Null,
        };
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
                    meta = self.emit_return();
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
                        meta = self.close_scope_without_return();
                    }
                }
                Opcode::BeginIf => {
                    let (condition, _) = self.pop();
                    let _has_else = inst.operand == 1;
                    let then_block = self
                        .if_block_stack[0];
                    //self.builder.ins().brz(condition_jit_value, first_else_if, &[condition_jit_value]);
                    let end_ifelse_block = IfElseBlock {
                        then_block: then_block.end_block,
                        end_block: then_block.end_block,
                    };
                    // 0 is end block, 1 is then block
                    let next_else_if = self.if_block_stack.get(2).unwrap_or(
                        &end_ifelse_block
                    );
                    self.builder.ins().brif(
                        condition,
                        then_block.then_block,
                        &[],
                        next_else_if.then_block,
                        &[],
                    );
                    self.builder.switch_to_block(then_block.then_block);
                }
                Opcode::EndIf => {
                    // previous if block
                    let _previous_if_condition_block = self.if_block_stack.remove(0);
                    // then block
                    let _previous_if_block = self.if_block_stack.remove(0);
                }
                Opcode::PrepareElseIf => {
                    let next_if = self.if_block_stack.remove(0);
                    let condition_else_if_block = next_if.then_block;

                    // Compile else-if block
                    self.builder.switch_to_block(condition_else_if_block);
                }
                Opcode::BeginElseIf => {
                    let (condition, _) = self.pop();
                    let _has_else = inst.operand == 1;
                    let then_block = self
                        .if_block_stack[0];
                    let end_ifelse_block = IfElseBlock {
                        then_block: then_block.end_block,
                        end_block: then_block.end_block,
                    };
                    // 0 is else if then block, 1 is next block
                    let next_else_if = self.if_block_stack.get(1).unwrap_or(
                        &end_ifelse_block
                    );
                    self.builder.ins().brif(
                        condition,
                        then_block.then_block,
                        &[],
                        next_else_if.then_block,
                        &[],
                    );
                    self.builder.switch_to_block(then_block.then_block);
                }
                Opcode::EndElseIf => {
                    let _previous_if_block = self.if_block_stack.remove(0);
                }
                Opcode::BeginElse => {
                    let end_block = self
                        .if_block_stack[0];
                    self.builder.switch_to_block(end_block.then_block);
                }
                Opcode::EndAllIf => {
                    //let (value, meta) = self.pop();
                    let end_block = self
                        .if_block_stack
                        .remove(0);
                    self.builder.seal_block(end_block.then_block);
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
        return StepResult::Ok(meta);
    }

    fn constant(&mut self, idx: u32, chunk: &Chunk) {
        let val = &chunk.constants[idx as usize];

        let (raw_i64, meta) = match val {
            // ── f64 を 64-bit 生ビットへ
            Constant::Number(f) => (
                f.to_bits() as i64,
                ValueMeta {
                    is_public: true,
                    is_mutable: true,
                    value_type: MvalueType::Number,
                },
            ),
            // TODO:
            other => unimplemented!("constant {:?} not lowered yet", other),
        };

        // 即値を IR に
        let v = self.builder.ins().iconst(types::I64, raw_i64);
        self.operand_stack.push(v);

        // meta 配列を使う場合はタグを同期 push
        self.operand_meta_stack.push(meta);
    }

    fn emit_bool(&mut self, b: bool) {
        // 0 / 1 をそのまま i64 即値に
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

        let var = Variable::new(slot);
        self.builder.declare_var(var, types::I64);
        self.builder.def_var(var, param);

        // ❸ 初期値を決める
        let slots_offset = self.slots_offset;
        //let val = self.stack[slots_offset + idx].clone();
        let valmeta = self.stack_meta[slots_offset + idx].clone();
        self.locals.insert(idx, var);
        // ❹ mutability メタを記憶しておきたいなら
        self.local_meta.insert(idx, valmeta);
    }

    fn emit_init_define_local(&mut self, packed: u32) {
        // ❶ is_mutable と idx を分離
        let (_is_mutable, idx) = unpack_one_flag(packed);

        // ❷ Variable を確保
        let var = *self
            .locals
            .entry(idx.try_into().unwrap())
            .or_insert_with(|| {
                let v = Variable::new(idx);
                self.builder.declare_var(v, types::I64);
                v
            });

        // ❸ 初期値
        let init_val = self.builder.ins().iconst(types::I64, 0);
        self.builder.def_var(var, init_val);

        let slots_offset = self.slots_offset;
        let _val = self.stack[slots_offset + idx].clone();
        let valmeta = self.stack_meta[slots_offset + idx].clone();

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

        let rhs = self.builder.ins().bitcast(types::F64, MemFlags::new(), rhs);
        let lhs = self.builder.ins().bitcast(types::F64, MemFlags::new(), lhs);

        let val = match op {
            Binop::Add => self.builder.ins().fadd(lhs, rhs),
            Binop::Sub => self.builder.ins().fsub(lhs, rhs),
            Binop::Mul => self.builder.ins().fmul(lhs, rhs),
            Binop::Div => self.builder.ins().fdiv(lhs, rhs),
            Binop::Pow => todo!(),
            Binop::Modulus => todo!(),
        };
        let answer = self.builder.ins().bitcast(types::I64, MemFlags::new(), val);
        self.operand_stack.push(answer);
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

        let meta_byte = self
            .builder
            .ins()
            .iconst(types::I8, pack_meta(&ret_val_meta) as i64);
        // ❷ そのままネイティブの戻り値として返す
        self.builder.ins().return_(&[ret_val, meta_byte]);

        return ret_val_meta;
    }

    fn close_scope_without_return(&mut self) -> ValueMeta {
        self.emit_null();
        // ❶ 計算結果を compile-time スタックから pop
        let (ret_val, ret_val_meta) = self.pop();

        let meta_byte = self
            .builder
            .ins()
            .iconst(types::I8, pack_meta(&ret_val_meta) as i64);
        // ❷ そのままネイティブの戻り値として返す
        self.builder.ins().return_(&[ret_val, meta_byte]);

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
