use cranelift::{codegen::ir::InstructionData, prelude::types};
use cranelift_frontend::FunctionBuilder;
use cranelift_jit::JITModule;

use std::collections::HashMap;

use crate::bytecode::values::value::Type as MvalueType;
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
}

impl<'fb, 'mval, 'h> FunctionTranslator<'fb, 'mval, 'h> {
    /// High-level entry point the JIT calls.
    /// Consumes `self`, walks the byte-code, fills `builder.func`,
    /// and then finalises the IR.
    pub fn translate(mut self, chunk: &Chunk) -> ValueMeta {
        let meta = self.lower_chunk(chunk);
        self.builder.finalize();
        return meta;
    }

    // -----------------------------------------------------------------
    // Private helpers
    // -----------------------------------------------------------------
    fn lower_chunk(&mut self, chunk: &Chunk) -> ValueMeta {
        // 1. エントリブロック
        let entry = self.builder.create_block();
        self.builder.append_block_params_for_function_params(entry);
        self.builder.switch_to_block(entry);

        // ① ここで関数パラメータを Variable に結び付ける
        for slot in 1..self.builder.block_params(entry).len() {
            // ── ① この行の &[] 借用はすぐにドロップされる ──
            let param_val = self.builder.block_params(entry)[slot];

            // ── ② ここからは &mut self.builder を使える ─────────
            let var = Variable::new(slot);
            self.builder.declare_var(var, types::I64);
            self.builder.def_var(var, param_val);
            self.locals.insert(slot, var);
            self.local_meta.insert(
                slot,
                ValueMeta {
                    is_public: true,
                    is_mutable: true,
                    value_type: MvalueType::Null,
                },
            );
        }

        self.builder.seal_block(entry);

        // init all variables
        for inst in &chunk.code {
            //println!("{:?}", inst);
            match inst.opcode {
                Opcode::DefineArgumentLocal => {
                    let slot = inst.operand;
                    self.emit_init_define_argument(slot)
                }
                Opcode::DefineLocal => {
                    let slot = inst.operand;
                    self.emit_init_define_local(slot)
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
        // 2. run code
        for inst in &chunk.code {
            //println!("{:?}", inst);
            match inst.opcode {
                Opcode::Add => self.emit_binop(Binop::Add),
                Opcode::Subtract => self.emit_binop(Binop::Sub),
                Opcode::Multiply => self.emit_binop(Binop::Mul),
                Opcode::Divide => self.emit_binop(Binop::Div),
                Opcode::Modulus => self.emit_binop(Binop::Modulus),
                Opcode::Pow => self.emit_binop(Binop::Pow),
                Opcode::DefineArgumentLocal => {
                    let slot = inst.operand;
                    self.emit_define_argument(slot)
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
                    meta = self.emit_return();
                    break;
                }
                Opcode::Pop => {
                    self.pop();
                }
                Opcode::Constant => {
                    self.constant(inst.operand, chunk);
                }
                // 他の opcode は後で
                _ => unimplemented!("{:?}", inst.opcode),
            }
        }

        // ❸ 以降の IR は到達不能になるのでブロックを閉じておくと親切
        // （必須ではないが verifier が喜ぶ）
        self.builder.seal_all_blocks();
        return meta;
    }

    fn constant(&mut self, idx: u32, chunk: &Chunk) {
        let val = &chunk.constants[idx as usize];

        let raw_i64 = match val {
            // ── f64 を 64-bit 生ビットへ
            Constant::Number(f) => f.to_bits() as i64,

            // ── TODO: ヒープ値は (heap_id<<3)|TAG_PTR など
            other => unimplemented!("constant {:?} not lowered yet", other),
        };

        // 即値を IR に
        let v = self.builder.ins().iconst(types::I64, raw_i64);
        self.operand_stack.push(v);

        // meta 配列を使う場合はタグを同期 push
        // self.operand_meta_stack.push(ValueMeta {
        //     tag: Tag::Number, is_public: true, is_mutable: true
        // });
    }

    fn emit_define_argument(&mut self, packed: u32) {
        // ❶ is_mutable と idx を分離
        let (_is_mutable, idx) = unpack_one_flag(packed);

        // ❷ Variable を確保
        let var = *self.locals.get(&idx).expect("local variable not found");

        // ❸ 初期値を決める
        let init_val = {
            let slots_offset = self.slots_offset;
            let val = self.stack[slots_offset + idx].clone();
            let valmeta = self.stack_meta[slots_offset + idx].clone();
            match val {
                Marieval::Number(f) => {
                    self.local_meta.entry(idx).and_modify(|v| *v = valmeta);
                    let bits: i64 = f.to_bits() as i64;
                    self.builder.ins().iconst(types::I64, bits)
                }
                Marieval::String(name_id) => {
                    self.local_meta.entry(idx).and_modify(|v| *v = valmeta);
                    //let string: String = self.heap.get_str(name_id).clone();
                    self.builder.ins().iconst(types::I64, name_id as i64)
                }
                _ => todo!(),
            }
        };
        self.builder.def_var(var, init_val);
    }

    // すでにある locals: HashMap<u32, Variable>
    fn emit_define_local(&mut self, packed: u32, init_from_stack: bool) {
        // ❶ is_mutable と idx を分離
        let (_is_mutable, idx) = unpack_one_flag(packed);

        // ❷ Variable を確保
        let var = *self.locals.get(&idx).expect("local variable not found");

        // ❸ 初期値を決める
        let init_val = if init_from_stack {
            self.pop()
        } else {
            self.builder.ins().iconst(types::I64, 0) // null
        };
        self.builder.def_var(var, init_val);
    }

    fn emit_init_define_argument(&mut self, packed: u32) {
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

        // ❸ 初期値を決める
        let init_val = self.builder.ins().iconst(types::I64, 0);
        self.builder.def_var(var, init_val);

        let slots_offset = self.slots_offset;
        let _val = self.stack[slots_offset + idx].clone();
        let valmeta = self.stack_meta[slots_offset + idx].clone();

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
    fn emit_binop(&mut self, op: Binop) {
        let rhs = self.pop();
        let rhs = self.builder.ins().bitcast(types::F64, MemFlags::new(), rhs);
        let lhs = self.pop();
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
        let val = self.pop();
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
        let ret_val_meta = self
            .operand_meta_stack
            .pop()
            .expect("compile-time operand stack underflow");
        let ret_val = self
            .operand_stack
            .pop()
            .expect("compile-time operand stack underflow");

        // ❷ そのままネイティブの戻り値として返す
        self.builder.ins().return_(&[ret_val]);

        return ret_val_meta;
    }

    // ───────────────────────────────────────────────
    // 小さなユーティリティ
    // ───────────────────────────────────────────────
    fn pop(&mut self) -> Value {
        self.operand_meta_stack
            .pop()
            .expect("compile-time operand stack underflow");
        self.operand_stack
            .pop()
            .expect("compile-time operand stack underflow")
    }
    // you’ll add pop()/push(), emit_binop(), emit_call(), … here
}
