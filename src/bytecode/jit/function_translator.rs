use cranelift::{codegen::ir::InstructionData, prelude::types};
use cranelift_frontend::FunctionBuilder;
use cranelift_jit::JITModule;

use std::collections::HashMap;

use cranelift::{
    prelude::*,
    codegen::ir::{ExternalName, FuncRef, Inst},
};
use cranelift_module::{Linkage, Module};
use crate::bytecode::{bytecode::{unpack_one_flag, Chunk, Opcode, ValueMeta}, values::{value::Value as Marieval, Binop}};

pub struct FunctionTranslator<'fb, 'mval> {
    pub builder: FunctionBuilder<'fb>,
    pub operand_stack: Vec<Value>,
    pub locals: std::collections::HashMap<usize, Variable>,
    pub local_mutable: std::collections::HashMap<usize, bool>,
    pub local_id: usize,
    pub stack: &'mval mut Vec<Marieval>,
    pub stack_meta: &'mval mut Vec<ValueMeta>,
    pub slots_offset: usize,
}

impl<'fb, 'mval> FunctionTranslator<'fb, 'mval> {
    /// High-level entry point the JIT calls.
    /// Consumes `self`, walks the byte-code, fills `builder.func`,
    /// and then finalises the IR.
    pub fn translate(mut self, chunk: &Chunk) {
        self.lower_chunk(chunk);
        self.builder.finalize(); 
    }
    
    // -----------------------------------------------------------------
    // Private helpers
    // -----------------------------------------------------------------
    fn lower_chunk(&mut self, chunk: &Chunk) {
        // 1. エントリブロック
        let entry = self.builder.create_block();
        self.builder.append_block_params_for_function_params(entry);
        self.builder.switch_to_block(entry);

        // ① ここで関数パラメータを Variable に結び付ける
        // block_param[0] = &mut Interpreter
        // block_param[1] = n
        // block_param[2] = n2
        for slot in 1..self.builder.block_params(entry).len() {
            // ── ① この行の &[] 借用はすぐにドロップされる ──
            let param_val = self.builder.block_params(entry)[slot];

            // ── ② ここからは &mut self.builder を使える ─────────
            let var = Variable::new(slot);
            self.builder.declare_var(var, types::I64);
            self.builder.def_var(var, param_val);
            self.locals.insert(slot, var);
            self.local_mutable.insert(slot, true);
        }

        self.builder.seal_block(entry);
        
        // 2. ここでは単に param1 + param2 を返すデモ
        for inst in &chunk.code {
            //println!("{:?}", inst);
            match inst.opcode {
                Opcode::Add => self.emit_binop(Binop::Add),
                Opcode::DefineArgumentLocal => {
                    let slot = inst.operand;
                    self.emit_define_argument(slot)
                },
                Opcode::DefineLocal => {
                    let slot = inst.operand;
                    self.emit_define_local(slot, false)
                },
                Opcode::GetLocal => {
                    let slot = inst.operand;
                    self.emit_get_local(slot);
                },
                Opcode::SetLocal => {
                    let slot = inst.operand;
                    self.emit_set_local(slot);
                },
                Opcode::Null => self.emit_null(),
                Opcode::Return => {
                    self.emit_return();
                    break;
                },
                // 他の opcode は後で
                _ => unimplemented!("{:?}", inst.opcode),
            }
        }
    }

    fn emit_define_argument(&mut self, packed: u32) {
        // ❶ is_mutable と idx を分離
        let (is_mutable, idx) = unpack_one_flag(packed);

        // ❷ Variable を確保
        let var = *self.locals.entry(idx.try_into().unwrap()).or_insert_with(|| {
            let v = Variable::new(idx);
            self.builder.declare_var(v, types::I64);
            v
        });

        // ❸ 初期値を決める
        let init_val = {
            let slots_offset = self.slots_offset;
            let val = self.stack[slots_offset + idx].clone();
            match val {
                Marieval::Number(f) => {
                    let bits: i64 = f.to_bits() as i64;
                    self.builder.ins().iconst(types::I64, bits)
                },
                _ => todo!()
            } 
            
        };
        self.builder.def_var(var, init_val);

        // ❹ mutability メタを記憶しておきたいなら
        self.local_mutable.insert(idx, is_mutable);
    }

    // すでにある locals: HashMap<u32, Variable>
    fn emit_define_local(&mut self, packed: u32, init_from_stack: bool) {
        // ❶ is_mutable と idx を分離
        let (is_mutable, idx) = unpack_one_flag(packed);

        // ❷ Variable を確保
        let var = *self.locals.entry(idx.try_into().unwrap()).or_insert_with(|| {
            let v = Variable::new(idx);
            self.builder.declare_var(v, types::I64);
            v
        });

        // ❸ 初期値を決める
        let init_val = if init_from_stack {
            self.pop()
        } else {
            self.builder.ins().iconst(types::I64, 0) // nil
        };
        self.builder.def_var(var, init_val);

        // ❹ mutability メタを記憶しておきたいなら
        self.local_mutable.insert(idx, is_mutable);
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
            Binop::Sub => todo!(),
            Binop::Mul => todo!(),
            Binop::Div => todo!(),
            Binop::Pow => todo!(),
            Binop::Modulus => todo!(),
        };
        let answer = self.builder.ins().bitcast(types::I64, MemFlags::new(), val);
        self.operand_stack.push(answer);
    }

    /// push literal null onto the compile-time operand stack
    fn emit_null(&mut self) {
        let null_val = self.builder.ins().iconst(types::I64, 0);   // ← Null タグ値
        self.operand_stack.push(null_val);
    }

    fn emit_set_local(&mut self, packed: u32) {
        let (_is_mutable, idx) = unpack_one_flag(packed);
        // ここで可変チェック
        if let Some(false) = self.local_mutable.get(&idx) {
            // 呼び出し側にエラーを報告するホスト関数を呼ぶ、など
            unimplemented!("attempt to assign to immutable local");
        }
        let var = self.locals[&idx];
        let val = self.pop();
        self.builder.def_var(var, val);
    }

    fn emit_get_local(&mut self, slot: u32) {
        // 1. Variable を取得（まだ無ければ宣言して nil=0 を入れておく）
        let var = *self.locals.entry(slot.try_into().unwrap()).or_insert_with(|| {
            // まだ値が定義されていないローカルは nil 初期化
            let v = Variable::new(slot.try_into().unwrap());
            self.builder.declare_var(v, types::I64);          // 64-bit tagged Value
            let nil = self.builder.ins().iconst(types::I64, 0);
            self.builder.def_var(v, nil);
            v
        });

        // 2. その値を SSA でロード
        let val = self.builder.use_var(var);

        // 3. コンパイル時オペランドスタックへ push
        self.operand_stack.push(val);
    }

    fn emit_return(&mut self) {
        // ❶ 計算結果を compile-time スタックから pop
        let ret_val = self.pop(); // ← self.pop() は underflow チェック付き

        // ❷ そのままネイティブの戻り値として返す
        self.builder.ins().return_(&[ret_val]);

        // ❸ 以降の IR は到達不能になるのでブロックを閉じておくと親切
        // （必須ではないが verifier が喜ぶ）
        self.builder.seal_all_blocks();
    }

    // ───────────────────────────────────────────────
    // 小さなユーティリティ
    // ───────────────────────────────────────────────
    fn pop(&mut self) -> Value {
        self.operand_stack
            .pop()
            .expect("compile-time operand stack underflow")
    }
    // you’ll add pop()/push(), emit_binop(), emit_call(), … here
}
