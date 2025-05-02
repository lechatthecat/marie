use crate::reader::value;

use super::bytecode::{self, ValueMeta};
use crate::bytecode::bytecode::Order;

#[derive(Default)]
pub struct CallFrame {
    pub closure: value::Closure,
    pub instruction_pointer: usize,
    pub slots_offset: usize,
    pub invoked_method_id: Option<usize>,
    pub is_include_file: bool, 
}

impl CallFrame {
    pub fn next_op(&self) -> Order {
        self.closure.function.chunk.code[self.instruction_pointer].clone()
    }

    pub fn next_op_and_advance(&mut self) -> Order {
        let res = self.next_op();
        self.instruction_pointer += 1;
        res
    }

    pub fn read_constant(&self, idx: usize) -> bytecode::Constant {
        self.closure.function.chunk.constants[idx].clone()
    }

    pub fn read_constant_meta(&self, idx: usize) -> ValueMeta {
        self.closure.function.chunk.constant_metas[idx].clone()
    }

    pub fn set_constant_meta(&mut self, idx: usize, meta: ValueMeta) {
        self.closure.function.chunk.constant_metas[idx] = meta;
    }

    pub fn _is_constants_empty(&self) -> bool {
        self.closure.function.chunk.constants.is_empty()
    }
}
