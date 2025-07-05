use cranelift::{codegen::ir::InstructionData, prelude::types};
use cranelift_frontend::FunctionBuilder;
use cranelift_jit::JITModule;

use std::collections::HashMap;

use cranelift::{
    prelude::*,
    codegen::ir::{ExternalName, FuncRef, Inst},
};
use cranelift_module::{Linkage, Module};
use crate::bytecode::{bytecode::{self, Chunk}, values::value as marieval};

/// Compile-time scratch: maps local index → Cranelift `Variable`
type LocalMap = HashMap<u32, Variable>;

pub struct FunctionTranslator<'m, 'fb> {
    // ---- Cranelift handles -------------------------------------------------
    module:  &'m mut JITModule,
    builder: &'fb mut FunctionBuilder<'fb>,

    // ---- scratch for this one function ------------------------------------
    operand_stack: Vec<Value>,        // SSA handles (compile-time only)
    locals:        LocalMap,
}


