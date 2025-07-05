use std::collections::HashMap;
use cranelift::{codegen::ir::FuncRef, prelude::*};
use cranelift_frontend::{FunctionBuilder, Variable};
use cranelift_jit::JITModule;

struct Translator<'f> {
    builder: &'f mut FunctionBuilder<'f>,
    stack: Vec<Value>,
    lookup:  fn(&str) -> FuncRef,
}

impl<'f> Translator<'f> {
    fn emit_call(&mut self, name: &str, args: &[Value]) {
        let callee = (self.lookup)(name);
        self.builder.ins().call(callee, args);
    }
}
