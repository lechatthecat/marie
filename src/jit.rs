use core::mem;
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataContext, Linkage, Module};
use std::collections::HashMap;
use std::slice;
use crate::foreign::{self, conversion::{
    is_f64,
    i64_to_i64,
    f64_to_jit_val,
    string_to_jit_val,
    print_string_jitval,
}};
use foreign::conversion::{bits_to_f64, f64_to_bits, print_jitval};

/// The basic JIT class.
pub struct JIT {
    /// The function builder context, which is reused across multiple
    /// FunctionBuilder instances.
    pub builder_context: FunctionBuilderContext,

    /// The main Cranelift context, which holds the state for codegen. Cranelift
    /// separates this from `Module` to allow for parallel compilation, with a
    /// context per thread, though this isn't in the simple demo here.
    pub ctx: codegen::Context,

    /// The data context, which is to data objects what `ctx` is to functions.
    pub data_ctx: DataContext,

    /// The module, with the jit backend, which manages the JIT'd
    /// functions.
    pub module: JITModule,
}

impl Default for JIT {
    fn default() -> Self {
        let mut builder = JITBuilder::new(cranelift_module::default_libcall_names()).unwrap();
        let f64_to_bits = f64_to_bits as *const u8;
        builder.symbol("f64_to_bits", f64_to_bits);
        let bits_to_f64 = bits_to_f64 as *const u8;
        builder.symbol("bits_to_f64", bits_to_f64);
        let print_jitval = print_jitval as *const u8;
        builder.symbol("print_jitval", print_jitval);
        let is_f64 = is_f64 as *const u8;
        builder.symbol("is_f64", is_f64);
        let i64_to_i64 = i64_to_i64 as *const u8;
        builder.symbol("i64_to_i64", i64_to_i64);
        let f64_to_jit_val = f64_to_jit_val as *const u8;
        builder.symbol("f64_to_jit_val", f64_to_jit_val);
        let string_to_jit_val = string_to_jit_val as *const u8;
        builder.symbol("string_to_jit_val", string_to_jit_val);
        let print_string_jitval = print_string_jitval as *const u8;
        builder.symbol("print_string_jitval", print_string_jitval);
        
        let module = JITModule::new(builder);
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_ctx: DataContext::new(),
            module,
        }
    }
}
