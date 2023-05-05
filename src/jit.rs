use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataContext, Module};
use crate::foreign::{self, conversion::{
    print_string, print_bool,
    i64_to_bool, nil_to_i64, bool_to_i64, 
    negate, bool_not, compare_strings, 
    bits_to_f64, f64_to_i64bits, print_number, f64_pow
}};

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
        let f64_to_i64bits = f64_to_i64bits as *const u8;
        builder.symbol("f64_to_i64bits", f64_to_i64bits);
        let bool_to_bits = bool_to_i64 as *const u8;
        builder.symbol("bool_to_i64", bool_to_bits);
        let nil_to_i64 = nil_to_i64 as *const u8;
        builder.symbol("nil_to_i64", nil_to_i64);
        let i64_to_bool = i64_to_bool as *const u8;
        builder.symbol("i64_to_bool", i64_to_bool);
        let bits_to_f64 = bits_to_f64 as *const u8;
        builder.symbol("bits_to_f64", bits_to_f64);
        let print_number = print_number as *const u8;
        builder.symbol("print_number", print_number);
        let print_string = print_string as *const u8;
        builder.symbol("print_string", print_string);
        let print_bool = print_bool as *const u8;
        builder.symbol("print_bool", print_bool);
        let negate = negate as *const u8;
        builder.symbol("negate", negate);
        let bool_not = bool_not as *const u8;
        builder.symbol("bool_not", bool_not);
        let compare_strings = compare_strings as *const u8;
        builder.symbol("compare_strings", compare_strings);
        let f64_pow = f64_pow as *const u8;
        builder.symbol("f64_pow", f64_pow);

        let module = JITModule::new(builder);
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_ctx: DataContext::new(),
            module,
        }
    }
}
