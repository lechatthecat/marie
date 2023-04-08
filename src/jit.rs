use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataContext, Module};
use crate::foreign::{self, conversion::{
    f64_to_jitval,
    string_to_jitval,
    print_string_jitval,
    printtest, print_bool_jitval,
    i64_to_bool, nil_to_jitval, bool_to_jitval, marieval_to_f64, marieval_to_heap_string, marieval_to_bool, negate, bool_not, compare_strings, bool_to_bits
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
        let bits_to_f64 = bits_to_f64 as *const u8;
        builder.symbol("bits_to_f64", bits_to_f64);
        let f64_to_bites = f64_to_bits as *const u8;
        builder.symbol("f64_to_bits", f64_to_bites);
        let bool_to_bits = bool_to_bits as *const u8;
        builder.symbol("bool_to_bits", bool_to_bits);
        let print_jitval = print_jitval as *const u8;
        builder.symbol("print_jitval", print_jitval);
        let f64_to_jitval = f64_to_jitval as *const u8;
        builder.symbol("f64_to_jitval", f64_to_jitval);
        let string_to_jitval = string_to_jitval as *const u8;
        builder.symbol("string_to_jitval", string_to_jitval);
        let print_string_jitval = print_string_jitval as *const u8;
        builder.symbol("print_string_jitval", print_string_jitval);
        let printtest = printtest as *const u8;
        builder.symbol("printtest", printtest);
        let print_bool_jitval = print_bool_jitval as *const u8;
        builder.symbol("print_bool_jitval", print_bool_jitval);
        let i64_to_bool = i64_to_bool as *const u8;
        builder.symbol("i64_to_bool", i64_to_bool);
        let nil_to_jitval = nil_to_jitval as *const u8;
        builder.symbol("nil_to_jitval", nil_to_jitval);
        let bool_to_jitval = bool_to_jitval as *const u8;
        builder.symbol("bool_to_jitval", bool_to_jitval);
        let marieval_to_f64 = marieval_to_f64 as *const u8;
        builder.symbol("marieval_to_f64", marieval_to_f64);
        let marieval_to_heap_string = marieval_to_heap_string as *const u8;
        builder.symbol("marieval_to_heap_string", marieval_to_heap_string);
        let marieval_to_bool = marieval_to_bool as *const u8;
        builder.symbol("marieval_to_bool", marieval_to_bool);
        let negate = negate as *const u8;
        builder.symbol("negate", negate);
        let bool_not = bool_not as *const u8;
        builder.symbol("bool_not", bool_not);
        let compare_strings = compare_strings as *const u8;
        builder.symbol("compare_strings", compare_strings);

        let module = JITModule::new(builder);
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_ctx: DataContext::new(),
            module,
        }
    }
}
