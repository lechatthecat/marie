use std::slice;

use cranelift::prelude::*;
use cranelift::{codegen, prelude::{settings, Configurable}};
use cranelift_codegen::ir::{FuncRef, UserFuncName};
use cranelift_codegen::isa::CallConv;
use cranelift_frontend::FunctionBuilderContext;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, Linkage, Module};
use crate::bytecode::jit::rust_defined_functions::*;
use crate::bytecode::values::value::Value as Marieval;

use crate::bytecode::bytecode::{Chunk, ValueMeta};
use crate::bytecode::bytecode_interpreter::{Interpreter, InterpreterError};
use crate::bytecode::jit::function_translator::FunctionTranslator;
use crate::gc::gc;

/// The basic JIT class.
pub struct JIT {
    /// The function builder context, which is reused across multiple
    /// FunctionBuilder instances.
    pub builder_context: FunctionBuilderContext,

    /// The main Cranelift context, which holds the state for codegen. Cranelift
    /// separates this from `Module` to allow for parallel compilation, with a
    /// context per thread, though this isn't in the simple demo here.
    pub ctx: codegen::Context,

    /// The data description, which is to data objects what `ctx` is to functions.
    pub data_description: DataDescription,

    /// The module, with the jit backend, which manages the JIT'd
    /// functions.
    pub module: JITModule,
}

impl Default for JIT {
    fn default() -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        flag_builder.set("opt_level", "speed_and_size").unwrap(); 

        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();
        let mut builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
        builder.symbol("native_print", native_print as *const u8);

        let module = JITModule::new(builder);
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_description: DataDescription::new(),
            module,
        }
    }
}

impl JIT {
    pub fn isa(&self) -> &dyn cranelift::codegen::isa::TargetIsa { self.module.isa() }
    pub fn ptr_type(&self) -> types::Type { self.module.isa().pointer_type() }
    pub fn compile_chunk(
        &mut self,
        func_id: gc::HeapId,
        name: &str,
        arity: usize,
        chunk: &Chunk,
        slots_offset: usize,
        stack: &mut Vec<Marieval>,
        stack_meta: &mut Vec<ValueMeta>,
        heap: &mut gc::Heap,
    ) -> Result<cranelift_module::FuncId, InterpreterError> {
        // 1. fresh signature
        self.ctx.func.signature = self.build_signature(arity);
        self.ctx.func.name = UserFuncName::user(0, func_id as u32); // or stable id

        {
            let print_func = self.define_print();

            // 2. build
            let mut builder =
                FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
            // 1. エントリブロック
            let entry = builder.create_block();
            builder.append_block_params_for_function_params(entry);
            builder.switch_to_block(entry);
            let tx = FunctionTranslator {
                builder,
                operand_stack: Vec::new(),
                operand_meta_stack: Vec::new(),
                local_meta: Default::default(),
                locals: Default::default(),
                stack,
                stack_meta,
                slots_offset,
                heap,
                if_block_stack: Vec::new(),
                entry_block: entry,
                funcs: vec![print_func]
            };
            match tx.translate(chunk) {
                crate::bytecode::StepResult::Ok(_) => {},
                crate::bytecode::StepResult::OkReturn(_) => {},
                crate::bytecode::StepResult::Err(err) => {
                    self.module.clear_context(&mut self.ctx);
                    return Err(err);
                },
            };
        }

        // display cranelift IR
        // println!("{}", self.ctx.func.display());

        if let Err(e) = self.ctx.verify(self.module.isa()) {
            self.module.clear_context(&mut self.ctx);
            return Err(InterpreterError::Runtime(e.to_string()));
        }

        // 3. hand IR to Cranelift
        let func_id = self
            .module
            .declare_function(name, Linkage::Export, &self.ctx.func.signature)
            .unwrap();
        self.module
            .define_function(func_id, &mut self.ctx).unwrap();
        let _result = self.module.finalize_definitions();
        self.module.clear_context(&mut self.ctx);   // reuse for next fn

        Ok(func_id)
    }

    fn build_signature(&self, arity: usize) -> Signature {
        let mut sig = self.module.make_signature();

        // Pointer type for the target ISA (x86_64 = I64, aarch64 = I64, …)
        let ptr_ty = self.module.isa().pointer_type();

        // 1.  &mut Interpreter  (implicit “vm state” param)
        sig.params.push(AbiParam::new(ptr_ty));

        // 2.  Positional script arguments
        for _ in 0..arity {
            sig.params.push(AbiParam::new(types::I64));  // or F64 if NaN-tagging
            sig.params.push(AbiParam::new(types::I64));
        }

        // 3.  Return value (one Value slot)
        sig.returns.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));

        sig.call_conv = CallConv::SystemV;

        sig
    }

    fn define_print(&mut self) -> FuncRef {
        let mut print_sig = self.module.make_signature();
        print_sig.call_conv = CallConv::SystemV;
        let ptr_ty = self.ptr_type();
        print_sig.params.push(AbiParam::new(ptr_ty));
        print_sig.params.push(AbiParam::new(types::I64));
        print_sig.params.push(AbiParam::new(types::I64));
        let print_fn = self
            .module
            .declare_function("native_print", Linkage::Import, &print_sig)
            .unwrap();
        let print_fn_ref = self
            .module
            .declare_func_in_func(print_fn, &mut self.ctx.func);

        return print_fn_ref;
    }

}
