use std::slice;

use cranelift::prelude::*;
use cranelift::{codegen, prelude::{settings, Configurable}};
use cranelift_codegen::ir::UserFuncName;
use cranelift_frontend::FunctionBuilderContext;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, Linkage, Module};
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
        let builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

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
        name: &str,
        arity: u8,
        chunk: &Chunk,
        slots_offset: usize,
        stack: &mut Vec<Marieval>,
        stack_meta: &mut Vec<ValueMeta>,
        heap: &mut gc::Heap,
    ) -> Result<(ValueMeta, cranelift_module::FuncId), InterpreterError> {
        // 1. fresh signature
        self.ctx.func.signature = self.build_signature(chunk, arity);
        self.ctx.func.name = UserFuncName::user(0, 0); // or stable id

        let meta: ValueMeta;
        {
            // 2. build
            let builder =
                FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
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
            };
            meta = tx.translate(chunk);
        }

        //println!("{}", self.ctx.func.display());

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

        Ok((meta, func_id))
    }

    fn build_signature(&self, _chunk: &Chunk, arity: u8) -> Signature {
        let mut sig = self.module.make_signature();

        // Pointer type for the target ISA (x86_64 = I64, aarch64 = I64, …)
        let ptr_ty = self.module.isa().pointer_type();

        // 1.  &mut Interpreter  (implicit “vm state” param)
        sig.params.push(AbiParam::new(ptr_ty));

        // 2.  Positional script arguments
        for _ in 0..arity {
            sig.params.push(AbiParam::new(types::I64));  // or F64 if NaN-tagging
        }

        // 3.  Return value (one Value slot)
        sig.returns.push(AbiParam::new(types::I64));

        sig
    }

}
