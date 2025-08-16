use super::bytecode::ValueMeta;
use super::frames::call_frame::CallFrame;
use super::StepResult;
use crate::bytecode::bytecode;
use crate::bytecode::eval::*;
use crate::bytecode::eval::calculation::*;
use crate::bytecode::eval::logical::*;
use crate::bytecode::eval::frame::*;
use crate::bytecode::eval::function::*;
use crate::bytecode::functions::builtins;
use crate::bytecode::jit::jit::JIT;
use crate::bytecode::jit::jitcache::JitCache;
use crate::bytecode::values::value;
use crate::bytecode::values::value::Type;
use crate::gc::gc;

use std::collections::HashMap;
use std::fmt;
use std::fmt::Write;
use std::num::NonZeroUsize;

pub struct Interpreter {
    pub frames: Vec<CallFrame>,
    pub stack: Vec<value::Value>,
    pub stack_meta: Vec<ValueMeta>,
    pub output: Vec<String>,
    pub globals: HashMap<String, value::Value>,
    pub heap: gc::Heap,
    pub gray_stack: Vec<gc::HeapId>,
    pub jit: JIT,
    pub compiled: JitCache,  // 生成済みネイティブポインタ
}

impl Default for Interpreter {
    fn default() -> Interpreter {
        let mut res = Interpreter {
            frames: Default::default(),
            stack: Default::default(),
            stack_meta: Default::default(),
            output: Default::default(),
            globals: Default::default(),
            heap: Default::default(),
            gray_stack: Default::default(),
            jit: JIT::default(),
            compiled: Default::default(),
        };
        res.stack.reserve(256);
        res.frames.reserve(64);

        res.globals.insert(
            String::from("clock"),
            value::Value::NativeFunction(value::NativeFunction {
                arity: 0,
                name: String::from("clock"),
                func: builtins::clock,
            }),
        );
        res.globals.insert(
            String::from("exp"),
            value::Value::NativeFunction(value::NativeFunction {
                arity: 1,
                name: String::from("exp"),
                func: builtins::exp,
            }),
        );
        res.globals.insert(
            String::from("sqrt"),
            value::Value::NativeFunction(value::NativeFunction {
                arity: 1,
                name: String::from("sqrt"),
                func: builtins::sqrt,
            }),
        );
        res.globals.insert(
            String::from("int_pow"),
            value::Value::NativeFunction(value::NativeFunction {
                arity: 2,
                name: String::from("int_pow"),
                func: builtins::int_pow,
            }),
        );
        res.globals.insert(
            String::from("len"),
            value::Value::NativeFunction(value::NativeFunction {
                arity: 1,
                name: String::from("len"),
                func: builtins::len,
            }),
        );
        res.globals.insert(
            String::from("forEach"),
            value::Value::NativeFunction(value::NativeFunction {
                arity: 2,
                name: String::from("forEach"),
                func: builtins::for_each,
            }),
        );
        res.globals.insert(
            String::from("map"),
            value::Value::NativeFunction(value::NativeFunction {
                arity: 2,
                name: String::from("map"),
                func: builtins::map,
            }),
        );

        res
    }
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum InterpreterError {
    Runtime(String),
}

impl fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            InterpreterError::Runtime(err) => write!(f, "marie runtime error: {}", err),
        }
    }
}

impl std::error::Error for InterpreterError {}

pub type OpFn = fn(&mut Interpreter, usize, usize) -> StepResult<(), InterpreterError>;

pub const OP_TABLE: &[OpFn] = &[
    |vm, operand, lineno| op_return(vm, operand, lineno), // 0
    |vm, operand, lineno| op_constant(vm, operand, lineno), // 1
    |vm, operand, lineno| op_closure(vm, operand, lineno), //closure 2
    |vm, operand, lineno| op_null(vm, operand, lineno), // null 3
    |vm, operand, lineno| op_true(vm, operand, lineno), // 4
    |vm, operand, lineno| op_false(vm, operand, lineno), // 5
    |vm, operand, lineno| op_negate(vm, operand, lineno), // 6
    |vm, operand, lineno| op_add(vm, operand, lineno),  // 7
    |vm, operand, lineno| op_add_string(vm, operand, lineno), // 8
    |vm, operand, lineno| op_subtract(vm, operand, lineno), // subtract 9
    |vm, operand, lineno| op_multiply(vm, operand, lineno), // multiply 10
    |vm, operand, lineno| op_divide(vm, operand, lineno), // divide 11
    |vm, operand, lineno| op_power(vm, operand, lineno), // pow 12
    |vm, operand, lineno| op_modulus(vm, operand, lineno), // modulus 13
    |vm, operand, lineno| op_not(vm, operand, lineno), // not 14
    |vm, operand, lineno| op_equal(vm, operand, lineno), // equal 15
    |vm, operand, lineno| op_greater(vm, operand, lineno), // greater 16
    |vm, operand, lineno| op_less(vm, operand, lineno), // less 17
    |vm, operand, lineno| op_print(vm, operand, lineno),  // 18
    |vm, operand, lineno| op_pop(vm, operand, lineno), // 19
    |vm, operand, lineno| op_define_global(vm, operand, lineno), // 20
    |vm, operand, lineno| op_define_local(vm, operand, lineno), // define local 21
    |vm, operand, lineno| op_get_global(vm, operand, lineno), // 22
    |vm, operand, lineno| op_set_global(vm, operand, lineno), // 23
    |vm, operand, lineno| op_get_local(vm, operand, lineno), // get local 24
    |vm, operand, lineno| op_set_local(vm, operand, lineno), // set local 25
    |vm, operand, lineno| op_jump_if_false(vm, operand, lineno), // jump if false 26
    |vm, operand, lineno| op_jump(vm, operand, lineno), // jump 27
    |vm, operand, lineno| op_loop(vm, operand, lineno), // loop 28
    |vm, operand, lineno| op_call(vm, operand, lineno), // call 29
    |vm, operand, lineno| op_null(vm, operand, lineno), // create instance 30
    |vm, operand, lineno| op_null(vm, operand, lineno), // class 31
    |vm, operand, lineno| op_null(vm, operand, lineno), // define property 32
    |vm, operand, lineno| op_null(vm, operand, lineno), // set property 33
    |vm, operand, lineno| op_null(vm, operand, lineno), // get property 34
    |vm, operand, lineno| op_null(vm, operand, lineno), // method 35
    |vm, operand, lineno| op_null(vm, operand, lineno), // invoke 36
    |vm, operand, lineno| op_null(vm, operand, lineno), // inherit 37
    |vm, operand, lineno| op_null(vm, operand, lineno), // get super 38
    |vm, operand, lineno| op_null(vm, operand, lineno), // super invoke 39
    |vm, operand, lineno| op_build_list(vm, operand, lineno), // build list 40
    |vm, operand, lineno| op_list_subscript(vm, operand, lineno), // subscr 41
    |vm, operand, lineno| op_null(vm, operand, lineno), // set item 42
    |vm, operand, lineno| op_start_include(vm, operand, lineno), // start include 43
    |vm, operand, lineno| op_set_local(vm, operand, lineno), // define argument local 44
    |vm, operand, lineno| op_end_of_scope(vm, operand, lineno), // end of scope 45
    |vm, operand, lineno| op_do_nothing(vm, operand, lineno), // begin if 46
    |vm, operand, lineno| op_do_nothing(vm, operand, lineno), // end if 47
    |vm, operand, lineno| op_do_nothing(vm, operand, lineno), // prepare else if 48
    |vm, operand, lineno| op_do_nothing(vm, operand, lineno), // begin else if 49
    |vm, operand, lineno| op_do_nothing(vm, operand, lineno), // end else if 50
    |vm, operand, lineno| op_do_nothing(vm, operand, lineno), // begin else 51
    |vm, operand, lineno| op_do_nothing(vm, operand, lineno), // end all if 52
    |vm, operand, lineno| op_pop(vm, operand, lineno), // jit ignored pop 53
    |vm, operand, lineno| op_return(vm, operand, lineno), // inside if return 54
    |vm, operand, lineno| op_do_nothing(vm, operand, lineno),// NullInitialize 55
    |vm, operand, lineno| op_greater_equal(vm, operand, lineno), // greaterOrEqual 56
    |vm, operand, lineno| op_less_equal(vm, operand, lineno), // greaterOrLess 57
];

impl Interpreter {
    pub fn prepare_interpret(&mut self, func: bytecode::Function) {
        self.stack
            .push(value::Value::Function(self.heap.manage_closure(
                value::Closure {
                    function: func.clone(),
                },
            )));
        self.stack_meta.push(ValueMeta {
            is_public: true,
            is_mutable: true,
            value_type: Type::Function,
        });
        self.frames.push(CallFrame {
            closure: value::Closure {
                function: func,
            },
            instruction_pointer: 0,
            slots_offset: 1,
            invoked_method_id: None,
            is_include_file: false,
            is_function: false,
        });
    }

    pub fn interpret(&mut self, func: bytecode::Function) -> Result<(), InterpreterError> {
        self.prepare_interpret(func);
        self.run()
    }
    
    fn digits(n: usize) -> usize {
        NonZeroUsize::new(n).unwrap().ilog10() as usize + 1
    }

    pub fn format_backtrace(&self) -> String {
        // ── 1st pass ─────────────────────────────────────────────
        let mut len = "Backtrace (most recent call is at top):\n\n".len();
        for f in self.frames.iter() {
            let code = &f.closure.function.chunk.code;
            let ip   = f.instruction_pointer.min(code.len().saturating_sub(1));
            let line = code[ip].lineno.value as usize;

            len += "[line ".len() + Self::digits(line) + "] in ".len();
            if f.closure.function.name.is_empty() {
                len += "script".len();
            } else {
                len += f.closure.function.name.len();
                if f.is_function { len += "()".len(); }
            }
            len += 1; // 改行
        }

        // ── 2nd pass ─────────────────────────────────────────────
        let mut out = String::with_capacity(len);
        out.push_str("Backtrace (most recent call is at top):\n\n");

        for frame in self.frames.iter().rev() {
            let code = &frame.closure.function.chunk.code;
            let ip   = frame.instruction_pointer.min(code.len().saturating_sub(1));
            let line = code[ip].lineno.value;

            if frame.closure.function.name.is_empty() {
                // [line N] in script
                let _ = write!(out, "[line {}] in script\n", line);
            } else if frame.is_function {
                let _ = write!(
                    out,
                    "[line {}] in {}()\n",
                    line,
                    frame.closure.function.name
                );
            } else {
                let _ = write!(
                    out,
                    "[line {}] in {}\n",
                    line,
                    frame.closure.function.name
                );
            }
        }
        out
    }

    fn run(&mut self) -> Result<(), InterpreterError> {
        loop {
            match self.step() {
                StepResult::Ok(_) => {}
                StepResult::OkReturn(_) => {
                    return Ok(());
                }
                StepResult::Err(err) => {
                    return Err(err);
                }
            }
        }
    }

    pub fn format_val(&self, val: &value::Value) -> String {
        match val {
            value::Value::Number(num) => num.to_string(),
            value::Value::Bool(b) => b.to_string(),
            value::Value::String(str_handle) => self.get_str(*str_handle).clone(),
            value::Value::Function(closure_handle) => {
                format!("<fn '{}'>", self.get_closure(*closure_handle).function.name)
            }
            value::Value::Class(class_handle) => {
                format!("<class '{}'>", self.get_class(*class_handle).name)
            }
            value::Value::Instance(instance_handle) => {
                let instance = self.get_instance(*instance_handle);
                let class_name = &self.get_class(instance.class_id).name;
                format!("<{} instance>", class_name)
            }
            value::Value::NativeFunction(func) => format!("<native fn {}>", func.name),
            value::Value::BoundMethod(bound_method_id) => {
                let bound_method = self.get_bound_method(*bound_method_id);
                let instance = self.get_instance(bound_method.instance_id);
                let class_name = &self.get_class(instance.class_id).name;
                format!("<bound method of {} instance>", class_name)
            }
            value::Value::Null => "null".to_string(),
            value::Value::List(list_id) => {
                let elements = self.get_list_elements(*list_id);
                format!(
                    "[{}]",
                    elements
                        .iter()
                        .map(|element| self.format_val(&element))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
        }
    }

    // This is_done function is for debugger
    pub fn is_done(&self) -> bool {
        self.frames.is_empty()
            || self.frame().instruction_pointer >= self.frame().closure.function.chunk.code.len()
    }

    pub fn step(&mut self) -> StepResult<(), InterpreterError> {
        let op = self.next_op_and_advance();
        if self.heap.should_collect() {
            self.collect_garbage();
        }
        let lineno = op.lineno;

        let op_idx = op.opcode as usize;

        let handler = OP_TABLE[op_idx];

        handler(self, op.operand, lineno.value)
    }
}
