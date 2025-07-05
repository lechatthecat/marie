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
use crate::bytecode::values::value;
use crate::gc::gc;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

pub struct Interpreter {
    pub frames: Vec<CallFrame>,
    pub stack: Vec<value::Value>,
    pub stack_meta: Vec<ValueMeta>,
    pub output: Vec<String>,
    pub globals: HashMap<String, value::Value>,
    pub upvalues: Vec<Rc<RefCell<value::Upvalue>>>,
    pub heap: gc::Heap,
    pub gray_stack: Vec<gc::HeapId>,
}

impl Default for Interpreter {
    fn default() -> Interpreter {
        let mut res = Interpreter {
            frames: Default::default(),
            stack: Default::default(),
            stack_meta: Default::default(),
            output: Default::default(),
            globals: Default::default(),
            upvalues: Default::default(),
            heap: Default::default(),
            gray_stack: Default::default(),
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

pub type OpFn = fn(&mut Interpreter, u32, u32) -> StepResult<(), InterpreterError>;

pub const OP_TABLE: &[OpFn] = &[
    |vm, operand, lineno| op_return(vm, operand, lineno),
    |vm, operand, lineno| op_constant(vm, operand, lineno),
    |vm, operand, lineno| op_null(vm, operand, lineno), //closure
    |vm, operand, lineno| op_null(vm, operand, lineno),
    |vm, operand, lineno| op_true(vm, operand, lineno), 
    |vm, operand, lineno| op_false(vm, operand, lineno),
    |vm, operand, lineno| op_negate(vm, operand, lineno),
    |vm, operand, lineno| op_add(vm, operand, lineno), 
    |vm, operand, lineno| op_add_string(vm, operand, lineno),
    |vm, operand, lineno| op_subtract(vm, operand, lineno), // subtract
    |vm, operand, lineno| op_multiply(vm, operand, lineno), // multiply
    |vm, operand, lineno| op_divide(vm, operand, lineno), // divide
    |vm, operand, lineno| op_power(vm, operand, lineno), // pow
    |vm, operand, lineno| op_modulus(vm, operand, lineno), // modulus
    |vm, operand, lineno| op_not(vm, operand, lineno), // not
    |vm, operand, lineno| op_equal(vm, operand, lineno), // equal
    |vm, operand, lineno| op_greater(vm, operand, lineno), // greater
    |vm, operand, lineno| op_less(vm, operand, lineno), // less
    |vm, operand, lineno| op_print(vm, operand, lineno), 
    |vm, operand, lineno| op_pop(vm, operand, lineno),
    |vm, operand, lineno| op_define_global(vm, operand, lineno),
    |vm, operand, lineno| op_define_local(vm, operand, lineno), // define local
    |vm, operand, lineno| op_get_global(vm, operand, lineno),
    |vm, operand, lineno| op_set_global(vm, operand, lineno),
    |vm, operand, lineno| op_get_local(vm, operand, lineno), // get local
    |vm, operand, lineno| op_set_local(vm, operand, lineno), // set local
    |vm, operand, lineno| op_null(vm, operand, lineno), // get upval
    |vm, operand, lineno| op_null(vm, operand, lineno), // set upval
    |vm, operand, lineno| op_jump_if_false(vm, operand, lineno), // jump if false
    |vm, operand, lineno| op_jump(vm, operand, lineno), // jump
    |vm, operand, lineno| op_loop(vm, operand, lineno), // loop
    |vm, operand, lineno| op_null(vm, operand, lineno), // call
    |vm, operand, lineno| op_null(vm, operand, lineno), // create instance
    |vm, operand, lineno| op_null(vm, operand, lineno), // closeupvalue
    |vm, operand, lineno| op_null(vm, operand, lineno), // class 
    |vm, operand, lineno| op_null(vm, operand, lineno), // define property
    |vm, operand, lineno| op_null(vm, operand, lineno), // set property
    |vm, operand, lineno| op_null(vm, operand, lineno), // get property
    |vm, operand, lineno| op_null(vm, operand, lineno), // method
    |vm, operand, lineno| op_null(vm, operand, lineno), // invoke
    |vm, operand, lineno| op_null(vm, operand, lineno), // inherit
    |vm, operand, lineno| op_null(vm, operand, lineno), // get super
    |vm, operand, lineno| op_null(vm, operand, lineno), // super invoke
    |vm, operand, lineno| op_build_list(vm, operand, lineno), // build list
    |vm, operand, lineno| op_list_subscript(vm, operand, lineno), // subscr
    |vm, operand, lineno| op_null(vm, operand, lineno), // set item
    |vm, operand, lineno| op_start_include(vm, operand, lineno), // start include
];

impl Interpreter {
    pub fn prepare_interpret(&mut self, func: bytecode::Function) {
        self.stack
            .push(value::Value::Function(self.heap.manage_closure(
                value::Closure {
                    function: func.clone(),
                    upvalues: Vec::new(),
                },
            )));
        self.stack_meta.push(ValueMeta {
            is_public: true,
            is_mutable: true,
        });
        self.frames.push(CallFrame {
            closure: value::Closure {
                function: func,
                upvalues: Vec::new(),
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

    pub fn format_backtrace(&self) -> String {
        let lines: Vec<_> = self
            .frames
            .iter()
            .rev()
            .map(|frame| {
                let frame_name = &frame.closure.function.name;
                let order = if frame.instruction_pointer <= frame.closure.function.chunk.code.len()-1 {
                    //println!("{:?}", frame.closure.function.chunk.code);
                    &frame.closure.function.chunk.code[frame.instruction_pointer]
                } else {
                    frame.closure.function.chunk.code.last().unwrap()
                };

                if frame_name.is_empty() {
                    format!("[line {}] in script", order.lineno.value)
                } else if !frame_name.is_empty() && frame.is_function {
                    format!("[line {}] in {}()", order.lineno.value, frame_name)
                } else {
                    format!("[line {}] in {}", order.lineno.value, frame_name)
                }
            })
            .collect();
        format!("Backtrace (most recent call last):\n\n{}", lines.join("\n"))
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

    pub fn format_upval(&self, val: &value::Upvalue) -> String {
        match val {
            value::Upvalue::Open(idx) => format!("Open({})", idx),
            value::Upvalue::Closed(val) => format!("Closed({})", self.format_val(val)),
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

        handler(self, op.operand, lineno.value as u32)
    }
}
