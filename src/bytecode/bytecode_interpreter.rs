use super::bytecode::ValueMeta;
use super::frames::call_frame::CallFrame;
use super::StepResult;
use crate::bytecode::bytecode;
use crate::bytecode::bytecode::Lineno;
use crate::bytecode::functions::builtins;
use crate::bytecode::values::value;
use crate::bytecode::values::Binop;
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

pub type InterpreterResult<T> = std::result::Result<T, InterpreterError>;
pub type OpHandler = fn(&mut Interpreter) -> InterpreterResult<()>;

pub type OpFn = fn(&mut Interpreter, u32, Lineno) -> StepResult<(), InterpreterError>;

pub const OP_TABLE: &[OpFn] = &[
    |vm, operand, lineno| op_return(vm, operand, lineno),
    |vm, operand, lineno| op_constant(vm, operand, lineno),
    |vm, operand, lineno| op_null(vm, operand, lineno), //closure
    |vm, operand, lineno| op_null(vm, operand, lineno), 
    |vm, operand, lineno| op_null(vm, operand, lineno), // true
    |vm, operand, lineno| op_null(vm, operand, lineno), // false
    |vm, operand, lineno| op_null(vm, operand, lineno), // negate
    |vm, operand, lineno| op_add(vm, operand, lineno), 
    |vm, operand, lineno| op_add_string(vm, operand, lineno),
    |vm, operand, lineno| op_null(vm, operand, lineno), // subtract
    |vm, operand, lineno| op_null(vm, operand, lineno), // multiply
    |vm, operand, lineno| op_null(vm, operand, lineno), // divide
    |vm, operand, lineno| op_null(vm, operand, lineno), // pow
    |vm, operand, lineno| op_null(vm, operand, lineno), // modulus
    |vm, operand, lineno| op_null(vm, operand, lineno), // not
    |vm, operand, lineno| op_null(vm, operand, lineno), // equal
    |vm, operand, lineno| op_null(vm, operand, lineno), // greater
    |vm, operand, lineno| op_null(vm, operand, lineno), // less
    |vm, operand, lineno| op_print(vm, operand, lineno), 
    |vm, operand, lineno| op_pop(vm, operand, lineno),
    |vm, operand, lineno| op_define_global(vm, operand, lineno),
    |vm, operand, lineno| op_null(vm, operand, lineno), // define local
    |vm, operand, lineno| op_get_global(vm, operand, lineno),
    |vm, operand, lineno| op_set_global(vm, operand, lineno),
    |vm, operand, lineno| op_null(vm, operand, lineno), // get local
    |vm, operand, lineno| op_null(vm, operand, lineno), // set local
    |vm, operand, lineno| op_null(vm, operand, lineno), // get upval
    |vm, operand, lineno| op_null(vm, operand, lineno), // set upval
    |vm, operand, lineno| op_null(vm, operand, lineno), // jump if false
    |vm, operand, lineno| op_null(vm, operand, lineno), // jump
    |vm, operand, lineno| op_null(vm, operand, lineno), // loop
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
    |vm, operand, lineno| op_null(vm, operand, lineno), // build list
    |vm, operand, lineno| op_null(vm, operand, lineno), // subscr
    |vm, operand, lineno| op_null(vm, operand, lineno), // set item
    |vm, operand, lineno| op_null(vm, operand, lineno), // start include
];

/*
    Return = 0,
    Constant,
    Closure,
    Null,
    True,
    False,
    Negate,
    Add,
    AddString,
    Subtract,
    Multiply,
    Divide,
    Pow,
    Modulus,
    Not,
    Equal,
    Greater,
    Less,
    Print,
    Pop,
    DefineGlobal,
    DefineLocal,
    GetGlobal,
    SetGlobal,
    GetLocal,
    SetLocal,
    GetUpval,
    SetUpval,
    JumpIfFalse,
    Jump,
    Loop,
    Call,
    CreateInstance,
    CloseUpvalue,
    Class,
    DefineProperty,
    SetProperty,
    GetProperty,
    Method,
    Invoke,
    Inherit,
    GetSuper,
    SuperInvoke,
    BuildList,
    Subscr,
    SetItem,
    StartInclude,
*/

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
            .map(|frame| {
                let frame_name = &frame.closure.function.name;
                let order = frame.closure.function.chunk.code[frame.instruction_pointer].clone();
                let lineno = order.lineno;
                if frame_name.is_empty() {
                    format!("[line {}] in script", lineno.value)
                } else {
                    format!("[line {}] in {}()", lineno.value, frame_name)
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

        handler(self, op.operand, lineno)
    }
}

pub fn op_return(vm: &mut Interpreter, _: u32, _: Lineno) -> StepResult<(), InterpreterError> {
    let result = vm.pop_stack();
    let _ = vm.pop_stack_meta();

    if vm.frames.len() <= 1 {
        vm.frames.pop();
        return StepResult::OkReturn(());
    }

    // pop function also, so we plus 1 here.
    let num_to_pop = usize::from(vm.frame().closure.function.locals_size + 1);
    vm.frames.pop();

    vm.pop_stack_n_times(num_to_pop);
    vm.pop_stack_meta_n_times(num_to_pop);

    vm.stack.push(result);
    vm.stack_meta.push(ValueMeta {
        is_public: true,
        is_mutable: true,
    });
    StepResult::Ok(())
}

pub fn op_constant(vm: &mut Interpreter, operand: u32, _: Lineno) -> StepResult<(), InterpreterError> {
    let idx = operand as usize;
    let constant = vm.read_constant(idx);
    vm.stack.push(constant);
    let meta = vm.read_constant_meta(idx);
    vm.stack_meta.push(meta);
    StepResult::Ok(())
}

pub fn op_null(vm: &mut Interpreter, _: u32, _: Lineno) -> StepResult<(), InterpreterError> {
    vm.stack.push(value::Value::Null);
    vm.stack_meta.push(ValueMeta {
        is_public: true,
        is_mutable: true,
    });
    StepResult::Ok(())
}

pub fn op_print(vm: &mut Interpreter, _: u32, _: Lineno) -> StepResult<(), InterpreterError> {
    let to_print = vm.peek().clone();
    vm.print_val(&to_print);
    vm.pop_stack();
    vm.pop_stack_meta();
    StepResult::Ok(())
}

pub fn op_add_string(vm: &mut Interpreter, _: u32, lineno: Lineno) -> StepResult<(), InterpreterError> {

    let val1 = vm.peek_by(0).clone();
    let val2 = vm.peek_by(1).clone();

    match (&val1, &val2) {
        (value::Value::String(s1), value::Value::String(s2)) => {
            vm.pop_stack_and_stackmeta();
            vm.pop_stack_and_stackmeta();
            vm.stack
                .push(value::Value::String(vm.heap.manage_str(format!(
                    "{}{}",
                    vm.get_str(*s2),
                    vm.get_str(*s1),
                ))));
            vm.stack_meta.push(ValueMeta {
                is_public: true,
                is_mutable: true,
            });
        }
        (value::Value::Number(s1), value::Value::String(s2)) => {
            vm.pop_stack_and_stackmeta();
            vm.pop_stack_and_stackmeta();
            vm.stack
                .push(value::Value::String(vm.heap.manage_str(format!(
                    "{}{}",
                    vm.get_str(*s2),
                    s1.to_string()
                ))));
            vm.stack_meta.push(ValueMeta {
                is_public: true,
                is_mutable: true,
            });
        }
        (value::Value::String(s1), value::Value::Number(s2)) => {
            vm.pop_stack_and_stackmeta();
            vm.pop_stack_and_stackmeta();
            vm.stack
                .push(value::Value::String(vm.heap.manage_str(format!(
                    "{}{}",
                    s2.to_string(),
                    vm.get_str(*s1),
                ))));
            vm.stack_meta.push(ValueMeta {
                is_public: true,
                is_mutable: true,
            });
        }
        (value::Value::Number(s1), value::Value::Number(s2)) => {
            vm.pop_stack_and_stackmeta();
            vm.pop_stack_and_stackmeta();
            vm.stack
                .push(value::Value::String(vm.heap.manage_str(format!(
                    "{}{}",
                    s2.to_string(),
                    s1.to_string()
                ))));
            vm.stack_meta.push(ValueMeta {
                is_public: true,
                is_mutable: true,
            });
        }
        _ => {
            return StepResult::Err(InterpreterError::Runtime(format!(
            "invalid operands of type {} and {} in string concatination expression: \
                        both operands must be string (line={})",
            value::type_of(&val1),
            value::type_of(&val2),
            lineno.value
        )))
        }
    }

    StepResult::Ok(())
}

pub fn op_add(vm: &mut Interpreter, _: u32, lineno: Lineno) -> StepResult<(), InterpreterError> {
    let val1 = vm.peek_by(0).clone();
    let val2 = vm.peek_by(1).clone();

    match (&val1, &val2) {
        (value::Value::Number(_), value::Value::Number(_)) => {
            return match vm.numeric_binop(Binop::Add, lineno) {
                Ok(()) => StepResult::Ok(()),
                Err(e) => StepResult::Err(e),
            };
        }
        (value::Value::String(_), value::Value::Number(_)) => {
            return match vm.numeric_binop(Binop::Add, lineno) {
                Ok(()) => StepResult::Ok(()),
                Err(e) => StepResult::Err(e),
            };
        }
        (value::Value::String(_), value::Value::String(_)) => {
            return match vm.numeric_binop(Binop::Add, lineno) {
                Ok(()) => StepResult::Ok(()),
                Err(e) => StepResult::Err(e),
            };
        }
        (value::Value::List(id1), value::Value::List(id2)) => {
            vm.pop_stack_and_stackmeta();
            vm.pop_stack_and_stackmeta();

            let mut res = vm.get_list_elements(*id2).clone();
            res.extend(vm.get_list_elements(*id1).clone());
            vm.stack
                .push(value::Value::List(vm.heap.manage_list(res)));
            vm.stack_meta.push(ValueMeta {
                is_public: true,
                is_mutable: true,
            });
        }
        _ => {
            return StepResult::Err(InterpreterError::Runtime(format!(
                "invalid operands of type {} and {} in add expression: \
                        both operands must be number or list (line={})",
                value::type_of(&val1),
                value::type_of(&val2),
                lineno.value
            )))
        }
    }
    StepResult::Ok(())
}

pub fn op_pop(vm: &mut Interpreter, _: u32, _: Lineno) -> StepResult<(), InterpreterError> {
    vm.pop_stack();
    vm.pop_stack_meta();
    StepResult::Ok(())
}

pub fn op_define_global(
    vm: &mut Interpreter,
    operand: u32,
    _: Lineno,
) -> StepResult<(), InterpreterError> {
    let (is_mutable, idx) = bytecode::unpack_one_flag(operand);
    if let value::Value::String(name_id) = vm.read_constant(idx) {
        vm.set_constant_meta(
            idx,
            ValueMeta {
                is_public: false,
                is_mutable,
            },
        );
        let val = vm.pop_stack();
        let _ = vm.pop_stack_meta();
        vm.globals.insert(vm.get_str(name_id).clone(), val);
    } else {
        panic!(
            "expected string when defining global, found {}",
            value::type_of(&vm.read_constant(idx))
        );
    }
    StepResult::Ok(())
}

pub fn op_get_global(vm: &mut Interpreter, operand: u32, lineno: Lineno) -> StepResult<(), InterpreterError> {
    let idx = operand as usize;
    if let value::Value::String(name_id) = vm.read_constant(idx) {
        let meta = vm.read_constant_meta(idx);
        match vm.globals.get(vm.get_str(name_id)) {
            Some(val) => {
                vm.stack.push(val.clone());
                vm.stack_meta.push(meta);
            }
            None => {
                return StepResult::Err(InterpreterError::Runtime(format!(
                    "Undefined variable '{}' at line {}.",
                    vm.get_str(name_id),
                    lineno.value
                )));
            }
        }
    } else {
        panic!(
            "expected string when defining global, found {}",
            value::type_of(&vm.read_constant(idx))
        );
    }

    StepResult::Ok(())
}

pub fn op_set_global(
    vm: &mut Interpreter,
    operand: u32,
    lineno: Lineno
) -> StepResult<(), InterpreterError> {
    let idx = operand as usize;
    if let value::Value::String(name_id) = vm.read_constant(idx) {
        let meta = vm.read_constant_meta(idx);
        let name_str = vm.get_str(name_id).clone();
        let val = vm.peek().clone();
        let stored_val = vm.globals.entry(name_str.clone());
        if let std::collections::hash_map::Entry::Occupied(mut e) = stored_val {
            if !meta.is_mutable {
                return StepResult::Err(InterpreterError::Runtime(format!(
                    "This variable {} is immutable but you tried to insert a value at line {}.",
                    name_str, lineno.value
                )));
            } else {
                e.insert(val);
            }
        } else {
            return StepResult::Err(InterpreterError::Runtime(format!(
                "Use of undefined variable {} in setitem expression at line {}.",
                name_str, lineno.value
            )));
        }
    } else {
        panic!(
            "expected string when setting global, found {}",
            value::type_of(&vm.read_constant(idx))
        );
    }
    StepResult::Ok(())
}
