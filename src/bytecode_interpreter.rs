use cranelift::codegen;
use cranelift::prelude::AbiParam;
use cranelift::prelude::Block;
use cranelift::prelude::FunctionBuilder;
use cranelift::prelude::FunctionBuilderContext;
use cranelift::prelude::InstBuilder;
use cranelift::prelude::Value;
use cranelift::prelude::types;
use cranelift_jit::JITModule;
use cranelift_module::DataContext;
use cranelift_module::Module;

use crate::builtins;
use crate::bytecode;
use crate::bytecode::Lineno;
use crate::gc;
use crate::jit;
use crate::step::call_func_pointer::CallFuncPointer;
use crate::step::step::StepFunction;
use crate::value;
use crate::value::{MarieValue, PropertyKey, TraitPropertyFind};

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::mem;
use std::rc::Rc;

pub fn disassemble_code(chunk: &bytecode::Chunk) -> Vec<String> {
    let mut lines: Vec<String> = Vec::new();

    for (idx, (op, lineno)) in chunk.code.iter().enumerate() {
        let formatted_op = match op {
            bytecode::Op::Return => "OP_RETURN".to_string(),
            bytecode::Op::Constant(const_idx) => format!(
                "OP_CONSTANT {} (idx={})",
                chunk.constants[*const_idx], *const_idx
            ),
            bytecode::Op::Nil => "OP_NIL".to_string(),
            bytecode::Op::True => "OP_TRUE".to_string(),
            bytecode::Op::False => "OP_FALSE".to_string(),
            bytecode::Op::Negate => "OP_NEGATE".to_string(),
            bytecode::Op::Add => "OP_ADD".to_string(),
            bytecode::Op::AddString => "OP_ADDSTRING".to_string(),
            bytecode::Op::Subtract => "OP_SUBTRACT".to_string(),
            bytecode::Op::Multiply => "OP_MULTIPLY".to_string(),
            bytecode::Op::Divide => "OP_DIVIDE".to_string(),
            bytecode::Op::Not => "OP_NOT".to_string(),
            bytecode::Op::Equal => "OP_NOT".to_string(),
            bytecode::Op::Greater => "OP_GREATER".to_string(),
            bytecode::Op::Less => "OP_LESS".to_string(),
            bytecode::Op::Print => "OP_PRINT".to_string(),
            bytecode::Op::Pop => "OP_POP".to_string(),
            bytecode::Op::EndScope => "OP_END_SCOPE".to_string(),
            bytecode::Op::DefineLocal(is_mutable, global_idx) => format!(
                "OP_DEFINE_LOCAL {:?} (is_mutable: {}, idx={})",
                chunk.constants[*global_idx], is_mutable, *global_idx
            ),
            bytecode::Op::DefineParamLocal(is_mutable, parameter_type, global_idx) => format!(
                "OP_DEFINE_PARAM_LOCAL {:?} (is_mutable: {}, parameter_type={}, idx={})",
                chunk.constants[*global_idx], is_mutable, parameter_type, *global_idx
            ),
            bytecode::Op::DefineGlobal(is_mutable, global_idx) => format!(
                "OP_DEFINE_GLOBAL {:?} (is_mutable: {}, idx={})",
                chunk.constants[*global_idx], is_mutable, *global_idx
            ),
            bytecode::Op::GetGlobal(global_idx) => format!(
                "OP_GET_GLOBAL {:?} (idx={})",
                chunk.constants[*global_idx], *global_idx
            ),
            bytecode::Op::SetGlobal(global_idx) => format!(
                "OP_SET_GLOBAL {:?} (idx={})",
                chunk.constants[*global_idx], *global_idx
            ),
            bytecode::Op::GetLocal(idx) => format!("OP_GET_LOCAL idx={}", *idx),
            bytecode::Op::SetLocal(idx) => format!("OP_SET_LOCAL idx={}", *idx),
            bytecode::Op::GetUpval(idx) => format!("OP_GET_UPVAL idx={}", *idx),
            bytecode::Op::SetUpval(idx) => format!("OP_SET_UPVAL idx={}", *idx),
            bytecode::Op::JumpIfFalse(loc) => format!("OP_JUMP_IF_FALSE {}", *loc),
            bytecode::Op::Jump(offset) => format!("OP_JUMP {}", *offset),
            bytecode::Op::Loop(offset) => format!("OP_LOOP {}", *offset),
            bytecode::Op::Call(arg_count) => format!("OP_CALL {}", *arg_count),
            bytecode::Op::CreateInstance(arg_count) => format!("OP_CREATE_INSTANCE {}", *arg_count),
            bytecode::Op::Closure(is_public, idx, function_type, _) => format!("OP_CLOSURE IS_PUBLIC={}, CONTENT={}, FUNCTION_TYPE={}", is_public, chunk.constants[*idx], function_type),
            bytecode::Op::CloseUpvalue => "OP_CLOSE_UPVALUE".to_string(),
            bytecode::Op::Class(idx) => format!("OP_CLASS {}", idx),
            bytecode::Op::DefineProperty(is_mutable, is_public, idx) => format!("OP_DEFINE_PROPERTY is_mutable={}, is_public={}, {}", is_mutable, is_public, idx),
            bytecode::Op::SetProperty(idx) => format!("OP_SET_PROPERTY {}", idx),
            bytecode::Op::GetProperty(idx) => format!("OP_GET_PROPERTY {}", idx),
            bytecode::Op::Method(is_public, idx) => format!("OP_METHOD ID={}, IS_PUBLIC={}", idx, is_public),
            bytecode::Op::Invoke(method_name, arg_count) => {
                format!("OP_INVOKE {} nargs={}", method_name, arg_count)
            }
            bytecode::Op::Inherit => "OP_INHERIT".to_string(),
            bytecode::Op::GetSuper(idx) => format!("OP_GET_SUPER {}", idx),
            bytecode::Op::SuperInvoke(method_name, arg_count) => {
                format!("OP_SUPER_INOKE {} nargs={}", method_name, arg_count)
            }
            bytecode::Op::BuildList(size) => format!("OP_BUILD_LIST {}", size),
            bytecode::Op::Subscr => "OP_SUBSCR".to_string(),
            bytecode::Op::SetItem => "OP_SETITEM".to_string(),
            bytecode::Op::StartUse(idx, locals_size, const_idx) 
                => format!("OP_STARTUSE {}, LOCALS_SIZE: {}, STRING: {}", chunk.constants[*idx], locals_size, chunk.constants[*const_idx]),
        };

        lines.push(format!(
            "{0: <04}   {1: <50} {2: <50}",
            idx,
            formatted_op,
            format!("line {}", lineno.value)
        ));
    }
    lines
}

pub fn disassemble_chunk(chunk: &bytecode::Chunk, name: &str) -> String {
    let mut lines: Vec<String> = Vec::new();

    if !name.is_empty() {
        lines.push(format!("============ {} ============", name));
    }

    lines.push("------------ constants -----------".to_string());
    for (idx, constant) in chunk.constants.iter().enumerate() {
        lines.push(format!("{:<4} {}", idx, constant));
    }

    lines.push("\n------------ code -----------------".to_string());

    for code_line in disassemble_code(chunk) {
        lines.push(code_line)
    }

    lines.join("\n")
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum Binop {
    Add,
    Sub,
    Mul,
    Div,
}

pub struct Interpreter {
    pub frames: Vec<CallFrame>,
    pub stack: Vec<value::MarieValue>,
    pub output: Vec<String>,
    pub globals: HashMap<String, value::MarieValue>,
    pub upvalues: Vec<Rc<RefCell<value::Upvalue>>>,
    pub heap: gc::Heap,
    pub gray_stack: Vec<gc::HeapId>,
    pub jit: jit::JIT,
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

#[derive(Default)]
pub struct CallFrame {
    pub closure: value::Closure,
    pub ip: usize,
    pub slots_offset: usize,
    pub invoked_method_id: Option<usize>,
    pub is_file: bool, 
}

impl CallFrame {
    fn next_op(&self) -> (bytecode::Op, bytecode::Lineno) {
        self.closure.function.chunk.code[self.ip].clone()
    }

    pub fn next_op_and_advance(&mut self) -> (bytecode::Op, bytecode::Lineno) {
        let res = self.next_op();
        self.ip += 1;
        res
    }

    pub fn read_constant(&self, idx: usize) -> bytecode::Constant {
        self.closure.function.chunk.constants[idx].clone()
    }

    fn _is_constants_empty(&self) -> bool {
        self.closure.function.chunk.constants.is_empty()
    }
}

impl Interpreter {
    pub fn default() -> Interpreter {
        let mut interp = Interpreter {
            frames: Default::default(),
            stack: Default::default(),
            output: Default::default(),
            globals: Default::default(),
            upvalues: Default::default(),
            heap: Default::default(),
            gray_stack: Default::default(),
            jit: jit::JIT::default(),
        };
        interp.stack.reserve(256);
        interp.frames.reserve(64);

        interp.globals.insert(
            String::from("dis"),
            MarieValue {
                is_public: false,
                is_mutable: false,
                val: value::Value::NativeFunction(value::NativeFunction {
                    arity: 1,
                    name: String::from("dis"),
                    func: builtins::dis_builtin,
                }),
                jit_value: None,
                jit_type: None
            }
        );
        interp.globals.insert(
            String::from("clock"),
            MarieValue {
                is_public: false,
                is_mutable: false,
                val: value::Value::NativeFunction(value::NativeFunction {
                    arity: 0,
                    name: String::from("clock"),
                    func: builtins::clock,
                }),
                jit_value: None,
                jit_type: None
            }
        );
        interp.globals.insert(
            String::from("exp"),
            MarieValue {
                is_public: false,
                is_mutable: false,
                val: value::Value::NativeFunction(value::NativeFunction {
                    arity: 1,
                    name: String::from("exp"),
                    func: builtins::exp,
                }),
                jit_value: None,
                jit_type: None
            }
        );
        interp.globals.insert(
            String::from("sqrt"),
            MarieValue {
                is_public: false,
                is_mutable: false,
                val: value::Value::NativeFunction(value::NativeFunction {
                    arity: 1,
                    name: String::from("sqrt"),
                    func: builtins::sqrt,
                }),
                jit_value: None,
                jit_type: None
            }
        );
        interp.globals.insert(
            String::from("len"),
            MarieValue {
                is_public: false,
                is_mutable: false,
                val: value::Value::NativeFunction(value::NativeFunction {
                    arity: 1,
                    name: String::from("len"),
                    func: builtins::len,
                }),
                jit_value: None,
                jit_type: None
            }
        );
        interp.globals.insert(
            String::from("forEach"),
            MarieValue {
                is_public: false,
                is_mutable: false,
                val: value::Value::NativeFunction(value::NativeFunction {
                    arity: 2,
                    name: String::from("forEach"),
                    func: builtins::for_each,
                }),
                jit_value: None,
                jit_type: None
            }
        );
        interp.globals.insert(
            String::from("map"),
            MarieValue {
                is_public: false,
                is_mutable: false,
                val: value::Value::NativeFunction(value::NativeFunction {
                    arity: 2,
                    name: String::from("map"),
                    func: builtins::map,
                }),
                jit_value: None,
                jit_type: None
            }
        );
        interp
    }
    pub fn prepare_interpret(&mut self, func: bytecode::Function) {
        self.stack
            .push(
                MarieValue {
                    is_public: false,
                    is_mutable: false,
                    val: value::Value::Function(self.heap.manage_closure(
                        value::Closure {
                            function: func.clone(),
                            upvalues: Vec::new(),
                        },
                    )),
                    jit_value: None,
                    jit_type: None
                }
            );
        self.frames.push(CallFrame {
            closure: value::Closure {
                function: func,
                upvalues: Vec::new(),
            },
            ip: 0,
            slots_offset: 1,
            invoked_method_id: None,
            is_file: true
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
                let (_, lineno) = frame.closure.function.chunk.code[frame.ip];
                if frame_name.is_empty() {
                    format!("[line {}] in script", lineno.value)
                } else if !frame_name.is_empty() && frame.is_file {
                    format!("[line {}] in {}", lineno.value, frame_name)
                } else {
                    format!("[line {}] in {}()", lineno.value, frame_name)
                }
            })
            .collect();
        format!("Backtrace (most recent call last):\n\n{}", lines.join("\n"))
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
            value::Value::Nil => "nil".to_string(),
            value::Value::List(list_id) => {
                let elements = self.get_list_elements(*list_id);
                format!(
                    "[{}]",
                    elements
                        .iter()
                        .map(|element| self.format_val(&element.val))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            },
        }
    }

    fn run(&mut self) -> Result<(), InterpreterError> {
        loop {
            if self.is_done() {
                return Ok(());
            }

            if let Err(err) = self.step() {
                return Err(err);
            }
        }
    }

    pub fn is_done(&self) -> bool {
        self.frames.is_empty() || self.frame().ip >= self.frame().closure.function.chunk.code.len()
    }

    pub fn setitem(
        &mut self,
        lhs: MarieValue,
        subscript: MarieValue,
        rhs: MarieValue,
        lineno: bytecode::Lineno,
    ) -> Result<(), InterpreterError> {
        if let value::Value::List(id) = lhs.val {
            if let value::Value::Number(index_float) = subscript.val {
                let elements = self.get_list_elements_mut(id);
                match Interpreter::subscript_to_inbound_index(elements.len(), index_float, lineno) {
                    Ok(index_int) => {
                        elements[index_int] = rhs;
                        Ok(())
                    }
                    Err(err) => Err(InterpreterError::Runtime(err)),
                }
            } else {
                Err(InterpreterError::Runtime(format!(
                    "Invalid subscript of type {:?} in subscript expression",
                    value::type_of(&lhs.val)
                )))
            }
        } else {
            Err(InterpreterError::Runtime(format!(
                "Invalid value of type {:?} in subscript expression",
                value::type_of(&subscript.val)
            )))
        }
    }

    pub fn subscript(
        &mut self,
        value: MarieValue,
        subscript: MarieValue,
        lineno: bytecode::Lineno,
    ) -> Result<MarieValue, InterpreterError> {
        if let value::Value::List(id) = value.val {
            if let value::Value::Number(index_float) = subscript.val {
                let elements = self.get_list_elements(id);
                match Interpreter::subscript_to_inbound_index(elements.len(), index_float, lineno) {
                    Ok(index_int) => Ok(elements[index_int].clone()),
                    Err(err) => Err(InterpreterError::Runtime(err)),
                }
            } else {
                Err(InterpreterError::Runtime(format!(
                    "Invalid subscript of type {:?} in subscript expression",
                    value::type_of(&value.val)
                )))
            }
        } else {
            Err(InterpreterError::Runtime(format!(
                "Invalid value of type {:?} in subscript expression",
                value::type_of(&value.val)
            )))
        }
    }

    pub fn subscript_to_inbound_index(
        list_len: usize,
        index_float: f64,
        lineno: bytecode::Lineno,
    ) -> Result<usize, String> {
        let index_int = index_float as i64;
        if 0 <= index_int && index_int < list_len as i64 {
            return Ok(index_int as usize);
        }
        if index_int < 0 && -index_int <= list_len as i64 {
            return Ok((list_len as i64 + index_int) as usize);
        }
        Err(format!(
            "List subscript index out of range at {}",
            lineno.value
        ))
    }

    pub fn invoke(&mut self, method_name: &str, arg_count: u8) -> Result<(), InterpreterError> {
        let receiver_id = match &self.peek_by(arg_count.into()).val {
            value::Value::Instance(id) => *id,
            _ => {
                return Err(InterpreterError::Runtime(
                    "Only instances have methods.".to_string(),
                ));
            }
        };

        let instance = self.get_instance(receiver_id);
        let instance_class_id = instance.class_id;
        let class = self.get_class(instance_class_id);
        let class_id = class
            .properties
            .find_methodid(method_name);

        let class_id = match class_id {
            Some((class_id, _method_id)) => class_id,
            None => instance_class_id
        };

        if let Some(field) = self
            .get_instance(receiver_id)
            .fields
            .get(&PropertyKey{ name: String::from(method_name), id: class_id })
            .cloned()
        {
            if field.is_public {
                return self.call_value(field, arg_count);
            } else if let Some(invoked_method_id) = self.frame().invoked_method_id {
                let class_id = instance.fields.find_classid(invoked_method_id);
                if let Some(class_id) = class_id {
                    if instance.class_id == class_id {
                        return self.call_value(field, arg_count);
                    }
                }
                return Err(InterpreterError::Runtime(format!(
                    "This attribute is private: {}",
                    method_name
                )))
            } else {
                return Err(InterpreterError::Runtime(format!(
                    "This attribute is private: {}",
                    method_name
                )))
            }
        }

        self.invoke_from_class(class_id, method_name, arg_count)
    }

    pub fn frame_mut(&mut self) -> &mut CallFrame {
        let frames_len = self.frames.len();
        &mut self.frames[frames_len - 1]
    }

    pub fn maybe_frame(&self) -> Option<&CallFrame> {
        self.frames.last()
    }

    pub fn frame(&self) -> &CallFrame {
        self.maybe_frame().unwrap()
    }

    pub fn invoke_from_class(
        &mut self,
        class_id: gc::HeapId,
        method_name: &str,
        arg_count: u8,
    ) -> Result<(), InterpreterError> {
        let class = self.get_class(class_id);
        let method_id = match class
            .properties
            .get(&PropertyKey { name: method_name.clone().to_string(), id: class_id })
        {
            Some(maybe_method_id) => {
                if let value::Value::Function (method_id) = maybe_method_id.val {
                    method_id
                } else {
                    return Err(InterpreterError::Runtime(format!(
                        "Undefined property {}.",
                        method_name
                    )))
                }
            },
            None => {
                return Err(InterpreterError::Runtime(format!(
                        "Undefined property {}.",
                        method_name
                    )))
            }
        };

        self.call_value(
            MarieValue{
                is_mutable:false,
                is_public: true,
                val: value::Value::Function(method_id),
                jit_value: None,
                jit_type: None
            },
            arg_count,
        )
    }

    pub fn close_upvalues(&mut self, index: usize) {
        let value = &self.stack[index];
        for upval in &self.upvalues {
            if upval.borrow().is_open_with_index(index) {
                upval.replace(value::Upvalue::Closed(value.clone().val));
            }
        }

        self.upvalues.retain(|u| u.borrow().is_open());
    }

    pub fn find_open_uval(&self, index: usize) -> Option<Rc<RefCell<value::Upvalue>>> {
        for upval in self.upvalues.iter().rev() {
            if upval.borrow().is_open_with_index(index) {
                return Some(upval.clone());
            }
        }

        None
    }

    pub fn call_value(
        &mut self,
        val_to_call: MarieValue,
        arg_count: u8,
    ) -> Result<(), InterpreterError> {
        match val_to_call.val {
            value::Value::Function(closure_handle) => {
                let closure = self.get_closure(closure_handle).clone();
                self.prepare_call(closure_handle, arg_count)?;
                let func = &closure.function;
                let fn_code = func.function_pointer.unwrap();
                let result: Result<i64, String> = unsafe { self.call_func_pointer(fn_code, arg_count) };
                // Users must specify argument value's type, 
                // so we are guessing only some runtime errors can be returned once the function is successfully defined
                // "some errors" contain, for example, when exception is thrown by external service 
                // or exception is thrown by user etc...
                match result {
                    Ok(result_val) => {
                        let val = if result_val > -1 {
                            value::Value::Number(f64::from_bits(result_val as u64))
                        } else {
                            value::Value::Nil
                        };
                        self.stack.push(MarieValue {
                            val,
                            is_mutable: true,
                            is_public: true, 
                            jit_value: None ,
                            jit_type: None
                        });
                        Ok(())
                    },
                    Err(err) => {
                        Err(InterpreterError::Runtime(err))
                    }
                }
            }
            value::Value::NativeFunction(native_func) => {
                self.call_native_func(native_func, arg_count)?;
                Ok(())
            }
            value::Value::BoundMethod(method_id) => {
                self.call_bound_method(method_id, arg_count)?;
                Ok(())
            }
            _ => Err(InterpreterError::Runtime(format!(
                "attempted to call non-callable value of type {:?}.",
                value::type_of(&val_to_call.val)
            ))),
        }
    }

    pub fn create_instance_val(
        &mut self,
        val_to_call: value::Value,
        arg_count: u8,
    ) -> Result<(), InterpreterError> {
        match val_to_call {
            value::Value::Class(class_id) => {
                let new_instance =
                    value::Value::Instance(self.heap.manage_instance(value::Instance {
                        class_id,
                        fields: HashMap::new(),
                    }));

                let arg_count_usize: usize = arg_count.into();
                let stack_len = self.stack.len();
                self.stack[stack_len - 1 - arg_count_usize] = MarieValue {
                    is_mutable: true,
                    is_public: true,
                    val: new_instance,
                    jit_value: None,
                    jit_type: None
                };

                {
                    let maybe_method_id = self
                        .get_class(class_id)
                        .properties
                        .get(&PropertyKey { name: "init".to_string(), id: class_id });

                    if let Some(method_id) = maybe_method_id {
                        if let value::Value::Function(method_id) = method_id.val {
                            return self.prepare_call(method_id, arg_count);
                        }
                    }
                }

                if arg_count > 0 {
                    return Err(InterpreterError::Runtime(format!(
                        "Call to class ctor expected 0 arguments, got {}.",
                        arg_count,
                    )));
                }

                self.create_instance(class_id);
                Ok(())
            }
            _ => Err(InterpreterError::Runtime(format!(
                "attempted to call non-callable value of type {:?}.",
                value::type_of(&val_to_call)
            ))),
        }
    }

    fn call_native_func(
        &mut self,
        native_func: value::NativeFunction,
        arg_count: u8,
    ) -> Result<(), InterpreterError> {
        if arg_count != native_func.arity {
            return Err(InterpreterError::Runtime(format!(
                "Native function {} expected {} arguments but found {}.",
                native_func.name, native_func.arity, arg_count
            )));
        }

        let mut args = Vec::new();
        for _ in 0..arg_count {
            args.push(self.pop_stack()) // pop args
        }
        args.reverse();
        let args = args;
        self.pop_stack(); // native function value

        let res = (native_func.func)(self, &args);

        match res {
            Ok(result) => {
                self.stack.push(result);
                Ok(())
            }
            Err(err) => Err(InterpreterError::Runtime(format!(
                "When calling {}: {}.",
                native_func.name, err
            ))),
        }
    }

    fn create_instance(&mut self, class_id: gc::HeapId) {
        self.pop_stack(); // class object
        let instance_id = self.heap.manage_instance(value::Instance {
            class_id,
            fields: HashMap::new(),
        });
        let properties = self.get_class(class_id).properties.clone();
        self.bindattr(instance_id, properties);
        self.stack.push(
            MarieValue {
                is_mutable: true,
                is_public: true,
                val: value::Value::Instance(instance_id),
                jit_value: None,
                jit_type: None
            }
        );
    }

    pub fn bindattr(
        &mut self,
        instance_id: usize,
        properties: HashMap<PropertyKey, MarieValue>,
    ) {
        let instance = self.heap.get_instance_mut(instance_id);
        instance.fields.extend(properties);
    }

    fn call_bound_method(
        &mut self,
        method_id: gc::HeapId,
        arg_count: u8,
    ) -> Result<(), InterpreterError> {
        let bound_method = self.get_bound_method(method_id).clone();
        let closure_id = bound_method.closure_id;
        let arg_count_usize: usize = arg_count.into();
        let stack_len = self.stack.len();
        self.stack[stack_len - arg_count_usize - 1] = MarieValue {
            is_mutable: true,
            is_public: true,
            val: value::Value::Instance(bound_method.instance_id),
            jit_value: None,
            jit_type: None
        };
        self.prepare_call(closure_id, arg_count)
    }

    /*
    Set up a few call frame so that on the next interpreter step we'll start executing code inside the function.
     */
    pub fn prepare_call(
        &mut self,
        closure_handle: gc::HeapId,
        arg_count: u8,
    ) -> Result<(), InterpreterError> {
        let closure = self.get_closure(closure_handle).clone();
        let func = &closure.function;
        if arg_count != func.arity {
            return Err(InterpreterError::Runtime(format!(
                "Expected {} arguments but found {}.",
                func.arity, arg_count
            )));
        }

        self.frames.push(CallFrame::default());
        let mut frame = self.frames.last_mut().unwrap();
        frame.closure = closure;
        frame.slots_offset = self.stack.len() - usize::from(arg_count);
        frame.invoked_method_id = Some(closure_handle);
        Ok(())
    }

    pub fn pop_stack_n_times(&mut self, num_to_pop: usize) {
        for _ in 0..num_to_pop {
            self.pop_stack();
        }
    }

    pub fn is_falsey(&self, val: &value::Value) -> bool {
        match val {
            value::Value::Nil => true,
            value::Value::Bool(b) => !*b,
            value::Value::Number(f) => *f == 0.0,
            value::Value::Function(_) => false,
            value::Value::NativeFunction(_) => false,
            value::Value::Class(_) => false,
            value::Value::Instance(_) => false,
            value::Value::BoundMethod(_) => false,
            value::Value::String(id) => self.get_str(*id).is_empty(),
            value::Value::List(id) => self.get_list_elements(*id).is_empty(),
        }
    }

    pub fn print_val(&mut self, val: &value::Value) {
        let output = self.format_val(val);
        println!("{}", output);
        self.output.push(output);
    }

    pub fn values_equal(&self, val1: &value::Value, val2: &value::Value) -> bool {
        match (val1, val2) {
            (value::Value::Number(n1), value::Value::Number(n2)) => (n1 - n2).abs() < f64::EPSILON,
            (value::Value::Bool(b1), value::Value::Bool(b2)) => b1 == b2,
            (value::Value::String(s1), value::Value::String(s2)) => {
                self.get_str(*s1) == self.get_str(*s2)
            }
            (value::Value::Nil, value::Value::Nil) => true,
            (_, _) => false,
        }
    }

    pub fn numeric_binop(
        &mut self,
        binop: Binop,
        lineno: bytecode::Lineno,
    ) -> Result<(), InterpreterError> {
        let val1 = self.peek_by(0).clone().val;
        let val2 = self.peek_by(1).clone().val;

        match (&val1, &val2) {
            (value::Value::Number(n1), value::Value::Number(n2)) => {
                self.pop_stack();
                self.pop_stack();
                self.stack
                    .push(
                        MarieValue {
                            is_mutable: true,
                            is_public: true,
                            val: value::Value::Number(Interpreter::apply_numeric_binop(
                                *n2, *n1, binop, // note the order!
                            )),
                            jit_value: None,
                            jit_type: None
                        }
                    );
                Ok(())
            }
            (value::Value::String(n1), value::Value::Number(n2)) => {
                self.pop_stack();
                self.pop_stack();
                let n1 = self.get_str(*n1).parse::<f64>();
                let num;
                match n1 {
                    Err(_) => {
                        return Err(InterpreterError::Runtime(format!(
                            "Expected numbers in {:?} expression. Found {:?} and {:?} (line={})",
                            binop,
                            value::type_of(&val1),
                            value::type_of(&val2),
                            lineno.value
                        )));
                    },
                    Ok(val) =>{
                        num = val;
                    }
                }
                self.stack
                    .push(
                        MarieValue {
                            is_mutable: true,
                            is_public: true,
                            val: value::Value::Number(Interpreter::apply_numeric_binop(
                                *n2, num, binop, // note the order!
                            )),
                            jit_value: None,
                            jit_type: None
                        }
                );
                Ok(())
            }
            (value::Value::Number(n1), value::Value::String(n2)) => {
                self.pop_stack();
                self.pop_stack();
                let n2 = self.get_str(*n2).parse::<f64>();
                let num;
                match n2 {
                    Err(_) => {
                        return Err(InterpreterError::Runtime(format!(
                            "Expected numbers in {:?} expression. Found {:?} and {:?} (line={})",
                            binop,
                            value::type_of(&val1),
                            value::type_of(&val2),
                            lineno.value
                        )));
                    },
                    Ok(val) =>{
                        num = val;
                    }
                }
                self.stack
                    .push(
                        MarieValue {
                            is_mutable: true,
                            is_public: true,
                            val: value::Value::Number(Interpreter::apply_numeric_binop(
                                *n1, num, binop, // note the order!
                            )),
                            jit_value: None,
                            jit_type: None
                        }
                    );
                Ok(())
            }
            (value::Value::String(n1), value::Value::String(n2)) => {
                self.pop_stack();
                self.pop_stack();
                let n1 = self.get_str(*n1).parse::<f64>();
                let num1;
                match n1 {
                    Err(_) => {
                        return Err(InterpreterError::Runtime(format!(
                            "Expected numbers in {:?} expression. Found {:?} and {:?} (line={})",
                            binop,
                            value::type_of(&val1),
                            value::type_of(&val2),
                            lineno.value
                        )));
                    },
                    Ok(val) =>{
                        num1 = val;
                    }
                }
                let n2 = self.get_str(*n2).parse::<f64>();
                let num2;
                match n2 {
                    Err(_) => {
                        return Err(InterpreterError::Runtime(format!(
                            "Expected numbers in {:?} expression. Found {:?} and {:?} (line={})",
                            binop,
                            value::type_of(&val1),
                            value::type_of(&val2),
                            lineno.value
                        )));
                    },
                    Ok(val) =>{
                        num2 = val;
                    }
                }
                self.stack
                    .push(
                        MarieValue {
                            is_mutable: true,
                            is_public: true,
                            val: value::Value::Number(Interpreter::apply_numeric_binop(
                                num1, num2, binop, // note the order!
                            )),
                            jit_value: None,
                            jit_type: None
                        }
                );
                Ok(())
            }
            _ => Err(InterpreterError::Runtime(format!(
                "Expected numbers in {:?} expression. Found {:?} and {:?} (line={})",
                binop,
                value::type_of(&val1),
                value::type_of(&val2),
                lineno.value
            ))),
        }
    }

    pub fn apply_numeric_binop(left: f64, right: f64, binop: Binop) -> f64 {
        match binop {
            Binop::Add => left + right,
            Binop::Sub => left - right,
            Binop::Mul => left * right,
            Binop::Div => left / right,
        }
    }

    pub fn setattr(
        &mut self,
        maybe_instance: MarieValue,
        val: MarieValue,
        attr_id: gc::HeapId,
    ) -> Result<(), InterpreterError> {
        let attr_name = self.get_str(attr_id).clone();
        match maybe_instance.val {
            value::Value::Instance(instance_id) => {
                let instance = self.heap.get_instance_mut(instance_id);
                let is_mutable = match instance.fields.get(&PropertyKey{ name: attr_name.clone(), id: instance_id }) {
                    Some(val) => {
                        val.is_mutable
                    },
                    None => true,
                };
                if !is_mutable {
                    return Err(InterpreterError::Runtime(format!(
                        "can't set a value to immutable attribute val = {:?}",
                        &attr_name
                    )));
                }
                instance.fields.insert(PropertyKey{ name: attr_name, id: instance_id }, val);
                Ok(())
            }
            _ => Err(InterpreterError::Runtime(format!(
                "can't set attribute on value of type {:?}. Need class instance. val = {:?}",
                value::type_of(&maybe_instance.val),
                self.format_val(&maybe_instance.val)
            ))),
        }
    }

    pub fn getattr(
        &self,
        maybe_instance: value::Value,
        attr_id: gc::HeapId,
        caller_class_id: gc::HeapId,
    ) -> Result<Option<value::Value>, InterpreterError> {
        // get the attribute name.
        let attr_name = self.get_str(attr_id).clone();
        // check where this attribute is.
        match maybe_instance {
            value::Value::Instance(instance_id) => {
                let instance = self.heap.get_instance(instance_id);
                match instance.fields.get(&PropertyKey{ name: attr_name.clone(), id: instance_id }) {
                    // if instance has this value with this instance_id, return it.
                    Some(val) => {
                        if val.is_public {
                            Ok(Some(val.clone().val))
                        } else {
                            Err(InterpreterError::Runtime(format!(
                                "This attribute is private: {}",
                                attr_name,
                            )))
                        }
                    },
                    // if instance doesn't, we will check with other id.
                    None => {
                        // check if the attribute is called from a method of class/instance.
                        if let Some(method_id) = self.frame().invoked_method_id {
                            // check if the class of the invoked method have the value.
                            let class = self.get_class(instance.class_id);
                            let maybe_class_id = class.properties.find_classid(method_id);
                            if let Some(class_id) = maybe_class_id {
                                match instance.fields.get(&PropertyKey{ name: attr_name.clone(), id: class_id }) { 
                                    Some(val) => {
                                        Ok(Some(val.clone().val))
                                    },
                                    // if the class or parent classes don't have the value,
                                    // the value doesn't exist.
                                    None => {
                                        Ok(None)
                                    }
                                }
                            } else {
                                // maybe this arrtibute belongs to the class which called the method?
                                match instance.fields.get(&PropertyKey{ name: attr_name.clone(), id: caller_class_id }) {
                                    Some(val) => {
                                        Ok(Some(val.clone().val))
                                    },
                                    None => {
                                        Ok(None)
                                    }
                                }
                            }
                        } else {
                            // if this attribute is not invoked from class/instance, maybe it is globally defined method?
                            match instance.fields.get(&PropertyKey{ name: attr_name.clone(), id: caller_class_id }) {
                                Some(val) => {
                                    if val.is_public {
                                        Ok(Some(val.clone().val))
                                    } else {
                                        Err(InterpreterError::Runtime(format!(
                                            "This attribute is private: {}",
                                            attr_name,
                                        )))
                                    }
                                },
                                // if it isn't, maybe it is a value, not method?
                                None => {
                                    match instance.fields.find_property(&attr_name.clone()) {
                                        Some(val) => {
                                            if val.is_public {
                                                Ok(Some(val.clone().val))
                                            } else {
                                                Err(InterpreterError::Runtime(format!(
                                                    "This attribute is private: {}",
                                                    attr_name,
                                                )))
                                            }
                                        },
                                        None => {
                                            Ok(None)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            _ => Err(InterpreterError::Runtime(format!(
                "can't get attribute {} on value of type {:?}. Need class instance.",
                attr_name,
                value::type_of(&maybe_instance)
            ))),
        }
    }

    pub fn bind_method(
        &mut self,
        instance_id: gc::HeapId,
        class: value::Class,
        attr_id: gc::HeapId,
    ) -> Result<bool, InterpreterError> {
        let attr_name = self.get_str(attr_id).clone();
        if let Some(closure_id) = class.properties.get(&PropertyKey{ name: attr_name, id: instance_id }) {
            if let value::Value::Function(closure_id) = closure_id.val {
                self.pop_stack();
                self.stack
                    .push(
                        MarieValue {
                            is_mutable: false,
                            is_public: true,
                            val: value::Value::BoundMethod(self.heap.manage_bound_method(
                                value::BoundMethod {
                                    instance_id,
                                    closure_id: closure_id,
                                },
                            )),
                            jit_value: None,
                            jit_type: None
                        }
                );
                Ok(true)
            } else {
                Ok(false) 
            }
        } else {
            Ok(false)
        }
    }

    pub fn pop_stack(&mut self) -> value::MarieValue {
        match self.stack.pop() {
            Some(val) => val,
            None => panic!("attempted to pop empty stack!"),
        }
    }

    pub fn peek(&self) -> &value::MarieValue {
        self.peek_by(0)
    }

    pub fn peek_by(&self, n: usize) -> &value::MarieValue {
        &self.stack[self.stack.len() - n - 1]
    }

    pub fn next_line(&self) -> usize {
        self.next_op().1.value
    }

    pub fn next_op(&self) -> (bytecode::Op, bytecode::Lineno) {
        self.frame().next_op()
    }

    pub fn next_op_and_advance(&mut self) -> (bytecode::Op, bytecode::Lineno) {
        self.frame_mut().next_op_and_advance()
    }

    pub fn read_constant(&mut self, idx: usize) -> value::Value {
        let constant = self.frame().read_constant(idx);
        match constant {
            bytecode::Constant::Number(num) => value::Value::Number(num),
            bytecode::Constant::String(s) => value::Value::String(self.heap.manage_str(s)),
            bytecode::Constant::Function(f) => {
                value::Value::Function(self.heap.manage_closure(value::Closure {
                    function: f.function,
                    upvalues: Vec::new(),
                }))
            }
        }
    }

    pub fn _is_constants_empty(&self) -> bool {
        self.frame()._is_constants_empty()
    }

    pub fn extract_number(val: &value::Value) -> Option<f64> {
        match val {
            value::Value::Number(f) => Some(*f),
            _ => None,
        }
    }

    pub fn extract_bool(val: &value::Value) -> Option<bool> {
        match val {
            value::Value::Bool(b) => Some(*b),
            _ => None,
        }
    }

    pub fn get_str(&self, str_handle: gc::HeapId) -> &String {
        self.heap.get_str(str_handle)
    }

    pub fn get_closure(&self, closure_handle: gc::HeapId) -> &value::Closure {
        self.heap.get_closure(closure_handle)
    }

    pub fn get_class(&self, class_handle: gc::HeapId) -> &value::Class {
        self.heap.get_class(class_handle)
    }

    pub fn get_class_mut(&mut self, class_handle: gc::HeapId) -> &mut value::Class {
        self.heap.get_class_mut(class_handle)
    }

    pub fn get_bound_method(&self, method_handle: gc::HeapId) -> &value::BoundMethod {
        self.heap.get_bound_method(method_handle)
    }

    pub fn get_list_elements(&self, list_handle: gc::HeapId) -> &Vec<MarieValue> {
        self.heap.get_list_elements(list_handle)
    }

    pub fn get_list_elements_mut(&mut self, list_handle: gc::HeapId) -> &mut Vec<MarieValue> {
        self.heap.get_list_elements_mut(list_handle)
    }

    pub fn get_instance(&self, instance_handle: gc::HeapId) -> &value::Instance {
        self.heap.get_instance(instance_handle)
    }

    pub fn collect_garbage(&mut self) {
        self.heap.unmark();
        self.mark_roots();
        self.trace_references();

        self.heap.sweep();
    }

    pub fn trace_references(&mut self) {
        loop {
            let maybe_val = self.gray_stack.pop();
            match maybe_val {
                Some(val) => self.blacken_object(val),
                None => break,
            }
        }
    }

    fn blacken_object(&mut self, val: gc::HeapId) {
        let children_to_walk = self.heap.children(val);
        for child_val in children_to_walk {
            if !self.heap.is_marked(child_val) {
                self.heap.mark(child_val);
                self.blacken_object(child_val);
            }
        }
    }

    fn mark_roots(&mut self) {
        let stack_vals_to_mark: Vec<gc::HeapId> =
            self.stack.iter().filter_map(gc::Heap::mutable_extract_id).collect();

        let frame_closure_children: Vec<gc::HeapId> = self
            .frames
            .iter()
            .flat_map(|frame| self.heap.closure_children(&frame.closure))
            .collect();

        let globals_to_mark: Vec<gc::HeapId> = self
            .globals
            .values()
            .flat_map(gc::Heap::mutable_extract_id)
            .collect();

        for val in stack_vals_to_mark
            .iter()
            .chain(frame_closure_children.iter())
            .chain(globals_to_mark.iter())
        {
            self.mark_value(*val);
        }
    }

    fn mark_value(&mut self, handle: gc::HeapId) {
        let is_marked = self.heap.is_marked(handle);
        if !is_marked {
            self.heap.mark(handle);
        }
        self.gray_stack.push(handle)
    }
}

#[cfg(test)]
mod tests {

    macro_rules! vec_of_strings {
    ($($x:expr),*) => (vec![$($x.to_string()),*]);
}

    use cranelift::codegen::ir;
    use cranelift_jit::JITBuilder;

    use crate::builtins;
    use crate::bytecode_interpreter::*;
    use crate::compiler::*;
    use crate::extensions;
    use crate::foreign::conversion::f64_to_bits;

    fn evaluate(code: &str, extensions: extensions::Extensions) -> Result<Vec<String>, String> {
        let func_or_err = Compiler::compile(
            String::from(code),
            extensions,
            None
        );

        match func_or_err {
            Ok(func) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(func);
                match res {
                    Ok(()) => Ok(interp.output),
                    Err(InterpreterError::Runtime(err)) => Err(err),
                }
            }
            Err(Error::Lexical(err)) => Err(err.what),
            Err(Error::Parse(err)) => Err(err.what),
            Err(Error::Semantic(err)) => Err(err.what),
            Err(Error::Internal(err)) => Err(err),
        }
    }

    fn check_output(code: &str, extensions: extensions::Extensions, expected_output: &[String]) {
        let res = evaluate(code, extensions);

        match res {
            Ok(output) => assert_eq!(output, expected_output),
            Err(err) => panic!("{}", err),
        }
    }

    fn check_output_default(code: &str, expected_output: &[String]) {
        check_output(code, extensions::Extensions::default(), expected_output);
    }

    fn check_output_lists(code: &str, expected_output: &[String]) {
        check_output(
            code,
            extensions::Extensions {
                ..Default::default()
            },
            expected_output,
        );
    }

    fn check_error(code: &str, extensions: extensions::Extensions, f: &dyn Fn(&str) -> ()) {
        let res = evaluate(code, extensions);

        match res {
            Ok(output) => panic!("{:?}", output),
            Err(err) => f(&err),
        }
    }

    fn check_error_default(code: &str, f: &dyn Fn(&str) -> ()) {
        check_error(code, extensions::Extensions::default(), f);
    }

    #[test]
    fn test_var_reading_1() {
        check_output_default("let x = 2; print(x);", &vec_of_strings!["2"]);
    }

    #[test]
    fn test_var_reading_locals_1() {
        check_output_default("{let x = 2; print(x);}", &vec_of_strings!["2"]);
    }

    #[test]
    fn test_var_reading_4() {
        check_output_default(
            "let x = 2;\n\
             let y = 3;\n\
             print(x * y + 4);",
            &vec_of_strings!["10"],
        );
    }

    #[test]
    fn test_var_reading_locals_2() {
        check_output_default(
            "{\n\
               let x = 2;\n\
               let y = 3;\n\
               print(x * y + 4);\n\
             }\n",
            &vec_of_strings!["10"],
        );
    }

    #[test]
    fn test_div_by_zero() {
        check_output_default("print(1 / 0);", &vec_of_strings!["inf"]);
    }

    #[test]
    fn test_setitem_globals() {
        check_output_default(
            "let mut breakfast = \"beignets\";\n\
             let beverage = \"cafe au lait\";\n\
             breakfast = \"beignets with \" << beverage;\n\
             print(breakfast);",
            &vec_of_strings!["beignets with cafe au lait"],
        );
    }

    #[test]
    fn test_setitem_locals() {
        check_output_default(
            "{\n\
               let mut breakfast = \"beignets\";\n\
               let beverage = \"cafe au lait\";\n\
               breakfast = \"beignets with \" << beverage;\n\
               print(breakfast);\n\
             }\n",
            &vec_of_strings!["beignets with cafe au lait"],
        );
    }

    #[test]
    fn test_read_in_own_initializer() {
        check_error_default(
            "{\n\
               let a = \"outer\";\n\
               {\n\
                let a = a;\n\
               }\n\
             }\n",
            &|err: &str| {
                assert!(err.starts_with("Cannot read local variable in its own initializer."))
            },
        )
    }

    #[test]
    fn test_if_stmt() {
        check_output_default(
            "let x = 0;\n\
             let y = 1;\n\
             if (x) {\n\
               print(x);\n\
             }\n\
             if (y) {\n\
               print(y);\n\
             }",
            &vec_of_strings!["1"],
        );
    }

    #[test]
    fn test_if_then_else_1() {
        check_output_default(
            "let x = 0;\n\
             if (x) {\n\
               print(\"hello\");\n\
             } else {\n\
               print(\"goodbye\");\n\
             }",
            &vec_of_strings!["goodbye"],
        );
    }

    #[test]
    fn test_if_then_else_2() {
        check_output_default(
            "let x = 1;\n\
             if (x) {\n\
               print(\"hello\");\n\
             } else {\n\
               print(\"goodbye\");\n\
             }",
            &vec_of_strings!["hello"],
        );
    }

    #[test]
    fn test_print_locals() {
        check_output_default(
            "{\n\
               let x = 0;\n\
               let y = 1;\n\
               print(x);\n\
               print(y);\n\
             }",
            &vec_of_strings!["0", "1"],
        );
    }

    #[test]
    fn test_print_globals() {
        check_output_default(
            "let x = 0;\n\
             let y = 1;\n\
             print(x);\n\
             print(y);\n",
            &vec_of_strings!["0", "1"],
        );
    }

    #[test]
    fn test_and_1() {
        check_output_default(
            "let x = false;\n\
             let y = true;\n\
             if (y and x) {\n\
               print(\"cat\");\n\
             } else {\n\
               print(\"dog\");\n\
             }\n",
            &vec_of_strings!["dog"],
        );
    }

    #[test]
    fn test_and_2() {
        check_output_default(
            "let x = false;\n\
             let y = true;\n\
             if (x and y) {\n\
               print(\"cat\");\n\
             } else {\n\
               print(\"dog\");\n\
             }\n",
            &vec_of_strings!["dog"],
        );
    }

    #[test]
    fn test_and_3() {
        check_output_default(
            "let x = true;\n\
             let y = true;\n\
             if (y and x) {\n\
               print(\"cat\");\n\
             } else {\n\
               print(\"dog\");\n\
             }\n",
            &vec_of_strings!["cat"],
        );
    }

    #[test]
    fn test_or_1() {
        check_output_default(
            "let x = false;\n\
             let y = true;\n\
             if (y or x) {\n\
               print(\"cat\");\n\
             } else {\n\
               print(\"dog\");\n\
             }\n",
            &vec_of_strings!["cat"],
        );
    }

    #[test]
    fn test_or_2() {
        check_output_default(
            "let x = false;\n\
             let y = true;\n\
             if (x or y) {\n\
               print(\"cat\");\n\
             } else {\n\
               print(\"dog\");\n\
             }\n",
            &vec_of_strings!["cat"],
        );
    }

    #[test]
    fn test_or_3() {
        check_output_default(
            "let x = false;\n\
             let y = false;\n\
             if (y or x) {\n\
               print(\"cat\");\n\
             } else {\n\
               print(\"dog\");\n\
             }\n",
            &vec_of_strings!["dog"],
        );
    }

    #[test]
    fn test_while() {
        check_output_default(
            "{let mut x = 0;\n\
             let mut sum = 0;\n\
             while (x < 100) {\n\
               x = x + 1;\n\
               sum = sum + x;\n\
             }\n\
             print(sum);}",
            &vec_of_strings!["5050"],
        );
    }

    #[test]
    fn test_for() {
        fn fact(n: i32) -> i32 {
            if n <= 1 {
                return 1;
            }
            return n * fact(n - 1);
        }

        check_output_default(
            "{\n\
               let mut fact = 1;\n\
               for (let mut i = 1; i <= 10; i = i + 1) {\n\
                 fact = fact * i;\n\
               }\n\
               print(fact);\n\
             }",
            &vec_of_strings![format!("{}", fact(10))],
        );
    }

    #[test]
    fn test_functions_1() {
        check_output_default(
            "fn areWeHavingItYet() {\n\
               print(\"Yes we are!\");\n\
             }\n\
             \n\
             print(areWeHavingItYet);\n",
            &vec_of_strings!["<fn 'areWeHavingItYet'>"],
        )
    }

    #[test]
    fn test_functions_2() {
        check_output_default(
            "fn f(x, y) {\n\
               print(x + y);\n\
             }\n\
             \n\
             print(f);\n",
            &vec_of_strings!["<fn 'f'>"],
        )
    }

    #[test]
    fn test_functions_3() {
        check_output_default(
            "fn f(x, y) {\n\
               return x + y;\n\
             }\n\
             \n\
             print(f);\n",
            &vec_of_strings!["<fn 'f'>"],
        )
    }

    #[test]
    fn test_functions_4() {
        check_output_default(
            "fn f() {\n\
               return;\n\
             }\n\
             \n\
             print(f());\n",
            &vec_of_strings!["nil"],
        )
    }

    #[test]
    fn test_functions_5() {
        check_error_default("return 42;", &|err: &str| {
            assert_eq!(err, "Cannot return from top-level code.")
        })
    }

    #[test]
    fn test_functions_6() {
        check_output_default(
            "fn f(x, y) {\n\
               return(x + y);\n\
             }\n\
             \n\
             print(f(1,2));\n",
            &vec_of_strings!["3"],
        );
    }

    #[test]
    fn test_functions_7() {
        check_output_default(
            "fn g(mut x) {\n\
               return 2 * x;\n\
             }\n\
             \n\
             fn f(x, y) {\n\
               return g(x) + y;\n\
             }\n\
             \n\
             print(f(1,2));\n",
            &vec_of_strings!["4"],
        );
    }

    #[test]
    fn test_functions_8() {
        check_output_default(
            "let x = 2;\n\
             fn f(x) {\n\
               print(2 * x);\n\
             }\n\
             \n\
             f(x);\n\
             print(x);\n",
            &vec_of_strings!["4", "2"],
        );
    }

    #[test]
    fn test_functions_9() {
        fn fact(n: i32) -> i32 {
            if n <= 1 {
                return 1;
            }
            return n * fact(n - 1);
        }

        check_output_default(
            "fn fact(n) {\n\
               if (n <= 1) { return 1; }\n\
               return n * fact(n - 1);\n\
             }\n\
             \n\
             print(fact(10));\n",
            &vec_of_strings![format!("{}", fact(10))],
        );
    }

    #[test]
    fn test_functions_10() {
        check_output_default(
            "fn isEven(mut n) {\n\
               if (n = 0) { return true; }\n\
               return isOdd(n - 1);\n\
             }\n\
             fn isOdd(mut n) {\n\
               if (n = 1) { return true; }\n\
               return isEven(n - 1);\n\
             }\n\
             \n\
             print(isEven(10));\n",
            &vec_of_strings!["true"],
        );
    }

    #[test]
    fn test_native_functions() {
        let res = evaluate(
            "fn fib(n) {\n\
               if (n < 2) return n;\n\
               return fib(n - 2) + fib(n - 1);\n\
             }\n\
             \n\
             let start = clock();\n\
             print(fib(5));\n\
             print(clock() - start);\n\
             print(42);",
            extensions::Extensions::default(),
        );

        match res {
            Ok(output) => {
                assert_eq!(output.len(), 3);
                assert_eq!(output[0], "5");
                assert_eq!(output[2], "42");
            }
            Err(err) => {
                panic!("{:?}", err);
            }
        }
    }

    #[test]
    fn test_get_upval_on_stack() {
        check_output_default(
            "fn outer() {\n\
               let x = \"outside\";\n\
               fn inner() {\n\
                 print(x);\n\
               }\n\
               inner();\n\
             }\n\
             outer();",
            &vec_of_strings!["outside"],
        );
    }

    #[test]
    fn test_set_upval_on_stack() {
        check_output_default(
            "fn outer() {\n\
               let x = \"before\";\n\
               fn inner() {\n\
                 x = \"assigned\";\n\
               }\n\
               inner();\n\
               print(x);\n\
             }\n\
             outer();",
            &vec_of_strings!["assigned"],
        );
    }

    #[test]
    fn test_closing_upvals_after_return() {
        check_output_default(
            "fn outer() {\n\
               let x = \"outside\";\n\
               fn inner() {\n\
                 print(x);\n\
               }\n\
               \n\
               return inner;\n\
            }\n\
            \n\
            let closure = outer();\n\
            closure();",
            &vec_of_strings!["outside"],
        );
    }

    #[test]
    fn test_closing_upvals_after_scope() {
        check_output_default(
            "let mut closure;\n\
             {\n\
               let x = \"outside\";\n\
               fn inner() {\n\
                 print(x);\n\
               }\n\
               \n\
               closure = inner;\n\
            }\n\
            \n\
            closure();",
            &vec_of_strings!["outside"],
        );
    }

    #[test]
    fn test_classes_1() {
        check_output_default(
            "class Brioche {}\n\
             print(Brioche);\n",
            &vec_of_strings!["<class 'Brioche'>"],
        );
    }

    #[test]
    fn test_classes_instances_1() {
        check_output_default(
            "class Brioche {}\n\
             let instance = new Brioche();\n\
             print(instance);\n",
            &vec_of_strings!["<Brioche instance>"],
        );
    }

    #[test]
    fn test_setattr_1() {
        check_output_default(
            "class Foo {}\n\
             let foo = new Foo();\n\
             foo.attr = 42;\n\
             print(foo.attr);\n",
            &vec_of_strings!["42"],
        );
    }

    #[test]
    fn test_setattr_2() {
        check_output_default(
            "class Toast {}\n\
             let toast = new Toast();\n\
             print(toast.jam = \"grape\");",
            &vec_of_strings!["grape"],
        );
    }

    #[test]
    fn test_setattr_3() {
        check_output_default(
            "class Pair {}\n\
             let pair = new Pair();\n\
             pair.first = 1;\n\
             pair.second = 2;\n\
             print(pair.first + pair.second);",
            &vec_of_strings!["3"],
        );
    }

    #[test]
    fn test_bound_methods_1() {
        check_output_default(
            "class Foo {\n\
                pub fn bar() {\n\
                  return 42;
                }\n\
              }\n\
              let foo = new Foo();\n\
              print(foo.bar);",
            &vec_of_strings!["<fn 'bar'>"],
        );
    }

    #[test]
    fn test_calling_bound_methods_no_this() {
        check_output_default(
            "class Scone {\n\
               pub fn topping(first, second) {\n\
                 print(\"scone with \" << first << \" and \" << second);\n\
               }\n\
             }\n\
             \n\
             let scone = new Scone();\n\
             scone.topping(\"berries\", \"cream\");",
            &vec_of_strings!["scone with berries and cream"],
        );
    }

    #[test]
    fn test_calling_bound_methods_with_this_1() {
        check_output_default(
            "class Nested {\n\
               pub fn method() {\n\
                 print(this);\n\
               }\n\
             }\n\
             \n\
             let n = new Nested();\n\
             n.method();",
            &vec_of_strings!["<Nested instance>"],
        );
    }

    #[test]
    fn test_calling_bound_methods_with_this_2() {
        check_output_default(
            "class Nested {\n\
               pub fn method() {\n\
                 fn function() {\n\
                   print(this);\n\
                 }\n\
                 \n\
                 function();\n\
               }\n\
             }\n\
             \n\
             let n = new Nested();\n\
             n.method();",
            &vec_of_strings!["<Nested instance>"],
        );
    }

    #[test]
    fn test_multiple_method_definitions() {
        check_output_default(
            "class Brunch {\n\
               pub fn bacon() {}\n\
               pub fn eggs() {}\n\
             }\n\
             let b = new Brunch();\n\
             print(b.bacon());",
            &vec_of_strings!["nil"],
        );
    }

    #[test]
    fn test_init_1() {
        check_output_default(
            "class Brunch {\n\
               fn init(x) {this.x = x;}\n\
               fn eggs(y) {return this.x + y;}\n\
             }\n\
             let b = new Brunch(2);
             print(b.eggs(3));",
            &vec_of_strings!["5"],
        );
    }

    #[test]
    fn test_invoking_fields() {
        check_output_default(
            "class Oops {\n\
                fn init() {\n\
                  fn f() {\n\
                    print(\"not a method\");\n\
                  }\n\
                  f();\n\
                }\n\
              }\n\
              let oops = new Oops();\n",
            &vec_of_strings!["not a method"],
        );
    }

    #[test]
    fn test_inheritance_1() {
        check_output_default(
            "class A {\n\
               pub fn f() {\n\
                 return \"cat\";\n\
               }\n\
             }\n\
             class B extends A {}\n\
             let b = new B();\n\
             print(b.f());",
            &vec_of_strings!["cat"],
        );
    }

    #[test]
    fn test_mutablity_gloval_instance() {
        check_error_default(
            "class A {\n\
               pub fn f() {\n\
                 return \"cat\";\n\
               }\n\
             }\n\
             class B extends A {}\n\
             let b = new B();\n\
             b = 1;",
            &|err: &str| {
                assert!(err.starts_with("This variable b is immutable but you tried to insert a value"))
            },
        );
    }

    #[test]
    fn test_mutablity_local_instance() {
        check_error_default(
            "class A {\n\
               pub fn f() {\n\
                 return \"cat\";\n\
               }\n\
             }\n\
             class B extends A {}\n\
             {let b = new B();\n\
             b = 1;}",
            &|err: &str| {
                assert!(err.starts_with("This variable is immutable but you tried to insert a value"))
            },
        );
    }

    #[test]
    fn test_mutablity_local_instance_2() {
        check_error_default(
            "{class A {\n\
               pub fn f() {\n\
                 return \"cat\";\n\
               }\n\
             }\n\
             class B extends A {}\n\
             let b = new B();\n\
             b = 1;}",
            &|err: &str| {
                assert!(err.starts_with("Undefined variable 'A' at line"))
            },
        );
    }

    #[test]
    fn test_mutablity_global_closure() {
        check_error_default(
            "let closure = 1;\n\
               let x = \"outside\";\n\
               fn inner() {\n\
                 print(x);\n\
               }\n\
               \n\
               closure = inner;\n\
            \n",
            &|err: &str| {
                assert!(err.starts_with("This variable closure is immutable but you tried to insert a value"))
            },
        );
    }
    
    #[test]
    fn test_mutablity_local_closure() {
        check_error_default(
            "{\n\
               let closure = 1;\n\
               let x = \"outside\";\n\
               fn inner() {\n\
                 print(x);\n\
               }\n\
               \n\
               closure = inner;\n\
            }\n",
            &|err: &str| {
                assert!(err.starts_with("This variable is immutable but you tried to insert a value at line"))
            },
        );
    }

    #[test]
    fn test_inheritance_2() {
        check_output_default(
            "class A {\n\
               pub fn f() {\n\
                 return \"cat\";\n\
               }\n\
             }\n\
             class B extends A {}\n\
             class C extends B {}\n\
             let c = new C();\n\
             print(c.f());",
            &vec_of_strings!["cat"],
        );
    }

    #[test]
    fn test_inheritance_3() {
        check_output_default(
            "class A {\n\
               pub fn f() {\n\
                 return this.attr;
               }\n\
             }\n\
             class B extends A {\n\
               fn init(attr) {\n\
                 this.attr = attr;\n\
               }\n\
             }\n\
             let b = new B(42);\n\
             print(b.f());",
            &vec_of_strings!["42"],
        );
    }

    #[test]
    fn test_inheritance_4() {
        check_output_default(
            "class A {\n\
               pub fn f() {\n\
                 return this.attr;
               }\n\
             }\n\
             class B extends A {\n\
             }\n\
             let b = new B();\n\
             b.attr = 42;
             print(b.f());",
            &vec_of_strings!["42"],
        );
    }

    #[test]
    fn test_inheriting_non_class() {
        check_error_default(
            "let NotClass = \"So not a class\";\n\
             class OhNo extends NotClass {}\n",
            &|err: &str| {
                assert!(err.starts_with("Superclass must be a class, found String at lineno="))
            },
        )
    }

    #[test]
    fn test_super_1() {
        check_output_default(
            "class A {\n\
               pub fn method() {\n\
                 print(\"A method\");\n\
               }\n\
             }\n\
             \n\
             class B extends A {\n\
               pub fn method() {\n\
                 print(\"B method\");\n\
               }\n\
               \n\
               pub fn test() {\n\
                 super.method();\n\
               }\n\
             }\n\
             \n\
             class C extends B {}\n\
             \n\
             let c = new C();\n\
             c.test();\n",
            &vec_of_strings!["A method"],
        )
    }

    #[test]
    fn test_super_2() {
        check_output_default(
            "class A {\n\
                pub fn method() {\n\
                  print(\"A method\");\n\
                }\n\
              }\n\
              \n\
              class B extends A {\n\
                pub fn method() {\n\
                  print(\"B method\");\n\
                }\n\
                \n\
                pub fn test() {\n\
                  super.method();\n\
                }\n\
              }\n\
              \n\
              class C extends B {}\n\
              \n\
              let c = new C();\n\
              c.test();\n",
            &vec_of_strings!["A method"],
        )
    }

    #[test]
    fn test_super_3() {
        check_output_default(
            "class Doughnut {\n\
               pub fn cook() {\n\
                 print(\"Dunk in the fryer.\");\n\
                 this.finish(\"sprinkles\");\n\
               }\n\
               \n\
               pub fn finish(ingredient) {\n\
                 print(\"Finish with \" << ingredient);\n\
               }\n\
             }\n\
             \n\
             class Cruller extends Doughnut {\n\
               pub fn finish(ingredient) {\n\
                 // No sprinkles.\n\
                 super.finish(\"icing\");\n\
               }\n\
             }\n\
             \n\
             let c = new Cruller();\n\
             let d = new Doughnut();\n\
             d.cook();\n\
             c.cook();\n",
            &vec_of_strings![
                "Dunk in the fryer.",
                "Finish with sprinkles",
                "Dunk in the fryer.",
                "Finish with icing"
            ],
        )
    }

    #[test]
    fn test_late_binding() {
        check_output_default(
            "fn a() { b(); }\n\
             fn b() { print(\"hello world\"); }\n\
             \n\
             a();\n",
            &vec_of_strings!["hello world"],
        )
    }

    #[test]
    fn test_list_building() {
        check_output_lists("print([1,2,3]);", &vec_of_strings!["[1, 2, 3]"])
    }

    #[test]
    fn test_empty_list_building() {
        check_output_lists("print([]);", &vec_of_strings!["[]"])
    }

    #[test]
    fn test_list_concat() {
        check_output_lists(
            "print([1,2,3] + [4,5,6]);",
            &vec_of_strings!["[1, 2, 3, 4, 5, 6]"],
        )
    }

    #[test]
    fn test_len() {
        check_output_lists(
            "print(len(\"\")); \n\
             print(len(\"cat\")); \n\
             print(len([])); \n\
             print(len([1,2,3,4]));",
            &vec_of_strings!["0", "3", "0", "4"],
        )
    }

    #[test]
    fn test_for_each() {
        check_output_lists(
            "fn f(arg) { print(arg); } \n\
             forEach([1,2,3,4], f);",
            &vec_of_strings!["1", "2", "3", "4"],
        )
    }

    #[test]
    fn test_map() {
        check_output_lists(
            "fn f(arg) { return arg + 1; } \n\
             print(map(f, [1,2,3,4]));",
            &vec_of_strings!["[2, 3, 4, 5]"],
        )
    }

    #[test]
    fn test_list_subscript() {
        check_output_lists(
            "let xs = [0,1]; \n\
             print(xs[0]); \n\
             print(xs[1]); \n\
             print(xs[-1]); \n\
             print(xs[-2]); \n\
             ",
            &vec_of_strings!["0", "1", "1", "0"],
        )
    }

    #[test]
    fn test_list_setitem_1() {
        check_output_lists(
            "let xs = [0,1]; \n\
             xs[-1] = 42; \n\
             print(xs);",
            &vec_of_strings!["[0, 42]"],
        )
    }

    #[test]
    fn test_list_setitem_2() {
        check_output_lists(
            "let xs = [[0,1]]; \n\
             xs[0][1] = 42; \n\
             print(xs);",
            &vec_of_strings!["[[0, 42]]"],
        )
    }

    #[test]
    fn test_list_setitem_3() {
        check_output_lists(
            "class Foo {}\n\
             let foo = new Foo();\n\
             foo.attr = [0];\n\
             foo.attr[0] = 1337;\n\
             print(foo.attr);",
            &vec_of_strings!["[1337]"],
        )
    }

    #[test]
    fn test_mutable_property_1() {
        check_output_default(
            "class A {\n\
                a = 1;\n\
              }\n\
              let a = new A();\n\
              a.a = 1;\n\
              print(a.a);",
            &vec_of_strings!["1"],
        )
    }

    #[test]
    fn test_mutable_property_2() {
        check_output_default(
            "class A {\n\
                a = 1;\n\
                b = 3;\n\
              }\n\
              let a = new A();\n\
              a.a = 1;\n\
              print(a.a);",
            &vec_of_strings!["1"],
        )
    }

    #[test]
    fn test_mutable_property_4() {
        check_output_default(
            "class A {\n\
                aa = 2;\n\
                a = 1;\n\
                b = 3;\n\
              }\n\
              let a = new A();\n\
              a.a = 1;\n\
              print(a.a);",
            &vec_of_strings!["1"],
        )
    }

    #[test]
    fn test_immutable_property_2() {
        check_error_default(
            "class A {\n\
                a = 1;\n\
                a = 3;\n\
              }",
            &|err: &str| {
                assert!(err.starts_with("This attribute is already defined"))
            },
        );
    }

    #[test]
    fn test_pub_property1() {
        check_output_default(
            "class A {\n\
                pub name1 = \"john1\";\n\
                name2 = \"john2\";\n\
            }\n\
            class B extends A {\n\
            }\n\
            let b = new B();\n\
            print(b.name1);",
            &vec_of_strings!["john1"],
        )
    }

    #[test]
    fn test_pub_property2() {
        check_error_default(
            "class A {\n\
                name1 = \"john1\";\n\
                name2 = \"john2\";\n\
            }\n\
            class B extends A {\n\
            }\n\
            let b = new B();\n\
            print(b.name1);",
            &|err: &str| {
                assert!(err.starts_with("This attribute is private"))
            },
        )
    }

    #[test]
    fn test_pub_property3() {
        check_output_default(
            "class A {\n\
                pub fn name1 () {\n\
                    return \"john1\";\n\
                }\n\
                name2 = \"john2\";\n\
            }\n\
            class B extends A {\n\
            }\n\
            let b = new B();\n\
            print(b.name1());",
            &vec_of_strings!["john1"],
        )
    }

    #[test]
    fn test_pub_property4() {
        check_error_default(
            "class A {\n\
                fn name1 () {\n\
                    return \"john1\";\n\
                }\n\
                name2 = \"john2\";\n\
            }\n\
            class B extends A {\n\
            }\n\
            let b = new B();\n\
            print(b.name1());",
            &|err: &str| {
                assert!(err.starts_with("This attribute is private"))
            },
        )
    }

    #[test]
    fn test_pub_property5() {
        check_output_default(
            "class Nested {\n\
                pub fn method() {\n\
                    this.test();\n\
                }\n\
                fn test () {\n\
                    print(\"aaa\");\n\
                }\n\
            }\n\
            let n = new Nested();\n\
            n.method();",
            &vec_of_strings!["aaa"],
        )
    }

    #[test]
    fn test_pub_property6() {
        check_output_default(
            "class Nested {\n\
                test = 1;\n\
                pub fn method() {\n\
                    print(this.test);\n\
                }\n\
            }\n\
            let n = new Nested();\n\
            n.method();",
            &vec_of_strings!["1"],
        )
    }

    #[test]
    fn test_pub_property7() {
        check_output_default(
            "class Nested {\n\
                pub fn method() {\n\
                    this.test();\n\
                }\n\
                fn test () {\n\
                    print(\"aaa\");\n\
                }\n\
            }\n\
            let n = new Nested();\n\
            n.method();",
            &vec_of_strings!["aaa"],
        )
    }

    #[test]
    fn test_pub_property8() {
        check_error_default(
            "class A {\n\
                fn name1 () {\n\
                    return \"john1\";\n\
                }\n\
                name2 = \"john2\";\n\
            }\n\
            class B extends A {}\n\
            class AA {\n\
                pub fn name1 () {\n\
                    let b = new B();\n\
                    print(b.name1());\n\
                }\n\
                name2 = \"john2\";\n\
            }\n\
            class B extends A {}\n\
            let b = new AA();\n\
            print(b.name1());",
            &|err: &str| {
                assert!(err.starts_with("This attribute is private"))
            },
        )
    }

    #[test]
    fn test_mut_args() {
        check_output_default(
            "fn hello (mut a,b,d) {\n\
                a = 1;\n\
                return a;\n\
            }\n\
            print(hello(5,1,1));\n",
            &vec_of_strings!["1"],
        )
    }

    #[test]
    fn test_immutable_args() {
        check_error_default(
            "fn hello (a,b,d) {\n\
                a = 1;
                return a;\n\
            }\n\
            print(hello(5,1,1));\n",
            &|err: &str| {
                assert!(err.starts_with("This variable is immutable but you tried to insert a value"))
            },
        );
    }

    #[test]
    fn test_many_args_1() {
        check_output_default(
            "fn hello (a,b,d) {\n\
                return a;\n\
            }\n\
            print(hello(5,1,1));\n",
            &vec_of_strings!["5"],
        )
    }

    #[test]
    fn test_many_args_2() {
        check_output_default(
            "fn hello (a,b,d,e) {\n\
                return b;\n\
            }\n\
            print(hello(1,2,3,4));\n",
            &vec_of_strings!["2"],
        )
    }

    #[test]
    fn test_many_args_3() {
        check_output_default(
            "fn hello (a,b,c,d,e) {\n\
                return c;\n\
            }\n\
            print(hello(1,2,3,4,5));\n",
            &vec_of_strings!["3"],
        )
    }

    #[test]
    fn test_many_args_4() {
        check_output_default(
            "fn hello (a,b,c,d,e,f) {\n\
                return d;\n\
            }\n\
            print(hello(1,2,3,4,5,6));\n",
            &vec_of_strings!["4"],
        )
    }

    #[test]
    fn test_many_args_5() {
        check_output_default(
            "fn hello (a,b,c,d,e,f,g) {\n\
                return e;\n\
            }\n\
            print(hello(1,2,3,4,5,6,7));\n",
            &vec_of_strings!["5"],
        )
    }

    #[test]
    fn test_many_args_6() {
        check_output_default(
            "fn hello (a,b,c,d,e,f,g,h) {\n\
                return f;\n\
            }\n\
            print(hello(1,2,3,4,5,6,7,8));\n",
            &vec_of_strings!["6"],
        )
    }

    #[test]
    fn test_many_args_7() {
        check_output_default(
            "fn hello (a,b,c,d,e,f,g,h,i) {\n\
                return g;\n\
            }\n\
            print(hello(1,2,3,4,5,6,7,8,9));\n",
            &vec_of_strings!["7"],
        )
    }

    #[test]
    fn test_many_args_8() {
        check_output_default(
            "fn hello (a,b,c,d,e,f,g,h,i,j) {\n\
                return h;\n\
            }\n\
            print(hello(1,2,3,4,5,6,7,8,9,10));\n",
            &vec_of_strings!["8"],
        )
    }

    #[test]
    fn test_return_nosemicolon_1() {
        check_output_default(
            "fn hello (a) {\n\
                a\n\
            }\n\
            print(hello(1));\n",
            &vec_of_strings!["1"],
        )
    }

    #[test]
    fn test_return_nosemicolon_2() {
        check_output_default(
            "fn hello (a) {\n\
                a + 1\n\
            }\n\
            print(hello(1));\n",
            &vec_of_strings!["2"],
        )
    }

    #[test]
    fn test_return_nosemicolon_3() {
        check_output_default(
            "fn hello (a) {\n\
                let test = \"$\";\n\
                test << (a + 1) * 5\n\
            }\n\
            print(hello(1));\n",
            &vec_of_strings!["$10"],
        )
    }

    #[test]
    fn test_return_nosemicolon_4() {
        check_output_default(
            "fn hello (a, b) {\n\
                let test = \"$\";\n\
                test << (b + 1) * 5\n\
            }\n\
            print(hello(1, 2));\n",
            &vec_of_strings!["$15"],
        )
    }

    #[test]
    fn test_return_nosemicolon_5() {
        check_output_default(
            "class A {\n\
                pub fn name1 () {\n\
                    \"john1\"\n\
                }\n\
                name2 = \"john2\";\n\
            }\n\
            class B extends A {}\n\
            let b = new B();\n\
            print(b.name1());",
            &vec_of_strings!["john1"],
        )
    }
}
