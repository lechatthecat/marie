use crate::{
    bytecode::{
        bytecode::{self, Order, ValueMeta},
        bytecode_interpreter::Interpreter,
        values::value,
    },
    gc::gc,
};

impl Interpreter {
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

    pub fn get_mut_closure(&mut self, closure_handle: gc::HeapId) -> &mut value::Closure {
        self.heap.get_mut_closure(closure_handle)
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

    pub fn get_list_elements(&self, list_handle: gc::HeapId) -> &Vec<value::Value> {
        self.heap.get_list_elements(list_handle)
    }

    pub fn get_list_elements_mut(&mut self, list_handle: gc::HeapId) -> &mut Vec<value::Value> {
        self.heap.get_list_elements_mut(list_handle)
    }

    pub fn get_instance(&self, instance_handle: gc::HeapId) -> &value::Instance {
        self.heap.get_instance(instance_handle)
    }

    pub fn pop_stack_n_times(&mut self, num_to_pop: usize) {
        for _ in 0..num_to_pop {
            self.pop_stack();
        }
    }

    pub fn pop_stack_meta_n_times(&mut self, num_to_pop: usize) {
        for _ in 0..num_to_pop {
            self.pop_stack_meta();
        }
    }

    pub fn pop_stack(&mut self) -> value::Value {
        match self.stack.pop() {
            Some(val) => val,
            None => panic!("attempted to pop empty stack!"),
        }
    }

    pub fn pop_stack_meta(&mut self) -> ValueMeta {
        match self.stack_meta.pop() {
            Some(val) => val,
            None => panic!("attempted to pop empty stack meta!"),
        }
    }

    pub fn pop_stack_and_stackmeta(&mut self) {
        self.pop_stack();
        self.pop_stack_meta();
    }

    pub fn peek(&self) -> &value::Value {
        self.peek_by(0)
    }

    pub fn peek_by(&self, n: usize) -> &value::Value {
        &self.stack[self.stack.len() - n - 1]
    }

    pub fn peek_meta(&self) -> &ValueMeta {
        self.peek_meta_by(0)
    }

    pub fn peek_meta_by(&self, n: usize) -> &ValueMeta {
        &self.stack_meta[self.stack_meta.len() - n - 1]
    }

    pub fn next_line(&self) -> usize {
        self.next_op().lineno.value
    }

    pub fn next_op(&self) -> Order {
        self.frame().next_op()
    }

    pub fn next_op_and_advance(&mut self) -> Order {
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
                }))
            }
        }
    }

    pub fn read_constant_meta(&mut self, idx: usize) -> ValueMeta {
        self.frame().read_constant_meta(idx)
    }

    pub fn set_constant_meta(&mut self, idx: usize, meta: ValueMeta) {
        self.frame_mut().set_constant_meta(idx, meta);
    }
}
