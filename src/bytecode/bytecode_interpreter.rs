use crate::bytecode::bytecode;
use crate::bytecode::bytecode::Order;
use crate::gc::gc;
use crate::reader::value;
use crate::reader::value::MarieValue;
use crate::reader::value::PropertyKey;
use super::bytecode::ValueMeta;
use super::StepResult;
use super::call_frame::CallFrame;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

#[allow(dead_code)]
#[derive(Debug)]
enum Binop {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Modulus,
}

impl std::fmt::Display for Binop {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Binop::Add => write!(fmt, "Add"),
            Binop::Sub => write!(fmt, "Sub"),
            Binop::Mul => write!(fmt, "Mul"),
            Binop::Div => write!(fmt, "Div"),
            Binop::Pow => write!(fmt, "Pow"),
            Binop::Modulus => write!(fmt, "Modulus"),
        }
    }
}

pub struct Interpreter {
    pub frames: Vec<CallFrame>,
    pub stack: Vec<value::Value>,
    pub stack_meta: Vec<ValueMeta>,
    output: Vec<String>,
    pub globals: HashMap<String, value::Value>,
    pub upvalues: Vec<Rc<RefCell<value::Upvalue>>>,
    pub heap: gc::Heap,
    gray_stack: Vec<gc::HeapId>,
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

impl Interpreter {
    pub fn prepare_interpret(&mut self, func: bytecode::Function) {
        self.stack
            .push(value::Value::Function(self.heap.manage_closure(
                value::Closure {
                    function: func.clone(),
                    upvalues: Vec::new(),
                },
            )));
        self.stack_meta.push(ValueMeta { is_public: true, is_mutable: true, });
        self.frames.push(CallFrame {
            closure: value::Closure {
                function: func,
                upvalues: Vec::new(),
            },
            instruction_pointer: 0,
            slots_offset: 1,
            invoked_method_id: None,
            is_include_file: false
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
                StepResult::Ok(_) => {},
                StepResult::OkReturn(_) => {
                    return Ok(());
                },
                StepResult::Err(err)  => {
                    return Err(err);
                }
             }
        }
    }

    // This is_done function is for debugger
    pub fn is_done(&self) -> bool {
        self.frames.is_empty() || self.frame().instruction_pointer >= self.frame().closure.function.chunk.code.len()
    }

    pub fn step(&mut self) -> StepResult<(), InterpreterError> {
        let op = self.next_op_and_advance();

        if self.heap.should_collect() {
            self.collect_garbage();
        }
        let lineno = op.lineno;
        match op.operation {
            _ => todo!()
        }
        StepResult::Ok(())
    }

    fn frame_mut(&mut self) -> &mut CallFrame {
        let frames_len = self.frames.len();
        &mut self.frames[frames_len - 1]
    }

    pub fn maybe_frame(&self) -> Option<&CallFrame> {
        self.frames.last()
    }

    pub fn frame(&self) -> &CallFrame {
        self.maybe_frame().unwrap()
    }

    fn close_upvalues(&mut self, index: usize) {
        let value = &self.stack[index];
        for upval in &self.upvalues {
            if upval.borrow().is_open_with_index(index) {
                upval.replace(value::Upvalue::Closed(value.clone()));
            }
        }

        self.upvalues.retain(|u| u.borrow().is_open());
    }

    fn find_open_uval(&self, index: usize) -> Option<Rc<RefCell<value::Upvalue>>> {
        for upval in self.upvalues.iter().rev() {
            if upval.borrow().is_open_with_index(index) {
                return Some(upval.clone());
            }
        }

        None
    }

    /*
    Set up a few call frame so that on the next interpreter step we'll start executing code inside the function.
     */
    fn push_frame_prepare_call(
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
        let frame = self.frames.last_mut().unwrap();
        frame.closure = closure;
        frame.slots_offset = self.stack.len() - usize::from(arg_count);
        frame.invoked_method_id = Some(closure_handle);
        Ok(())
    }

    fn pop_stack_n_times(&mut self, num_to_pop: usize) {
        for _ in 0..num_to_pop {
            self.pop_stack();
        }
    }

    fn pop_stack_meta_n_times(&mut self, num_to_pop: usize) {
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

    fn peek(&self) -> &value::Value {
        self.peek_by(0)
    }

    fn peek_by(&self, n: usize) -> &value::Value {
        &self.stack[self.stack.len() - n - 1]
    }

    fn peek_meta(&self) -> &ValueMeta {
        self.peek_meta_by(0)
    }

    fn peek_meta_by(&self, n: usize) -> &ValueMeta {
        &self.stack_meta[self.stack_meta.len() - n - 1]
    }

    pub fn next_line(&self) -> usize {
        self.next_op().lineno.value
    }

    pub fn next_op(&self) -> Order {
        self.frame().next_op()
    }

    fn next_op_and_advance(&mut self) -> Order {
        self.frame_mut().next_op_and_advance()
    }

    fn read_constant(&mut self, idx: usize) -> value::Value {
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

    fn read_constant_meta(&mut self, idx: usize) -> ValueMeta {
        self.frame().read_constant_meta(idx)
    }

    pub fn set_constant_meta(&mut self, idx: usize, meta: ValueMeta) {
        self.frame_mut().set_constant_meta(idx, meta);
    }

    fn _is_constants_empty(&self) -> bool {
        self.frame()._is_constants_empty()
    }

    fn extract_bool(val: &value::Value) -> Option<bool> {
        match val {
            value::Value::Bool(b) => Some(*b),
            _ => None,
        }
    }

    fn get_str(&self, str_handle: gc::HeapId) -> &String {
        self.heap.get_str(str_handle)
    }

    fn get_closure(&self, closure_handle: gc::HeapId) -> &value::Closure {
        self.heap.get_closure(closure_handle)
    }

    fn get_class(&self, class_handle: gc::HeapId) -> &value::Class {
        self.heap.get_class(class_handle)
    }

    fn get_class_mut(&mut self, class_handle: gc::HeapId) -> &mut value::Class {
        self.heap.get_class_mut(class_handle)
    }

    fn get_bound_method(&self, method_handle: gc::HeapId) -> &value::BoundMethod {
        self.heap.get_bound_method(method_handle)
    }

    fn get_list_elements(&self, list_handle: gc::HeapId) -> &Vec<value::Value> {
        self.heap.get_list_elements(list_handle)
    }

    fn get_list_elements_mut(&mut self, list_handle: gc::HeapId) -> &mut Vec<value::Value> {
        self.heap.get_list_elements_mut(list_handle)
    }

    fn get_instance(&self, instance_handle: gc::HeapId) -> &value::Instance {
        self.heap.get_instance(instance_handle)
    }

    fn collect_garbage(&mut self) {
        self.heap.unmark();
        self.mark_roots();
        self.trace_references();

        self.heap.sweep();
    }

    fn trace_references(&mut self) {
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

    use std::env;

    use crate::bytecode::bytecode_interpreter::*;
    use crate::compiler::*;

    fn evaluate(code: &str) -> Result<Vec<String>, String> {
        let func_or_err = Compiler::compile(String::from(code));

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

    fn check_output(code: &str, expected_output: &[String]) {
        let res = evaluate(code);

        match res {
            Ok(output) => assert_eq!(output, expected_output),
            Err(err) => panic!("{}", err),
        }
    }

    fn check_output_default(code: &str, expected_output: &[String]) {
        check_output(code, expected_output);
    }

    fn check_output_lists(code: &str, expected_output: &[String]) {
        check_output(
            code,
            expected_output,
        );
    }

    fn check_error(code: &str, f: &dyn Fn(&str) -> ()) {
        let res = evaluate(code);

        match res {
            Ok(output) => panic!("{:?}", output),
            Err(err) => f(&err),
        }
    }

    fn check_error_default(code: &str, f: &dyn Fn(&str) -> ()) {
        check_error(code, f);
    }

    #[test]
    fn test_pow_1() {
        check_output_default("let x = 2 ** 3; print(x);", &vec_of_strings!["8"]);
    }

    #[test]
    fn test_pow_2() {
        check_output_default("let x = (2+1) ** 3; print(x);", &vec_of_strings!["27"]);
    }

    #[test]
    fn test_pow_3() {
        check_output_default("let x = 1 + (2+1) ** 3; print(x);", &vec_of_strings!["28"]);
    }

    #[test]
    fn test_pow_4() {
        check_output_default("let x = 1 + (2+1) ** 3 - 1; print(x);", &vec_of_strings!["27"]);
    }

    #[test]
    fn test_pow_5() {
        check_output_default("let x = (2+3)**2+1 / (4+1); print(x);", &vec_of_strings!["25.2"]);
    }
    
    #[test]
    fn test_modulus_1() {
        check_output_default("let x = 1 % 2 + (2+1) ** 3 - 1; print(x);", &vec_of_strings!["27"]);
    }

    #[test]
    fn test_modulus_2() {
        check_output_default("let x = 2 % 2 + (2+1) ** 3 - 1; print(x);", &vec_of_strings!["26"]);
    }
    
    #[test]
    fn test_modulus_3() {
        check_output_default("let x = 2 % 3 + (2+1) ** 3 - 1 + 9 % 3; print(x);", &vec_of_strings!["28"]);
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
             if (y && x) {\n\
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
             if (x && y) {\n\
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
             if (y && x) {\n\
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
             if (y || x) {\n\
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
             if (x || y) {\n\
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
             if (y || x) {\n\
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
            &vec_of_strings!["null"],
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
        );

        match res {
            Ok(output) => {
                assert_eq!(output.len(), 3);
                assert_eq!(output[0], "5");
                assert_eq!(output[2], "42");
            }
            Err(err) => {
                panic!("{}", err);
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
            &vec_of_strings!["null"],
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
