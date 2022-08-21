use std::{mem, slice};

use crate::value::{MarieValue, self, Value, JitResult};

#[no_mangle]
pub extern "C" fn println_num(word: f64) {
    println!("{}", word);
}

#[no_mangle]
pub extern "C" fn f64_to_bits(word: f64) -> u64 {
    word.to_bits()
}

#[no_mangle]
pub extern "C" fn bits_to_f64(word: i64) -> f64 {
    f64::from_bits(word as u64)
}

#[no_mangle]
pub extern "C" fn print_num(word: f64) {
    print!("{}", word);
}

// #[no_mangle]
// pub extern "C" fn test2 (word: usize) -> usize {
//     let val = unsafe{ std::ptr::read(word as *const MarieValue) };
//     let value_type = value::type_of(&val.val);

//     0
// }

// #[no_mangle]
// pub extern "C" fn test (word: u64) -> u64 {
//     let a = word;
//     println!("{}", word);
//     0
// }

#[no_mangle]
pub extern "C" fn make_return_val(result_ptr: i64, err_string_ptr: i64, is_error: bool) -> i64 {
    let mut r = unsafe {Box::from_raw(result_ptr as *mut JitResult)};
    r.jit_error = "".to_string();
    Box::into_raw(r) as i64
}

#[no_mangle]
pub extern "C" fn test1(ptr: i64) -> i64 {
    let mut r = unsafe {Box::from_raw(ptr as *mut JitResult)};
    println!("test1 1: {:?}", r);
    r.jit_error = "aa".to_string();
    println!("test1 2: {:?}", r);
    Box::into_raw(r) as i64
}

#[no_mangle]
pub extern "C" fn test2(ptr: i64) {
    let r = unsafe {Box::from_raw(ptr as *mut JitResult)};
    println!("test 2 (heap consumed): {:?}", r);
}

#[no_mangle]
pub extern "C" fn print_jitval (word: f64) {
    println!("{}", word);
}

#[no_mangle]
pub extern "C" fn is_f64 (word: i64) -> bool {
    word == 1
}

#[no_mangle]
pub extern "C" fn i64_to_i64 (word: i64) -> i64 {
    word
}

// #[no_mangle]
// pub extern "C" fn format_val(val: &value::Value) -> String {
//     match val {
//         value::Value::Number(num) => num.to_string(),
//         value::Value::Bool(b) => b.to_string(),
//         value::Value::String(str_handle) => self.get_str(*str_handle).clone(),
//         value::Value::Function(closure_handle) => {
//             format!("<fn '{}'>", self.get_closure(*closure_handle).function.name)
//         }
//         value::Value::Class(class_handle) => {
//             format!("<class '{}'>", self.get_class(*class_handle).name)
//         }
//         value::Value::Instance(instance_handle) => {
//             let instance = self.get_instance(*instance_handle);
//             let class_name = &self.get_class(instance.class_id).name;
//             format!("<{} instance>", class_name)
//         }
//         value::Value::NativeFunction(func) => format!("<native fn {}>", func.name),
//         value::Value::BoundMethod(bound_method_id) => {
//             let bound_method = self.get_bound_method(*bound_method_id);
//             let instance = self.get_instance(bound_method.instance_id);
//             let class_name = &self.get_class(instance.class_id).name;
//             format!("<bound method of {} instance>", class_name)
//         }
//         value::Value::Nil => "nil".to_string(),
//         value::Value::List(list_id) => {
//             let elements = self.get_list_elements(*list_id);
//             format!(
//                 "[{}]",
//                 elements
//                     .iter()
//                     .map(|element| self.format_val(&element.val))
//                     .collect::<Vec<String>>()
//                     .join(", ")
//             )
//         },
//         value::Value::Errored => "errored".to_string(),
//     }
// }
