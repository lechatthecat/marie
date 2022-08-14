use std::{mem, slice};

use crate::value::{MarieValue, self, Value};

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

#[no_mangle]
pub extern "C" fn test2 (word: usize) -> usize {
    let val = unsafe{ std::ptr::read(word as *const MarieValue) };
    let value_type = value::type_of(&val.val);

    0
}

#[no_mangle]
pub extern "C" fn test (word: u64) -> u64 {
    let a = word;
    println!("{}", word);
    0
}

#[no_mangle]
pub extern "C" fn try_add (word1: usize, word2: usize, word3: usize) -> usize {
    let val1 = unsafe{ std::ptr::read(word1 as *mut Value) };
    let val2 = unsafe{ std::ptr::read(word2 as *mut Value) };
    let mut answer = unsafe{ std::ptr::read(word3 as *mut Value) };
    let mut is_number1 = false;
    let mut is_number2 = false;
    if let value::Value::Number(num1) = &val1 {
        is_number1 = true;
        if let value::Value::Number(num2) = &val2 {
            is_number2 = true;
            let answer_num = num1 + num2;
            answer = value::Value::Number(answer_num);
        }
    }

    if !is_number1 || !is_number2 {
        return 1;
    }
    let boxed = unsafe { value::any_as_u8_slice(&mut answer) };
    boxed.as_ptr() as *mut Value as usize
}

#[no_mangle]
pub extern "C" fn print_jitval (word: f64) {
    println!("{}", word);
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
