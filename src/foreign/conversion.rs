use std::{mem, slice, ffi::CStr};

use crate::value::{MarieValue, self, Value, JitParameter};

// #[no_mangle]
// pub extern "C" fn println_num(word: f64) {
//     println!("{}", word);
// }

// #[no_mangle]
// pub extern "C" fn print_num(word: f64) {
//     print!("{}", word);
// }

#[no_mangle]
pub extern "C" fn f64_to_bits(word: f64) -> u64 {
    word.to_bits()
}

#[no_mangle]
pub extern "C" fn i64_to_bool(word: i64) -> bool {
    word == 1
}

#[no_mangle]
pub extern "C" fn bits_to_f64(word: i64) -> f64 {
    f64::from_bits(word as u64)
}

#[no_mangle]
pub extern "C" fn printtest(word: i64) {
    println!("{}", word);
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
pub extern "C" fn string_to_jitval(val: usize) -> i64 {
    let v = MarieValue {
        is_public: true,
        is_mutable: true,
        val: value::Value::String(val),
        jit_value: None,
    };
    Box::into_raw(Box::new(v)) as i64
}

#[no_mangle]
pub extern "C" fn nil_to_jitval() -> i64 {
    let v = MarieValue {
        is_public: true,
        is_mutable: true,
        val: value::Value::Nil,
        jit_value: None,
    };
    Box::into_raw(Box::new(v)) as i64
}

#[no_mangle]
pub extern "C" fn bool_to_jitval(val: bool) -> i64 {
    let v = MarieValue {
        is_public: true,
        is_mutable: true,
        val: value::Value::Bool(val),
        jit_value: None,
    };
    Box::into_raw(Box::new(v)) as i64
}

#[no_mangle]
pub extern "C" fn f64_to_jitval(val: f64) -> i64 {
    let v = MarieValue {
        is_public: true,
        is_mutable: true,
        val: value::Value::Number(val),
        jit_value: None,
    };
    Box::into_raw(Box::new(v)) as i64
}

#[no_mangle]
pub extern "C" fn test1(ptr: *mut MarieValue) -> i64 {
    let mut r = unsafe {Box::from_raw(ptr)};
    println!("test1 1: {}", r);
    r.val = value::Value::Number(5f64);
    println!("test1 2: {}", r);
    Box::into_raw(r) as i64
}

#[no_mangle]
pub extern "C" fn marieval_to_jitval(ptr: *mut JitParameter) -> i64 {
    let r = unsafe {Box::from_raw(ptr)};
    r.value
}

#[no_mangle]
pub extern "C" fn marieval_to_f64(ptr: *mut MarieValue) -> f64 {
    let r = unsafe {Box::from_raw(ptr)};
    if let value::Value::Number(val) = r.val {
        val
    } else {
        0.0
    }
}

#[no_mangle]
pub extern "C" fn marieval_to_string(ptr: *mut MarieValue) -> i64 {
    let r = unsafe {Box::from_raw(ptr)};
    if let value::Value::String(val_ptr) = r.val {
        val_ptr as i64
    } else {
        0
    }
}

#[no_mangle]
pub extern "C" fn marieval_to_bool(ptr: *mut MarieValue) -> bool {
    let r = unsafe {Box::from_raw(ptr)};
    if let value::Value::Bool(bool_val) = r.val {
        bool_val
    } else {
        true
    }
}

#[no_mangle]
pub extern "C" fn marieval_to_jittype(ptr: *mut JitParameter) -> i64 {
    let r = unsafe {Box::from_raw(ptr)};
    let a = *r;
    Box::into_raw(Box::new(a.clone()));
    a.value_type
}

#[no_mangle]
pub extern "C" fn make_err_val_type(type1: i64, type2: i64) -> i64 {
    let val = MarieValue {
        is_public: false,
        is_mutable: false,
        val: value::Value::Err(
            format!("Wrong type of value. Expected: {}, found: {}",
                value::type_id_to_string(type1 as usize),
                value::type_id_to_string(type2 as usize),
            )
        ),
        jit_value: None,
    };
    Box::into_raw(Box::new(val)) as i64
}

#[no_mangle]
pub extern "C" fn test2(ptr: *mut MarieValue) {
    let r = unsafe {Box::from_raw(ptr)};
    println!("test 2: {}", r);
    Box::into_raw(r); // これがないとheapがconsumeされるので2回目以降でエラーになる
}

#[no_mangle]
pub extern "C" fn test3(ptr: *mut String) {
    let r = unsafe {Box::from_raw(ptr)};
    println!("test 3: {}", r);
    Box::into_raw(r); // これがないとheapがconsumeされるので2回目以降でエラーになる
}

#[no_mangle]
pub extern "C" fn print_jitval (word: f64) {
    println!("{}", word);
}

#[no_mangle]
pub extern "C" fn print_string_jitval (ptr: *mut String) -> i64 {
    let string_to_show = unsafe {Box::from_raw(ptr)};
    println!("{}", string_to_show);
    Box::into_raw(Box::new(string_to_show)) as i64 // TODO メモリリークになるのでは
}

#[no_mangle]
pub extern "C" fn print_bool_jitval (boolval: bool) {
    println!("{}", boolval);
}

#[no_mangle]
pub extern "C" fn is_true (word: i64) -> bool {
    word == 1
}

#[no_mangle]
pub extern "C" fn i64_to_i64 (word: i64) -> i64 {
    word
}
