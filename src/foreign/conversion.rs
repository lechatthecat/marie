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
pub extern "C" fn f64_to_i64bits(word: f64) -> i64 {
    word.to_bits() as i64
}

#[no_mangle]
pub extern "C" fn nil_to_i64() -> i64 {
    0
}

#[no_mangle]
pub extern "C" fn bool_to_i64(val: bool) -> i64 {
    val as i64
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
    let a = f64::from_bits(word as u64);
    let b = 1;
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
pub extern "C" fn test1(ptr: *mut MarieValue) -> i64 {
    let mut r = unsafe {Box::from_raw(ptr)};
    println!("test1 1: {}", r);
    r.val = value::Value::Number(5f64);
    println!("test1 2: {}", r);
    Box::into_raw(r) as i64
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
pub extern "C" fn print_number (word: f64) {
    println!("{}", word);
}

#[no_mangle]
pub extern "C" fn print_string (ptr: *mut String) -> i64 {
    let string_to_show = unsafe {Box::from_raw(ptr)};
    println!("{}", string_to_show);
    Box::into_raw(Box::new(string_to_show)) as i64 // TODO メモリリークになるのでは
}

// TODO allocのほうがいいかも。そうでないとcompareのたびにもとのstringのデータが使えなくなる
// https://stackoverflow.com/questions/48485454/rust-manual-memory-management
#[no_mangle]
pub extern "C" fn compare_strings (ptr1: *mut String, ptr2: *mut String) -> bool {
    let string_to_compare1 = unsafe {Box::from_raw(ptr1)};
    let string_to_compare2 = unsafe {Box::from_raw(ptr2)};
    let is_same = string_to_compare1 == string_to_compare2;
    is_same
}

#[no_mangle]
pub extern "C" fn print_bool (boolval: bool) {
    println!("{}", boolval);
}

#[no_mangle]
pub extern "C" fn is_true (word: i64) -> bool {
    word == 1
}

#[no_mangle]
pub extern "C" fn negate (num: f64) -> f64 {
    -num
}

#[no_mangle]
pub extern "C" fn bool_not (boolval: bool) -> bool {
    !boolval
}
