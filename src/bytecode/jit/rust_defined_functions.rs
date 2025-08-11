use core::panic;

use crate::bytecode::{bytecode_interpreter::Interpreter, jit::unpack_meta, values::value};

pub extern "sysv64" fn native_print(
    vm: *mut Interpreter,
    value_bits: i64,
    meta_bits: i64,
) {
    let vm = unsafe {
        &mut *vm
    };
    let meta = unpack_meta(meta_bits);
    let val = match meta.value_type {
        value::Type::Number =>
            value::Value::Number(f64::from_bits(value_bits as u64)),
        value::Type::String =>
            value::Value::String(value_bits as usize),
        value::Type::Bool => value::Value::Bool(value_bits != 0),
        value::Type::Null => value::Value::Null,
        _ => panic!("This value type is not supported by print"),
    };
    vm.print_val(&val);
}
