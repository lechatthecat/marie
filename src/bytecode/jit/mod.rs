use crate::bytecode::{bytecode::ValueMeta, values::value::Type};

pub mod jit;
pub mod call_func_pointer;
mod function_translator;

pub fn pack_meta(m: &ValueMeta) -> u8 {
    let tag = match m.value_type {
        Type::Number => 0,
        Type::String => 1,
        Type::Bool   => 2,
        Type::Null   => 3,
        Type::Function => 4,
        Type::NativeFunction => 5,
        Type::Class => 6,
        Type::BoundMethod => 7,
        Type::Instance => 8,
        Type::List => 9,
    };
    (tag << 2)
        | (m.is_public  as u8)
        | ((m.is_mutable as u8) << 1)
}

pub fn unpack_meta(b: u8) -> ValueMeta {
    let tag = (b >> 2) & 0b1111;
    ValueMeta {
        is_public:  b & 1 != 0,
        is_mutable: b & 2 != 0,
        value_type: match tag {
            0 => Type::Number,
            1 => Type::String,
            2 => Type::Bool,
            3 => Type::Null,
            4 => Type::Function,
            5 => Type::NativeFunction,
            6 => Type::Class,
            7 => Type::BoundMethod,
            8 => Type::Instance, // or Type::List, depending on your encoding
            9 => Type::List,
            _ => panic!("Unknown tag value: {}", tag),
        },
    }
}

