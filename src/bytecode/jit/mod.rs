use crate::bytecode::{bytecode::ValueMeta, values::value::Type};

pub mod jit;
pub mod call_func_pointer;
pub mod jitcache;
mod rust_defined_functions;
mod function_translator;

#[inline]
pub fn pack_meta(m: &ValueMeta) -> i64 {
    // タグ部分（4 bit）
    let tag: u8 = match m.value_type {
        Type::Number         => 0,
        Type::String         => 1,
        Type::Bool           => 2,
        Type::Null           => 3,
        Type::Function       => 4,
        Type::NativeFunction => 5,
        Type::Class          => 6,
        Type::BoundMethod    => 7,
        Type::Instance       => 8,
        Type::List           => 9,
    };

    // | tag(4) | mutable(1) | public(1) | padding(2) |
    let packed_u8: u8 =
        ((tag << 2) & 0b1111_1100)           // tag を上位 4 bit へ
      | ((m.is_public  as u8) & 0b0000_0001) // public → bit0
      | ((m.is_mutable as u8) << 1);         // mutable → bit1

    packed_u8 as i64   // 下位 8 bit だけを使うが i64 で返す
}

#[inline]
pub fn unpack_meta(raw: i64) -> ValueMeta {
    // 下位 1 バイトにしか意味が無いのでマスクして u8 に落とす
    let b = (raw & 0xFF) as u8;

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
            8 => Type::Instance,
            9 => Type::List,
            _ => panic!("Unknown tag value: {}", tag),
        },
    }
}
