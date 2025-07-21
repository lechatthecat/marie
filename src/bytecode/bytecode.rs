use crate::bytecode::bytecode;
use crate::bytecode::values::value::Type;
use serde::{Deserialize, Serialize};

use std::f64;
use std::fmt;

#[derive(Default, Copy, Clone, Debug)]
pub struct Lineno {
    pub value: usize,
}

impl std::fmt::Display for Lineno {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(fmt, "{}", self.value)
    }
}

#[allow(non_snake_case)]
pub fn Lineno(value: usize) -> Lineno {
    Lineno { value }
}

pub const N_OPS: usize = 48;

#[repr(u8)]
#[derive(Serialize, Deserialize, Debug, Clone, Copy)]
#[serde(untagged)]
pub enum Opcode {
    Return = 0,
    Constant,
    Closure,
    Null,
    True,
    False,
    Negate,
    Add,
    AddString,
    Subtract,
    Multiply,
    Divide,
    Pow,
    Modulus,
    Not,
    Equal,
    Greater,
    Less,
    Print,
    Pop,
    DefineGlobal,
    DefineLocal,
    GetGlobal,
    SetGlobal,
    GetLocal,
    SetLocal,
    JumpIfFalse,
    Jump,
    Loop,
    Call,
    CreateInstance,
    Class,
    DefineProperty,
    SetProperty,
    GetProperty,
    Method,
    Invoke,
    Inherit,
    GetSuper,
    SuperInvoke,
    BuildList,
    Subscr,
    SetItem,
    StartInclude,
    DefineArgumentLocal,
    EndOfScope,
    //PrepareIf,
    BeginIf,
    EndIf,
    PrepareElseIf,
    BeginElseIf,
    EndElseIf,
    BeginElse,
    EndAllIf,
    JitIgnoredPop,
}

#[derive(Default, Clone, Debug)]
pub struct Function {
    pub arity: u8,
    pub locals_size: u8,
    pub chunk: Chunk,
    pub name: String,
}

#[derive(Debug, Clone, Default)]
pub struct Closure {
    pub function: Function,
}

#[derive(Debug, Clone)]
pub enum Constant {
    Number(f64),
    String(String),
    Function(Closure),
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Constant::Number(n) => write!(f, "{}", n),
            Constant::String(s) => write!(f, "\"{}\"", s),
            Constant::Function(Closure {
                function:
                    Function {
                        arity: _,
                        locals_size: _,
                        chunk: _,
                        name,
                    },
            }) => write!(f, "<fn {}>", name),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Order {
    pub operand: u32,
    pub opcode: bytecode::Opcode,
    pub lineno: bytecode::Lineno,
}

const FLAG31_IS_MUTABLE: u32 = 1 << 31; // 最上位 1bit
const FLAG30_IS_PUBLIC: u32 = 1 << 30; // その次

#[inline]
pub const fn mask(bits: u32) -> u32 {
    (1 << bits) - 1
}

#[inline]
pub fn pack_two_flags(flag_a: bool, flag_b: bool, idx: usize) -> u32 {
    (idx as u32 & mask(30))
        | if flag_a { FLAG31_IS_MUTABLE } else { 0 }
        | if flag_b { FLAG30_IS_PUBLIC } else { 0 }
}

#[inline]
pub fn unpack_two_flags(raw: u32) -> (bool, bool, usize) {
    let a = (raw & FLAG31_IS_MUTABLE) != 0;
    let b = (raw & FLAG30_IS_PUBLIC) != 0;
    let idx = (raw & mask(30)) as usize;
    (a, b, idx)
}

#[inline]
pub fn pack_one_flag(flag: bool, idx: usize) -> u32 {
    (idx as u32 & mask(31)) | if flag { FLAG31_IS_MUTABLE } else { 0 }
}

#[inline]
pub fn unpack_one_flag(raw: u32) -> (bool, usize) {
    let flag = (raw & FLAG31_IS_MUTABLE) != 0;
    let idx = (raw & mask(31)) as usize;
    (flag, idx)
}

#[inline]
pub fn pack_two_nums(first_num: u16, second_num: u16) -> u32 {
    ((first_num as u32) << 16) | second_num as u32
}

#[inline]
pub fn unpack_start_include(raw: u32) -> (u16, u16) {
    let const_idx   =  raw        as u16;        // 下位 16bit
    let locals_size = (raw >> 16) as u16;        // 上位 16bit
    (const_idx, locals_size)
}

#[derive(Debug, Default, Clone)]
pub struct Chunk {
    pub arity: u8,
    pub code: Vec<Order>,
    pub constants: Vec<Constant>,
    pub constant_metas: Vec<ValueMeta>,
}

#[derive(Debug, Clone, Copy)]
pub struct ValueMeta {
    pub is_public: bool,
    pub is_mutable: bool,
    pub value_type: Type,
}

impl Chunk {
    pub fn add_constant_number(&mut self, c: f64, m: ValueMeta) -> usize {
        if let Some(id) = self.find_number(c) {
            id
        } else {
            self.add_constant(Constant::Number(c), m)
        }
    }

    pub fn add_constant_string(&mut self, s: String, m: ValueMeta) -> usize {
        if let Some(id) = self.find_string(&s) {
            id
        } else {
            self.add_constant(Constant::String(s), m)
        }
    }

    pub fn add_constant(&mut self, val: Constant, meta: ValueMeta) -> usize {
        let const_idx = self.constants.len();
        self.constants.push(val);
        self.constant_metas.push(meta);
        const_idx
    }

    pub fn find_string(&self, s: &str) -> Option<usize> {
        self.constants.iter().position(|c| {
            if let Constant::String(s2) = c {
                s == s2
            } else {
                false
            }
        })
    }

    fn find_number(&self, num: f64) -> Option<usize> {
        self.constants.iter().position(|c| {
            if let Constant::Number(num2) = c {
                (num - num2).abs() < f64::EPSILON
            } else {
                false
            }
        })
    }
}
