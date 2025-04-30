use crate::bytecode::bytecode;
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

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Copy, Clone)]
pub enum UpvalueLoc {
    Upvalue(/*upvalue idx*/ usize),
    Local(/*stack idx*/ usize),
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(untagged)]
pub enum Op {
    Return,
    Constant(usize),
    Closure(bool, usize, Vec<UpvalueLoc>),
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
    EndScope,
    DefineGlobal(bool, usize),
    DefineLocal(bool, usize),
    GetGlobal(usize),
    SetGlobal(usize),
    GetLocal(usize),
    SetLocal(usize),
    GetUpval(usize),
    SetUpval(usize),
    JumpIfFalse(usize),
    Jump(usize),
    Loop(usize),
    Call(u8),
    CreateInstance(u8),
    CloseUpvalue,
    Class(usize),
    DefineProperty(bool, bool, usize),
    SetProperty(usize),
    GetProperty(usize),
    Method(bool, usize),
    Invoke(/*method_name*/ String, /*arg count*/ u8),
    Inherit,
    GetSuper(usize),
    SuperInvoke(/*method_name*/ String, /*arg count*/ u8),
    BuildList(usize),
    Subscr,
    SetItem,
    StartInclude(usize, u8),
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
    pub upvalues: Vec<UpvalueLoc>,
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
                upvalues: _,
            }) => write!(f, "<fn {}>", name),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Order {
    pub operation: bytecode::Op,
    pub lineno: bytecode::Lineno,
    pub caller_func_ip: Option<usize>,
}

#[derive(Debug, Default, Clone)]
pub struct Chunk {
    pub code: Vec<Order>,
    pub constants: Vec<Constant>,
    pub constant_metas: Vec<ValueMeta>,
}

#[derive(Debug, Default, Clone, Copy)]
pub struct ValueMeta {
    pub is_public: bool,
    pub is_mutable: bool,
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
