use crate::bytecode::bytecode;
use crate::bytecode::bytecode_interpreter;
use crate::gc::gc;

use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt::Display;
use std::hash::Hasher;
use std::ops::Add;
use std::ops::Div;
use std::ops::Mul;
use std::ops::Sub;
use std::rc::Rc;
use std::str::FromStr;
use itertools::Itertools;

#[derive(Clone)]
pub struct MarieValue {
    pub val: Value,
    pub is_mutable: bool,
    pub is_public: bool,
}

#[derive(Clone)]
pub enum Upvalue {
    Open(usize),
    Closed(Value),
}

impl Upvalue {
    pub fn is_open(&self) -> bool {
        match self {
            Upvalue::Open(_) => true,
            Upvalue::Closed(_) => false,
        }
    }

    pub fn is_open_with_index(&self, index: usize) -> bool {
        match self {
            Upvalue::Open(idx) => index == *idx,
            Upvalue::Closed(_) => false,
        }
    }
}

#[derive(Default, Clone)]
pub struct Closure {
    pub function: bytecode::Function,
    pub upvalues: Vec<Rc<RefCell<Upvalue>>>,
}

#[derive(Clone)]
pub struct NativeFunction {
    pub arity: u8,
    pub name: String,
    pub func: fn(&mut bytecode_interpreter::Interpreter, &[MarieValue]) -> Result<MarieValue, String>,
}

#[derive(Clone)]
pub struct PropertyKey {
    pub name: String,
    pub id: usize
}

impl PartialEq for PropertyKey {
    fn eq(&self, other: &PropertyKey) -> bool {
        self.name == other.name && self.id == other.id
    }
}

impl Eq for PropertyKey {}

impl std::hash::Hash for PropertyKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.name.hash(state);
    }
}

impl Ord for PropertyKey {
    fn cmp(&self, other: &Self) -> Ordering {
        other.id.cmp(&self.id)
    }
}

impl PartialOrd for PropertyKey {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

pub trait TraitPropertyFind {
    fn find_property_ignore_id(&self, name: &String, ignore_id: usize) -> Option<MarieValue>;
    fn find_property(&self, name: &String) -> Option<MarieValue>;
    fn find_methodid(&self, name: &str) -> Option<(gc::HeapId, usize)>;
    fn find_methodid_with_ignore_id(&self, name: &str, ignore_class_id: usize) -> Option<(gc::HeapId, usize)>;
    fn find_classid(&self, method_id: usize) -> Option<usize>;
}

impl TraitPropertyFind for HashMap<PropertyKey, MarieValue> {
    fn find_property_ignore_id(&self, name: &String, ignore_id: usize) -> Option<MarieValue> {
        for item in self.keys().sorted().into_iter() {
            if &item.id != &ignore_id && &item.name == name {
                return Some(self[item].clone());
            }
        }
        None
    }
    fn find_property(&self, name: &String) -> Option<MarieValue> {
        for item in self.keys().sorted().into_iter() {
            if &item.name == name {
                return Some(self[item].clone());
            }
        }
        None
    }
    fn find_methodid(&self, name: &str) -> Option<(gc::HeapId, usize)> {
        for item in self.keys().sorted().into_iter() {
            // item.name is method name.
            if &item.name == name {
                if let Value::Function (methodid) = self[item].val {
                    return Some((item.id, methodid));
                }
            }
        }
        None
    }
    fn find_methodid_with_ignore_id(&self, name: &str, ignore_class_id: usize) -> Option<(gc::HeapId, usize)> {
        for item in self.keys().sorted().into_iter() {
            // item.name is method name.
            if &item.id != &ignore_class_id && &item.name == name {
                if let Value::Function (methodid) = self[item].val {
                    return Some((item.id, methodid));
                }
            }
        }
        None
    }
    fn find_classid(&self, methodid_tofind: usize) -> Option<usize> {
        for item in self.keys().sorted().into_iter() {
            if let Value::Function (methodid) = self[item].val {
                if methodid == methodid_tofind {
                    return Some(item.id);
                }
            }
        }
        None
    }
}

#[derive(Clone)]
pub struct Class {
    pub name: String,
    pub properties: HashMap<PropertyKey, MarieValue>,
}

#[derive(Clone)]
pub struct Instance {
    pub class_id: gc::HeapId,
    pub fields: HashMap<PropertyKey, MarieValue>,
}

#[derive(Clone)]
pub struct BoundMethod {
    pub instance_id: gc::HeapId,
    pub closure_id: gc::HeapId,
}

#[derive(Clone)]
pub enum Value {
    Number(NumberVal),
    Bool(bool),
    String(gc::HeapId),
    Function(gc::HeapId),
    Instance(gc::HeapId),
    BoundMethod(gc::HeapId),
    Class(gc::HeapId),
    NativeFunction(NativeFunction),
    Null,
    List(gc::HeapId),
}

#[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
pub enum NumberVal {
    Int(i64),
    Float(f64),
}

impl Add for NumberVal {
    type Output = NumberVal;

    fn add(self, rhs: NumberVal) -> NumberVal {
        match (self, rhs) {
            (NumberVal::Int(a),   NumberVal::Int(b))   => NumberVal::Int(a + b),
            (NumberVal::Int(a),   NumberVal::Float(b)) => NumberVal::Float(a as f64 + b),
            (NumberVal::Float(a), NumberVal::Int(b))   => NumberVal::Float(a + b as f64),
            (NumberVal::Float(a), NumberVal::Float(b)) => NumberVal::Float(a + b),
        }
    }
}

impl Sub for NumberVal {
    type Output = NumberVal;

    fn sub(self, rhs: NumberVal) -> NumberVal {
        match (self, rhs) {
            (NumberVal::Int(a),   NumberVal::Int(b))   => NumberVal::Int(a - b),
            (NumberVal::Int(a),   NumberVal::Float(b)) => NumberVal::Float(a as f64 - b),
            (NumberVal::Float(a), NumberVal::Int(b))   => NumberVal::Float(a - b as f64),
            (NumberVal::Float(a), NumberVal::Float(b)) => NumberVal::Float(a - b),
        }
    }
}

impl Mul for NumberVal {
    type Output = NumberVal;

    fn mul(self, rhs: NumberVal) -> NumberVal {
        match (self, rhs) {
            (NumberVal::Int(a),   NumberVal::Int(b))   => NumberVal::Int(a * b),
            (NumberVal::Int(a),   NumberVal::Float(b)) => NumberVal::Float(a as f64 * b),
            (NumberVal::Float(a), NumberVal::Int(b))   => NumberVal::Float(a * b as f64),
            (NumberVal::Float(a), NumberVal::Float(b)) => NumberVal::Float(a * b),
        }
    }
}

impl Div for NumberVal {
    type Output = NumberVal;

    fn div(self, rhs: NumberVal) -> NumberVal {
        match (self, rhs) {
            // 整数÷整数＝整数（端数切捨て）にしているが、好みによって Float にする方が安全
            (NumberVal::Int(a),   NumberVal::Int(b))   => NumberVal::Int(a / b),
            (NumberVal::Int(a),   NumberVal::Float(b)) => NumberVal::Float(a as f64 / b),
            (NumberVal::Float(a), NumberVal::Int(b))   => NumberVal::Float(a / b as f64),
            (NumberVal::Float(a), NumberVal::Float(b)) => NumberVal::Float(a / b),
        }
    }
}

impl NumberVal {
    pub fn is_float(&self) -> bool {
        match self {
            NumberVal::Float(_) => true,
            NumberVal::Int(_) => false,
        }
    }
    
    pub fn is_int(&self) -> bool {
        match self {
            NumberVal::Float(_) => false,
            NumberVal::Int(_) => true,
        }
    }

    pub fn nearly_eq(self, other: NumberVal) -> bool {
        // どちらも f64 へ
        let (a, b) = (self.to_f64(), other.to_f64());
        // 好みのトレランスを決める
        const REL: f64 = 1e-12;
        const ABS: f64 = 1e-12;
        (a - b).abs() <= (ABS + REL * a.abs().max(b.abs()))
    }

    /// f64 に丸めて取得（比較などで便利）
    #[inline]
    pub fn to_f64(self) -> f64 {
        match self {
            NumberVal::Int(i)   => i as f64,
            NumberVal::Float(f) => f,
        }
    }

    /// 絶対値を NumberVal として返す
    #[inline]
    pub fn abs(self) -> NumberVal {
        match self {
            NumberVal::Int(i)   => NumberVal::Int(i.abs()),   // i64::abs はオーバーフローに注意
            NumberVal::Float(f) => NumberVal::Float(f.abs()),
        }
    }

    /// 絶対値を f64 で返す（EPSILON 比較用に便利）
    #[inline]
    pub fn abs_as_f64(self) -> f64 {
        self.to_f64().abs()
    }
    
}

impl FromStr for NumberVal {
    type Err = String; // You can also define a custom error type

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Ok(i) = s.parse::<i64>() {
            Ok(NumberVal::Int(i))
        } else if let Ok(f) = s.parse::<f64>() {
            Ok(NumberVal::Float(f))
        } else {
            Err("String is not a valid NumberVal".to_string())
        }
    }
}

impl std::fmt::Display for NumberVal {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            NumberVal::Int(v) => write!(fmt, "{}", v),
            NumberVal::Float(v) => write!(fmt, "{}", v),
        }
    }
}

#[derive(Clone)]
pub struct JitParameter {
    pub value: i64,
    pub value_type: i64,
}


impl std::fmt::Display for Value {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Value::Number(v) => write!(fmt, "{}", v),
            Value::Bool(v) => write!(fmt, "{}", v),
            Value::String(v) => write!(fmt, "{}", v),
            Value::Function(v) => write!(fmt, "{}", v),
            Value::Instance(v) => write!(fmt, "{}", v),
            Value::BoundMethod(v) => write!(fmt, "{}", v),
            Value::Class(v) => write!(fmt, "{}", v),
            Value::NativeFunction(f) => write!(fmt, "<native function: {}>", f.name),
            Value::Null => write!(fmt, "null"),
            Value::List(v) => write!(fmt, "{}", v),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Type {
    Int,
    Float,
    Bool,
    String,
    Function,
    NativeFunction,
    Class,
    BoundMethod,
    Instance,
    Null,
    List,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "Int"),
            Type::Float => write!(f, "Float"),
            Type::Bool => write!(f, "Bool"),
            Type::String => write!(f, "String"),
            Type::Function => write!(f, "Function"),
            Type::NativeFunction => write!(f, "NativeFunction"),
            Type::BoundMethod => write!(f, "BoundMethod"),
            Type::Class => write!(f, "Class"),
            Type::Instance => write!(f, "Instance"),
            Type::Null => write!(f, "Nil"),
            Type::List => write!(f, "List"),
        }
    }
}

pub fn type_of(value: &Value) -> Type {
    match value {
        Value::Number(n) => {
            if n.is_int() {
                Type::Int
            } else {
                Type::Float
            }
        },
        Value::Bool(_) => Type::Bool,
        Value::String(_) => Type::String,
        Value::Function(_) => Type::Function,
        Value::NativeFunction(_) => Type::NativeFunction,
        Value::BoundMethod(_) => Type::BoundMethod,
        Value::Class(_) => Type::Class,
        Value::Instance(_) => Type::Instance,
        Value::Null => Type::Null,
        Value::List(_) => Type::List,
    }
}

pub fn type_id_of(value: &Value) -> usize {
    match value {
        Value::Number(n) => {
            if n.is_int() {
                1
            } else {
                2
            }
        },
        Value::Bool(_) => 3,
        Value::String(_) => 4,
        Value::Function(_) => 5,
        Value::NativeFunction(_) => 6,
        Value::BoundMethod(_) => 7,
        Value::Class(_) => 8,
        Value::Instance(_) => 9,
        Value::Null => 10,
        Value::List(_) => 11,
    }
}

pub fn from_type_to_type_id(value: &Type) -> usize {
    match value {
        Type::Int => 1,
        Type::Float => 2,
        Type::Bool => 3,
        Type::String => 4,
        Type::Function => 5,
        Type::Instance => 9,
        Type::Null => 10,
        Type::List => 11,
        _ => panic!("unknown type")
    }
}

pub fn from_string_type_id_of(value: &String) -> usize {
    match value.as_str() {
        "int" => 1,
        "float" => 2,
        "bool" => 3,
        "string" => 4,
        "callable" => 5,
        //"native_callable" => 6,
        //"bound_method" => 7,
        //"class" => 8,
        "instance" => 9,
        "void" => 10,
        "list" => 11,
        _ => 0
    }
}

pub fn type_id_to_string(type_id: usize) -> String {
    match type_id {
        1 => "int".to_string(),
        2 => "float".to_string(),
        3 => "bool".to_string(),
        4 => "string".to_string(),
        5 => "callable".to_string(),
        9 => "instance".to_string(),
        10 => "void".to_string(),
        11 => "list".to_string(),
        _ => panic!("unknown type_id")
    }
}
