use crate::bytecode::bytecode;
use crate::bytecode::bytecode_interpreter;
use crate::gc::gc;

use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::hash::Hasher;
use std::rc::Rc;
use itertools::Itertools;

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

#[derive(Clone, Debug)]
pub struct NativeFunction {
    pub arity: u8,
    pub name: String,
    pub func: fn(&mut bytecode_interpreter::Interpreter, &[Value]) -> Result<Value, String>,
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

pub trait TraitMarieValuePropertyFind {
    fn find_property_by_name(&self, name: &String) -> Option<MarieValue>;
    fn find_methodid_by_name(&self, name: &str) -> Option<(gc::HeapId, usize)>;
    fn find_classid(&self, method_id: usize) -> Option<usize>;
}

impl TraitMarieValuePropertyFind for HashMap<PropertyKey, MarieValue> {
    fn find_property_by_name(&self, name: &String) -> Option<MarieValue> {
        for item in self.keys().sorted().into_iter() {
            if &item.name == name {
                return Some(self[item].clone());
            }
        }
        None
    }
    fn find_methodid_by_name(&self, name: &str) -> Option<(gc::HeapId, usize)> {
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
pub struct MarieValue {
    pub val: Value,
    pub is_mutable: bool,
    pub is_public: bool,
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

#[derive(Clone, Debug)]
pub enum Value {
    Number(f64),
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

impl Value {
    pub fn get_type (&self) -> Type {
        type_of(self)
    }
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
            Value::NativeFunction(_) => write!(fmt, "Function"),
            Value::Null => write!(fmt, "null"),
            Value::List(v) => write!(fmt, "{}", v),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Type {
    Number,
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

pub fn type_of(value: &Value) -> Type {
    match value {
        Value::Number(_) => Type::Number,
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

impl std::fmt::Display for Type {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Type::Number => write!(fmt, "Number"),
            Type::Bool => write!(fmt, "Bool"),
            Type::String => write!(fmt, "String"),
            Type::Function => write!(fmt, "Function"),
            Type::Instance => write!(fmt, "Instance"),
            Type::BoundMethod => write!(fmt, "BoundMethod"),
            Type::Class => write!(fmt, "Class"),
            Type::NativeFunction => write!(fmt, "Class"),
            Type::Null => write!(fmt, "Class"),
            Type::List => write!(fmt, "List"),
        }
    }
}
