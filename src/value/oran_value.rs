use std::fmt;
use std::cmp::{PartialOrd, Ord, Ordering};
use std::ops::{Add, Sub, Div, Mul, Rem};
use ordered_float::OrderedFloat;
use super::oran_variable::{OranVariable, OranVariableValue};
use super::oran_string::OranStringRef;
use crate::parser::astnode::AstNode;

#[derive(Debug)]
pub enum OranValue<'a> {
    Float(f64),
    Str(OranStringRef<'a>),
    Boolean(bool),
    Variable(OranVariable<'a>),
    Function(FunctionDefine<'a>),
    Null
}

impl Clone for OranValue<'_> {
    fn clone(&self) -> Self {
        match self {
            OranValue::Float(a) => OranValue::Float(*a),
            OranValue::Str(a) => OranValue::Str(a.clone()),
            OranValue::Boolean(a) => OranValue::Boolean(*a),
            OranValue::Variable(a) => OranValue::Variable(a.clone()),
            OranValue::Function(a) => OranValue::Function(*a),
            OranValue::Null => OranValue::Null
        }
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub struct FunctionDefine<'a> {
    pub name: &'a str,
    pub args: &'a Vec<AstNode>,
    pub fn_return: &'a Box<AstNode>,
    pub body: &'a Vec<AstNode>,
}

impl fmt::Display for OranValue<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            OranValue::Float(ref fl) => write!(f, "{}", fl),
            OranValue::Str(ref s) => write!(f, "{}", s.val_str.as_ref()),
            OranValue::Boolean(ref b) => write!(f, "{}", b),
            OranValue::Variable(ref v) => write!(f, "{}", v.value),
            OranValue::Null => write!(f, ""),
            _ => write!(f, "")
        }
    }
}

impl PartialEq for OranValue<'_> {
    fn eq(&self, other: &OranValue) -> bool {
        match self {
            OranValue::Float(ref fl) => fl == &f64::from(other),
            OranValue::Str(ref s) =>  s.val_str.as_ref().to_string() == other.to_string(),
            OranValue::Boolean(ref b) => bool::from(*b) == bool::from(other),
            OranValue::Variable(ref v) => v.value == OranVariableValue::from(other),
            OranValue::Null => false,
            _ => false
        }
    }
}

impl Eq for OranValue<'_> {}

impl PartialOrd for OranValue<'_> {
    fn partial_cmp(&self, other: &OranValue) -> Option<Ordering> {
        match self {
            OranValue::Float(fl) => fl.partial_cmp(&f64::from(other)),
            OranValue::Str(_s) => {
                f64::from(self).partial_cmp(&f64::from(other))
            }
            OranValue::Variable(v) => {
                match &v.value {
                    OranVariableValue::Float(fl) => fl.partial_cmp(&f64::from(other)),
                    OranVariableValue::Str(_st) => {
                        f64::from(&v.value).partial_cmp(&f64::from(other))
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }
}

impl Ord for OranValue<'_> {
    fn cmp(&self, other: &OranValue) -> Ordering {
        match self {
            OranValue::Float(fl) => OrderedFloat(*fl).cmp(&OrderedFloat(f64::from(other))),
            OranValue::Str(_s) => {
                OrderedFloat(f64::from(self)).cmp(&OrderedFloat(f64::from(other)))
            }
            OranValue::Variable(v) => {
                match &v.value {
                    OranVariableValue::Float(fl) => OrderedFloat(*fl).cmp(&OrderedFloat(f64::from(other))),
                    OranVariableValue::Str(_st) => {
                        OrderedFloat(f64::from(&v.value)).cmp(&OrderedFloat(f64::from(other)))
                    }
                    _ => panic!("Variable type can not be conberted Number: {:?}", self)
                }
            }
            _ => panic!("Variable type can not be conberted Number: {:?}", self)
        }
    }
}

impl Sub for OranValue<'_> {
    type Output = Self;

    fn sub(self, other: Self) -> Self::Output {
        match self {
            OranValue::Float(ref fl) => { OranValue::Float(fl - f64::from(other)) },
            OranValue::Str(ref s) => OranValue::Float(s.val_str.as_ref().parse::<f64>().unwrap_or_else(|e| panic!("{}", e)) - f64::from(other)),
            OranValue::Variable(ref v) => { 
                match v.value {
                    OranVariableValue::Float(ref vfl) => { OranValue::Float(vfl - f64::from(other)) },
                    OranVariableValue::Str(ref s) => OranValue::Float(s.val_str.as_ref().parse::<f64>().unwrap_or_else(|e| panic!("{}", e)) - f64::from(other)),
                    _ => panic!("Variable types are not Number: {:?}", self)
                }
            },
            _ => panic!("Variable types are not Number: {:?}", self)
        }
    }
}

impl Add for OranValue<'_> {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        match self {
            OranValue::Float(ref fl) => { OranValue::Float(fl + f64::from(other)) },
            OranValue::Str(ref s) => OranValue::Float(s.val_str.as_ref().parse::<f64>().unwrap_or_else(|e| panic!("{}", e)) + f64::from(other)),
            OranValue::Variable(ref v) => { 
                match v.value {
                    OranVariableValue::Float(ref vfl) => { OranValue::Float(vfl + f64::from(other)) },
                    OranVariableValue::Str(ref s) => OranValue::Float(s.val_str.as_ref().parse::<f64>().unwrap_or_else(|e| panic!("{}", e)) + f64::from(other)),
                    _ => panic!("Variable types are not Number: {:?}", self)
                }
            },
            _ => panic!("Variable types are not Number: {:?}", self)
        }
    }
}

impl Div for OranValue<'_> {
    type Output = Self;

    fn div(self, other: Self) -> Self::Output {
        match self {
            OranValue::Float(ref fl) => { OranValue::Float(fl / f64::from(other)) },
            OranValue::Str(ref s) => OranValue::Float(s.val_str.as_ref().parse::<f64>().unwrap_or_else(|e| panic!("{}", e)) / f64::from(other)),
            OranValue::Variable(ref v) => { 
                match v.value {
                    OranVariableValue::Float(ref vfl) => { OranValue::Float(vfl / f64::from(other)) },
                    OranVariableValue::Str(ref s) => OranValue::Float(s.val_str.as_ref().parse::<f64>().unwrap_or_else(|e| panic!("{}", e)) / f64::from(other)),
                    _ => panic!("Variable types are not Number: {:?}", self)
                }
            },
            _ => panic!("Variable types are not Number: {:?}", self)
        }
    }
}

impl Mul for OranValue<'_> {
    type Output = Self;

    fn mul(self, other: Self) -> Self::Output {
        match self {
            OranValue::Float(ref fl) => { OranValue::Float(fl * f64::from(other)) },
            OranValue::Str(ref s) => OranValue::Float(s.val_str.as_ref().parse::<f64>().unwrap_or_else(|e| panic!("{}", e)) * f64::from(other)),
            OranValue::Variable(ref v) => { 
                match v.value {
                    OranVariableValue::Float(ref vfl) => { OranValue::Float(vfl * f64::from(other)) },
                    OranVariableValue::Str(ref s) => OranValue::Float(s.val_str.as_ref().parse::<f64>().unwrap_or_else(|e| panic!("{}", e)) * f64::from(other)),
                    _ => panic!("Variable types are not Number: {:?}", self)
                }
            },
            _ => panic!("Variable types are not Number: {:?}", self)
        }
    }
}

impl Rem for OranValue<'_> {
    type Output = Self;

    fn rem(self, other: Self) -> Self::Output {
        match self {
            OranValue::Float(ref fl) => { OranValue::Float(fl % f64::from(other)) },
            OranValue::Str(ref s) => OranValue::Float(s.val_str.as_ref().parse::<f64>().unwrap_or_else(|e| panic!("{}", e)) % f64::from(other)),
            OranValue::Variable(ref v) => { 
                match v.value {
                    OranVariableValue::Float(ref vfl) => { OranValue::Float(vfl % f64::from(other)) },
                    OranVariableValue::Str(ref s) => OranValue::Float(s.val_str.as_ref().parse::<f64>().unwrap_or_else(|e| panic!("{}", e)) % f64::from(other)),
                    _ => panic!("Variable types are not Number: {:?}", self)
                }
            },
            _ => panic!("Variable types are not Number: {:?}", self)
        }
    }
}

impl From<OranValue<'_>> for f64 {
    fn from(val: OranValue) -> Self {
        match val {
            OranValue::Float(ref fl) => { *fl },
            OranValue::Str(ref s) => s.val_str.as_ref().parse().unwrap_or_else(|e| panic!("{}", e)),
            OranValue::Variable(ref v) => {
                match v.value {
                    OranVariableValue::Float(ref fl) => { *fl },
                    OranVariableValue::Str(ref s) => s.val_str.as_ref().parse().unwrap_or_else(|e| panic!("{}", e)),
                    OranVariableValue::Null => { f64::from(0) }
                    _ => panic!("Variable types are not Number: {:?}", val)
                }
            },
            _ => panic!("Variable types are not Number: {:?}", val)
        }
    }
}

impl From<&OranValue<'_>> for Result<f64, String> {
    fn from(val: &OranValue) -> Self {
        match val {
            OranValue::Float(ref fl) => { Ok(*fl) },
            OranValue::Str(ref s) => {
                match s.val_str.as_ref().parse() {
                    Ok(v) => Ok(v),
                    Err(_e) => {
                        let mut err = "Variable type is not Number:".to_owned();
                        err.push_str(&val.to_string());
                        Err(err)
                    }
                }
            },
            OranValue::Variable(ref v) => {
                match v.value {
                    OranVariableValue::Float(ref fl) => { Ok(*fl) },
                    OranVariableValue::Str(ref s) => match s.val_str.as_ref().parse() {
                        Ok(v) => Ok(v),
                        Err(_e) => {
                            let mut err = "Variable type is not Number:".to_owned();
                            err.push_str(&val.to_string());
                            Err(err)
                        }
                    },
                    OranVariableValue::Null => { Ok(f64::from(0)) }
                    _ => panic!("Variable type is not Number: {:?}", val)
                }
            },
            _ => panic!("Variable types are not Number: {:?}", val)
        }
    }
}

impl From<&OranValue<'_>> for f64 {
    fn from(val: &OranValue) -> Self {
        match val {
            OranValue::Float(ref fl) => { *fl },
            OranValue::Str(ref s) => s.val_str.as_ref().parse().unwrap_or_else(|e| panic!("{}", e)),
            OranValue::Variable(ref v) => {
                match v.value {
                    OranVariableValue::Float(ref fl) => { *fl },
                    OranVariableValue::Str(ref s) => s.val_str.as_ref().parse().unwrap_or_else(|e| panic!("{}", e)),
                    OranVariableValue::Null => { f64::from(0) }
                    _ => panic!("Variable types are not Number: {:?}", val)
                }
            },
            _ => panic!("Variable types are not Number: {:?}", val)
        }
    }
}

impl From<&OranVariableValue<'_>> for f64 {
    fn from(val: &OranVariableValue) -> Self {
        match val {
            OranVariableValue::Float(ref fl) => { *fl },
            OranVariableValue::Str(ref s) => s.val_str.as_ref().parse().unwrap_or_else(|e| panic!("{}", e)),
            OranVariableValue::Null => { f64::from(0) }
            _ => panic!("Variable types are not Number: {:?}", val)
        }
    }
}

impl From<OranValue<'_>> for String {
    fn from(val: OranValue) -> Self {
        match val {
            OranValue::Str(ref s) => s.val_str.as_ref().to_string(),
            OranValue::Float(ref fl) => { fl.to_string() },
            OranValue::Boolean(ref bl) => { bl.to_string() },
            OranValue::Variable(ref v) => { v.value.to_string() },
            OranValue::Null => { "".to_string() },
            _ => { "".to_string() }
        }
    }
}

impl From<&OranValue<'_>> for String {
    fn from(val: &OranValue) -> Self {
        match val {
            OranValue::Str(ref s) => s.val_str.as_ref().to_string(),
            OranValue::Float(ref fl) => { fl.to_string() },
            OranValue::Boolean(ref bl) => { bl.to_string() },
            OranValue::Variable(ref v) => { v.value.to_string() },
            OranValue::Null => { "".to_string() },
            _ => { "".to_string() }
        }
    }
}

impl From<OranValue<'_>> for bool {
    fn from(val: OranValue) -> Self {
        match val {
            OranValue::Str(ref s) => {
                if s.val_str.as_ref().to_string() == "true" {
                    return true;
                } else if s.val_str.as_ref().to_string() == "" {
                    return false;
                }
                true
            },
            OranValue::Float(ref fl) => {
                if *fl == f64::from(0) {
                    return false;
                }
                true
            },
            OranValue::Boolean(ref bl) => { *bl },
            OranValue::Variable(ref v) => {
                match v.value {
                    OranVariableValue::Str(ref s) => {
                        if s.val_str.as_ref().to_string() == "true" {
                            return true;
                        } else if s.val_str.as_ref().to_string() == "" {
                            return false;
                        }
                        true
                    },
                    OranVariableValue::Float(ref fl) => {
                        if *fl == f64::from(0) {
                            return false;
                        }
                        true
                    },
                    OranVariableValue::Boolean(ref bl) => { *bl },
                    OranVariableValue::Null => false,
                }
            },
            OranValue::Null => false,
            _ => false
        }
    }
}

impl From<&OranValue<'_>> for bool {
    fn from(val: &OranValue) -> Self {
        match val {
            OranValue::Str(ref s) => {
                if s.val_str.as_ref().to_string() == "true" {
                    return true;
                } else if s.val_str.as_ref().to_string() == "" {
                    return false;
                }
                true
            },
            OranValue::Float(ref fl) => {
                if *fl == f64::from(0) {
                    return false;
                }
                true
            },
            OranValue::Boolean(ref bl) => { *bl },
            OranValue::Variable(ref v) => {
                match v.value {
                    OranVariableValue::Str(ref s) => {
                        if s.val_str.as_ref().to_string() == "true" {
                            return true;
                        } else if s.val_str.as_ref().to_string() == "" {
                            return false;
                        }
                        true
                    },
                    OranVariableValue::Float(ref fl) => {
                        if *fl == f64::from(0) {
                            return false;
                        }
                        true
                    },
                    OranVariableValue::Boolean(ref bl) => { *bl },
                    OranVariableValue::Null => false
                }
            },
            OranValue::Null => false,
            _ => false
        }
    }
}

impl<'a> From<OranValue<'a>> for OranVariableValue<'a> {
    fn from(val: OranValue<'a>) -> Self {
        match val {
            OranValue::Str(ref s) => {
                OranVariableValue::Str(s.to_owned())
            },
            OranValue::Float(ref fl) => { OranVariableValue::Float(*fl) },
            OranValue::Boolean(ref bl) => { OranVariableValue::Boolean(*bl) },
            OranValue::Null => { OranVariableValue::Null },
            OranValue::Variable(ref v) => { 
                match v.value {
                    OranVariableValue::Str(ref s) => { OranVariableValue::Str(s.to_owned()) },
                    OranVariableValue::Float(ref fl) => { OranVariableValue::Float(*fl) },
                    OranVariableValue::Boolean(ref bl) => { OranVariableValue::Boolean(*bl) },
                    OranVariableValue::Null => { OranVariableValue::Null },
                }
            },
            _ => panic!("Failed to parse: {:?}", val)
        }
    }
}

impl<'a> From<&OranValue<'a>> for OranVariableValue<'a> {
    fn from(val: &OranValue<'a>) -> Self {
        match val {
            OranValue::Str(ref s) => {
                OranVariableValue::Str(s.to_owned())
            },
            OranValue::Float(ref fl) => { OranVariableValue::Float(*fl) },
            OranValue::Boolean(ref bl) => { OranVariableValue::Boolean(*bl) },
            OranValue::Null => { OranVariableValue::Null },
            OranValue::Variable(ref v) => { 
                match v.value {
                    OranVariableValue::Str(ref s) => { OranVariableValue::Str(s.to_owned()) },
                    OranVariableValue::Float(ref fl) => { OranVariableValue::Float(*fl) },
                    OranVariableValue::Boolean(ref bl) => { OranVariableValue::Boolean(*bl) },
                    OranVariableValue::Null => { OranVariableValue::Null },
                }
            },
            _ => panic!("Failed to parse: {:?}", val)
        }
    }
}

impl<'a> From<&OranValue<'a>> for OranVariable<'a> {
    fn from(val: &OranValue<'a>) -> Self {
        match &val {
            OranValue::Variable(ref v) => { 
                OranVariable {
                    var_type: v.var_type,
                    value: v.value.clone(),
                    name: v.name,
                }
            },
            _ => panic!("Failed to parse: {:?}", val)
        }
    }
}

impl<'a> From<&OranValue<'a>> for FunctionDefine<'a> {
    fn from(val: &OranValue<'a>) -> Self {
        match val {
            OranValue::Function(f) => {
                f.clone()
            },
            _ => panic!("Failed to parse: {:?}", val)
        }
    }
}
