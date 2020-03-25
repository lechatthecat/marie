use std::fmt;
use std::ops::{Add, Sub, Div, Mul, Rem};
use super::oran_string::OranString;

#[derive(Clone, Debug)]
pub struct OranVariable<'a> {
    pub var_type: i32,
    pub name: &'a str,
    pub value: OranVariableValue<'a>
}

impl PartialEq for OranVariable<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

#[derive(Clone, Debug)]
pub enum OranVariableValue<'a> {
    Float(f64),
    Str(OranString<'a>),
    Boolean(bool),
    Null
}

impl fmt::Display for OranVariableValue<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            OranVariableValue::Float(ref fl) => write!(f, "{}", fl),
            OranVariableValue::Str(ref s) => {
                if s.is_ref {
                    write!(f, "{}", s.ref_str.unwrap())
                } else {
                    write!(f, "{}", s.val_str.as_ref().unwrap())
                }
            },
            OranVariableValue::Boolean(ref b) => write!(f, "{}", b),
            OranVariableValue::Null => write!(f, ""),
        }
    }
}

impl PartialEq for OranVariableValue<'_> {
    fn eq(&self, other: &OranVariableValue) -> bool {
        match *self {
            OranVariableValue::Float(ref fl) => *fl == f64::from(other),
            OranVariableValue::Str(ref s) => {
                if s.is_ref {
                    s.ref_str.unwrap().to_string() == other.to_string()
                } else {
                    s.val_str.as_ref().unwrap().to_string() == other.to_string()
                }
            }
            OranVariableValue::Boolean(ref b) => bool::from(*b) == bool::from(other),
            OranVariableValue::Null => false
        }
    }
}

impl From<OranVariableValue<'_>> for bool {
    fn from(val: OranVariableValue) -> Self {
        match val {
            OranVariableValue::Str(ref s) => {
                if s.is_ref {
                    if s.ref_str.unwrap().to_string() == "true" {
                        return true;
                    } else if s.ref_str.unwrap().to_string() == "" {
                        return false;
                    }
                    true
                } else {
                    if s.val_str.as_ref().unwrap().to_string() == "true" {
                        return true;
                    } else if s.val_str.as_ref().unwrap().to_string() == "" {
                        return false;
                    }
                    true
                }
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
    }
}

impl From<&OranVariableValue<'_>> for bool {
    fn from(val: &OranVariableValue) -> Self {
        match val {
            OranVariableValue::Str(ref s) => {
                if s.is_ref {
                    if s.ref_str.unwrap().to_string() == "true" {
                        return true;
                    } else if s.ref_str.unwrap().to_string() == "" {
                        return false;
                    }
                    true
                } else {
                    if s.val_str.as_ref().unwrap().to_string() == "true" {
                        return true;
                    } else if s.val_str.as_ref().unwrap().to_string() == "" {
                        return false;
                    }
                    true
                }
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
    }
}

impl Sub for OranVariableValue<'_> {
    type Output = Self;

    fn sub(self, other: Self) -> Self::Output {
        match self {
            OranVariableValue::Float(ref fl) => { OranVariableValue::Float(fl - f64::from(other)) },
            _ => panic!("Variable types are not Number: {:?}", self)
        }
    }
}

impl Add for OranVariableValue<'_> {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        match self {
            OranVariableValue::Float(ref fl) => { OranVariableValue::Float(fl + f64::from(other)) },
            _ => panic!("Variable types are not Number: {:?}", self)
        }
    }
}

impl Div for OranVariableValue<'_> {
    type Output = Self;

    fn div(self, other: Self) -> Self::Output {
        match self {
            OranVariableValue::Float(ref fl) => { OranVariableValue::Float(fl / f64::from(other)) },
            _ => panic!("Variable types are not Number: {:?}", self)
        }
    }
}

impl Mul for OranVariableValue<'_> {
    type Output = Self;

    fn mul(self, other: Self) -> Self::Output {
        match self {
            OranVariableValue::Float(ref fl) => { OranVariableValue::Float(fl * f64::from(other)) },
            _ => panic!("Variable types are not Number: {:?}", self)
        }
    }
}

impl Rem for OranVariableValue<'_> {
    type Output = Self;

    fn rem(self, other: Self) -> Self::Output {
        match self {
            OranVariableValue::Float(ref fl) => { OranVariableValue::Float(fl % f64::from(other)) },
            _ => panic!("Variable types are not Number: {:?}", self)
        }
    }
}

impl From<OranVariableValue<'_>> for f64 {
    fn from(val: OranVariableValue) -> Self {
        match val {
            OranVariableValue::Float(ref fl) => { *fl },
            OranVariableValue::Str(ref s) => {
                if s.is_ref {
                    s.ref_str.unwrap().to_owned().parse().unwrap_or_else(|e| panic!("{}", e))
                } else {
                    s.val_str.as_ref().unwrap().parse().unwrap_or_else(|e| panic!("{}", e))
                }
            },
            OranVariableValue::Null => { f64::from(0) }
            _ => panic!("Variable types are not Number: {:?}", val)
        }
    }
}

impl From<OranVariableValue<'_>> for String {
    fn from(val: OranVariableValue) -> Self {
        match val {
            OranVariableValue::Str(ref s) => {
                if s.is_ref {
                    s.ref_str.unwrap().to_string()
                } else {
                    s.val_str.as_ref().unwrap().to_string()
                }
            },
            OranVariableValue::Float(ref fl) => { fl.to_string() },
            OranVariableValue::Boolean(ref bl) => { bl.to_string() },
            OranVariableValue::Null => { "".to_string() }
        }
    }
}