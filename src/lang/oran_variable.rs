use std::fmt;
use std::ops::{Add, Sub, Div, Mul, Rem};

#[derive(Clone, Debug)]
pub struct OranVariable {
    pub is_const: bool,
    pub name: String,
    pub value: OranVariableValue
}

impl PartialEq for OranVariable {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum OranVariableValue {
    Float(f64),
    Str(String),
    Boolean(bool),
}

impl fmt::Display for OranVariableValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            OranVariableValue::Float(ref fl) => write!(f, "{}", fl),
            OranVariableValue::Str(ref s) => write!(f, "{}", s),
            OranVariableValue::Boolean(ref b) => write!(f, "{}", b),
        }
    }
}

impl Sub for OranVariableValue {
    type Output = Self;

    fn sub(self, other: Self) -> Self::Output {
        match self {
            OranVariableValue::Float(ref fl) => { OranVariableValue::Float(fl - f64::from(other)) },
            _ => panic!("Variable types are not Number: {:?}", self)
        }
    }
}

impl Add for OranVariableValue {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        match self {
            OranVariableValue::Float(ref fl) => { OranVariableValue::Float(fl + f64::from(other)) },
            _ => panic!("Variable types are not Number: {:?}", self)
        }
    }
}

impl Div for OranVariableValue {
    type Output = Self;

    fn div(self, other: Self) -> Self::Output {
        match self {
            OranVariableValue::Float(ref fl) => { OranVariableValue::Float(fl / f64::from(other)) },
            _ => panic!("Variable types are not Number: {:?}", self)
        }
    }
}

impl Mul for OranVariableValue {
    type Output = Self;

    fn mul(self, other: Self) -> Self::Output {
        match self {
            OranVariableValue::Float(ref fl) => { OranVariableValue::Float(fl * f64::from(other)) },
            _ => panic!("Variable types are not Number: {:?}", self)
        }
    }
}

impl Rem for OranVariableValue {
    type Output = Self;

    fn rem(self, other: Self) -> Self::Output {
        match self {
            OranVariableValue::Float(ref fl) => { OranVariableValue::Float(fl % f64::from(other)) },
            _ => panic!("Variable types are not Number: {:?}", self)
        }
    }
}

impl From<OranVariableValue> for f64 {
    fn from(val: OranVariableValue) -> Self {
        match val {
            OranVariableValue::Float(ref fl) => { *fl },
            OranVariableValue::Str(ref str) => { str.parse().unwrap() },
            _ => panic!("Variable types are not Number: {:?}", val)
        }
    }
}

impl From<OranVariableValue> for String {
    fn from(val: OranVariableValue) -> Self {
        match val {
            OranVariableValue::Str(ref st) => { st.to_string() },
            OranVariableValue::Float(ref fl) => { fl.to_string() },
            OranVariableValue::Boolean(ref bl) => { bl.to_string() },
        }
    }
}
