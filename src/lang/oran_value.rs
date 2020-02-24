use std::fmt;
use std::ops::{Add, Sub, Div, Mul, Rem};
use super::oran_variable::{OranVariable, OranVariableValue};

#[derive(Clone, PartialEq, Debug)]
pub enum OranValue {
    Float(f64),
    Str(String),
    Boolean(bool),
    Variable(OranVariable),
}

impl fmt::Display for OranValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            OranValue::Float(ref fl) => write!(f, "{}", fl),
            OranValue::Str(ref s) => write!(f, "{}", s),
            OranValue::Boolean(ref b) => write!(f, "{}", b),
            OranValue::Variable(ref v) => write!(f, "{}", v.value),
        }
    }
}

impl Sub for OranValue {
    type Output = Self;

    fn sub(self, other: Self) -> Self::Output {
        match self {
            OranValue::Float(ref fl) => { OranValue::Float(fl - f64::from(other)) },
            OranValue::Str(ref s) => { OranValue::Float(s.parse::<f64>().unwrap_or_else(|e| panic!("{}", e)) - f64::from(other)) },
            OranValue::Variable(ref v) => { 
                match v.value {
                    OranVariableValue::Float(ref vfl) => { OranValue::Float(vfl - f64::from(other)) },
                    OranVariableValue::Str(ref vs) => { OranValue::Float(vs.parse::<f64>().unwrap_or_else(|e| panic!("{}", e))- f64::from(other)) },
                    _ => panic!("Variable types are not Number: {:?}", self)
                }
            },
            _ => panic!("Variable types are not Number: {:?}", self)
        }
    }
}

impl Add for OranValue {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        match self {
            OranValue::Float(ref fl) => { OranValue::Float(fl + f64::from(other)) },
            OranValue::Str(ref s) => { OranValue::Float(s.parse::<f64>().unwrap_or_else(|e| panic!("{}", e)) + f64::from(other)) },
            OranValue::Variable(ref v) => { 
                match v.value {
                    OranVariableValue::Float(ref vfl) => { OranValue::Float(vfl + f64::from(other)) },
                    OranVariableValue::Str(ref vs) => { OranValue::Float(vs.parse::<f64>().unwrap_or_else(|e| panic!("{}", e)) + f64::from(other)) },
                    _ => panic!("Variable types are not Number: {:?}", self)
                }
            },
            _ => panic!("Variable types are not Number: {:?}", self)
        }
    }
}

impl Div for OranValue {
    type Output = Self;

    fn div(self, other: Self) -> Self::Output {
        match self {
            OranValue::Float(ref fl) => { OranValue::Float(fl / f64::from(other)) },
            OranValue::Str(ref s) => { OranValue::Float(s.parse::<f64>().unwrap_or_else(|e| panic!("{}", e)) / f64::from(other)) },
            OranValue::Variable(ref v) => { 
                match v.value {
                    OranVariableValue::Float(ref vfl) => { OranValue::Float(vfl / f64::from(other)) },
                    OranVariableValue::Str(ref vs) => { OranValue::Float(vs.parse::<f64>().unwrap_or_else(|e| panic!("{}", e)) / f64::from(other)) },
                    _ => panic!("Variable types are not Number: {:?}", self)
                }
            },
            _ => panic!("Variable types are not Number: {:?}", self)
        }
    }
}

impl Mul for OranValue {
    type Output = Self;

    fn mul(self, other: Self) -> Self::Output {
        match self {
            OranValue::Float(ref fl) => { OranValue::Float(fl * f64::from(other)) },
            OranValue::Str(ref s) => { OranValue::Float(s.parse::<f64>().unwrap_or_else(|e| panic!("{}", e)) * f64::from(other)) },
            OranValue::Variable(ref v) => { 
                match v.value {
                    OranVariableValue::Float(ref vfl) => { OranValue::Float(vfl * f64::from(other)) },
                    OranVariableValue::Str(ref vs) => { OranValue::Float(vs.parse::<f64>().unwrap_or_else(|e| panic!("{}", e)) * f64::from(other)) },
                    _ => panic!("Variable types are not Number: {:?}", self)
                }
            },
            _ => panic!("Variable types are not Number: {:?}", self)
        }
    }
}

impl Rem for OranValue {
    type Output = Self;

    fn rem(self, other: Self) -> Self::Output {
        match self {
            OranValue::Float(ref fl) => { OranValue::Float(fl % f64::from(other)) },
            OranValue::Str(ref s) => { OranValue::Float(s.parse::<f64>().unwrap_or_else(|e| panic!("{}", e)) % f64::from(other)) },
            OranValue::Variable(ref v) => { 
                match v.value {
                    OranVariableValue::Float(ref vfl) => { OranValue::Float(vfl % f64::from(other)) },
                    OranVariableValue::Str(ref vs) => { OranValue::Float(vs.parse::<f64>().unwrap_or_else(|e| panic!("{}", e)) % f64::from(other)) },
                    _ => panic!("Variable types are not Number: {:?}", self)
                }
            },
            _ => panic!("Variable types are not Number: {:?}", self)
        }
    }
}

impl From<OranValue> for f64 {
    fn from(val: OranValue) -> Self {
        match val {
            OranValue::Float(ref fl) => { *fl },
            OranValue::Str(ref str) => { str.parse().unwrap() },
            OranValue::Variable(ref v) => {
                match v.value {
                    OranVariableValue::Float(ref fl) => { *fl },
                    OranVariableValue::Str(ref s) => { s.parse().unwrap() }
                    _ => panic!("Variable types are not Number: {:?}", val)
                }
            },
            _ => panic!("Variable types are not Number: {:?}", val)
        }
    }
}

impl From<OranValue> for String {
    fn from(val: OranValue) -> Self {
        match val {
            OranValue::Str(ref st) => { st.to_string() },
            OranValue::Float(ref fl) => { fl.to_string() },
            OranValue::Boolean(ref bl) => { bl.to_string() },
            OranValue::Variable(ref v) => { v.value.to_string() },
        }
    }
}

impl From<OranValue> for OranVariableValue {
    fn from(val: OranValue) -> Self {
        match val {
            OranValue::Str(ref st) => { OranVariableValue::Str(st.to_string()) },
            OranValue::Float(ref fl) => { OranVariableValue::Float(*fl) },
            OranValue::Boolean(ref bl) => { OranVariableValue::Boolean(*bl) },
            OranValue::Variable(ref v) => { 
                match v.value {
                    OranVariableValue::Str(ref s) => { OranVariableValue::Str(s.to_string()) },
                    OranVariableValue::Float(ref fl) => { OranVariableValue::Float(*fl) },
                    OranVariableValue::Boolean(ref bl) => { OranVariableValue::Boolean(*bl) },
                }
            },
        }
    }
}
