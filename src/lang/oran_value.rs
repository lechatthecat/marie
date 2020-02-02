use std::fmt;
use std::ops::{Add, Sub, Div, Mul, Rem};

#[derive(Clone, PartialEq, Debug)]
pub enum OranValue {
    Float(f64),
    Str(String),
    Boolean(bool),
}

impl fmt::Display for OranValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            OranValue::Float(ref fl) => write!(f, "{}", fl),
            OranValue::Str(ref s) => write!(f, "{}", s),
            OranValue::Boolean(ref b) => write!(f, "{}", b),
        }
    }
}

impl Sub for OranValue {
    type Output = Self;

    fn sub(self, other: Self) -> Self::Output {
        match self {
            OranValue::Float(ref fl) => { OranValue::Float(fl - f64::from(other)) },
            _ => panic!("Variable types are not Number: {:?}", self)
        }
    }
}

impl Add for OranValue {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        match self {
            OranValue::Float(ref fl) => { OranValue::Float(fl + f64::from(other)) },
            _ => panic!("Variable types are not Number: {:?}", self)
        }
    }
}

impl Div for OranValue {
    type Output = Self;

    fn div(self, other: Self) -> Self::Output {
        match self {
            OranValue::Float(ref fl) => { OranValue::Float(fl / f64::from(other)) },
            _ => panic!("Variable types are not Number: {:?}", self)
        }
    }
}

impl Mul for OranValue {
    type Output = Self;

    fn mul(self, other: Self) -> Self::Output {
        match self {
            OranValue::Float(ref fl) => { OranValue::Float(fl * f64::from(other)) },
            _ => panic!("Variable types are not Number: {:?}", self)
        }
    }
}

impl Rem for OranValue {
    type Output = Self;

    fn rem(self, other: Self) -> Self::Output {
        match self {
            OranValue::Float(ref fl) => { OranValue::Float(fl % f64::from(other)) },
            _ => panic!("Variable types are not Number: {:?}", self)
        }
    }
}

impl From<OranValue> for f64 {
    fn from(val: OranValue) -> Self {
        match val {
            OranValue::Float(ref fl) => { *fl },
            OranValue::Str(ref str) => { str.parse().unwrap() },
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
            _ => panic!("Unknown variable type: {:?}", val)
        }
    }
}
