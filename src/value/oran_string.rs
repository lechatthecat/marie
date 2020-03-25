use std::fmt;
use std::hash::{Hash, Hasher};
use super::oran_value::OranValue;

#[derive(Clone, Eq, Debug)]
pub struct OranString<'a> {
    pub val_str: Option<String>,
    pub ref_str: Option<&'a str>,
    pub is_ref: bool,
}

impl<'a> From<OranString<'a>> for String {
    fn from(val: OranString) -> Self {
        if val.is_ref {
            val.ref_str.unwrap().to_string()
        } else {
            val.val_str.as_ref().unwrap().to_string()
        }
    }
}

impl<'a> From<&OranString<'a>> for String {
    fn from(val: &OranString) -> Self {
        if val.is_ref {
            val.ref_str.unwrap().to_string()
        } else {
            val.val_str.as_ref().unwrap().to_string()
        }
    }
}

impl<'a> From<String> for OranString<'a> {
    fn from(val: String) -> Self {
        OranString {
            val_str: Some(val),
            ref_str: None,
            is_ref: false
        }
    }
}

impl<'a> From<&'a str> for OranString<'a> {
    fn from(val: &'a str) -> Self {
        OranString {
            val_str: None,
            ref_str: Some(val),
            is_ref: true
        }
    }
}

impl<'a> From<&'a String> for OranString<'a> {
    fn from(val: &'a String) -> Self {
        OranString {
            val_str: None,
            ref_str: Some(val),
            is_ref: true
        }
    }
}

impl<'a> From<OranValue<'a>> for OranString<'a> {
    fn from(val: OranValue<'a>) -> Self {
        let val = String::from(val);
        OranString {
            val_str: Some(val),
            ref_str: None,
            is_ref: false
        }
    }
}

impl<'a> From<&OranValue<'a>> for OranString<'a> {
    fn from(val: &OranValue<'a>) -> Self {
        let val = String::from(val);
        OranString {
            val_str: Some(val),
            ref_str: None,
            is_ref: false
        }
    }
}

impl<'a> PartialEq for OranString<'a> {
    fn eq(&self, other: &OranString) -> bool {
        String::from(self) == String::from(other)
    }
}

impl fmt::Display for OranString<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.is_ref {
            write!(f, "{}", self.ref_str.unwrap())
        } else {
            write!(f, "{}", self.val_str.as_ref().unwrap())
        }            
    }
}

impl<'a> Hash for OranString<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        if self.is_ref {
            String::from(self.ref_str.unwrap()).hash(state);
        } else {
            String::from(self.val_str.as_ref().unwrap()).hash(state);
        }      
        
    }
}
