use std::fmt::{Display, Formatter, Result};
use std::hash::Hash;
use super::oran_value::OranValue;
use std::borrow::Cow;

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct OranStringRef<'a> {
    pub val_str: Cow<'a, str>,
}

impl<'a> From<&'a String> for OranStringRef<'a> {
    fn from(val: &'a String) -> Self {
        OranStringRef {
            val_str:Cow::from(val),
        }
    }
}

impl<'a> From<String> for OranStringRef<'a> {
    fn from(val: String) -> Self {
        OranStringRef {
            val_str: Cow::from(val),
        }
    }
}

impl<'a> From<&'a str> for OranStringRef<'a> {
    fn from(val: &'a str) -> Self {
        OranStringRef {
            val_str: Cow::from(val),
        }
    }
}

impl<'a> From<OranStringRef<'a>> for String {
    fn from(val: OranStringRef) -> Self {
        val.val_str.to_string()
    }
}

impl<'a> From<&OranStringRef<'a>> for String {
    fn from(val: &OranStringRef) -> Self {
        val.val_str.as_ref().to_string()
    }
}

impl<'a> From<OranValue<'a>> for OranStringRef<'a> {
    fn from(val: OranValue<'a>) -> Self {
        let val = String::from(val);
        OranStringRef {
            val_str: Cow::from(val),
        }
    }
}

impl<'a> From<&OranValue<'a>> for OranStringRef<'a> {
    fn from(val: &OranValue<'a>) -> Self {
        let val = String::from(val);
        OranStringRef {
            val_str: Cow::from(val),
        }
    }
}

impl Display for OranStringRef<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", self.val_str)           
    }
}
