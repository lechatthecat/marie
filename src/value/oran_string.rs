use std::fmt::{Display, Formatter, Result};
use std::hash::Hash;
use super::oran_value::OranValue;
use std::borrow::Cow;

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct OranString<'a> {
    pub val_str: Option<Cow<'a, str>>,
}

impl<'a> From<&'a String> for OranString<'a> {
    fn from(val: &'a String) -> Self {
        OranString {
            val_str: Some(Cow::from(val)),
        }
    }
}

impl<'a> From<String> for OranString<'a> {
    fn from(val: String) -> Self {
        OranString {
            val_str: Some(Cow::from(val)),
        }
    }
}

impl<'a> From<&'a str> for OranString<'a> {
    fn from(val: &'a str) -> Self {
        OranString {
            val_str: Some(Cow::from(val)),
        }
    }
}

impl<'a> From<OranString<'a>> for String {
    fn from(val: OranString) -> Self {
        val.val_str.unwrap().to_string()
    }
}

impl<'a> From<&OranString<'a>> for String {
    fn from(val: &OranString) -> Self {
        val.val_str.as_ref().unwrap().to_string()
    }
}

impl<'a> From<OranValue<'a>> for OranString<'a> {
    fn from(val: OranValue<'a>) -> Self {
        let val = String::from(val);
        OranString {
            val_str: Some(Cow::from(val)),
        }
    }
}

impl<'a> From<&OranValue<'a>> for OranString<'a> {
    fn from(val: &OranValue<'a>) -> Self {
        let val = String::from(val);
        OranString {
            val_str: Some(Cow::from(val)),
        }
    }
}

impl Display for OranString<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", self.val_str.as_ref().unwrap())           
    }
}
