#[derive(Clone, PartialEq, Debug)]
pub struct OranString<'a> {
    pub val_str: Option<String>,
    pub ref_str: Option<&'a str>,
    pub is_ref: bool,
}
