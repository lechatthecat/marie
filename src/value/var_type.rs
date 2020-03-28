#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum VarType {
    CONSTANT,
    VARIABLE,
    REASSIGNED
}

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub enum OranValueType {
    VALUE,
    FUNCTION,
    //TEMP
}

