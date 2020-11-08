#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum VarType {
    Constant,
    VariableFirstAssigned,
    VariableReAssigned
}

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub enum FunctionOrValueType {
    Value,
    Function,
}
