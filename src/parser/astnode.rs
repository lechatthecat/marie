use crate::value::var_type::VarType;

#[derive(PartialEq, Debug)]
pub enum AstNode {
    Assign(VarType, String, Box<AstNode>),
    ArrayElementAssign(VarType, String, Box<AstNode>, Box<AstNode>),
    FunctionDefine(String, Vec<AstNode>, Vec<AstNode>, Box<AstNode>),
    FunctionCall(String, Vec<AstNode>),
    Ident(String),
    Argument(String, Box<AstNode>),
    Str(String),
    Strs(Vec<AstNode>),
    Number(f64),
    Calc(CalcOp, Box<AstNode>, Box<AstNode>),
    Bool(bool),
    IF(Box<AstNode>, Vec<AstNode>, Vec<(Vec<AstNode>, Vec<AstNode>)>, Vec<AstNode>),
    Condition(ComparisonlOperatorType, Box<AstNode>, Box<AstNode>),
    Comparison(Box<AstNode>, LogicalOperatorType, Box<AstNode>),
    ForLoop(bool, VarType, String, Box<AstNode>, Box<AstNode>, Vec<AstNode>),
    ForLoopIdent(VarType, String, String, Vec<AstNode>),
    ForLoopArray(VarType, String, Vec<AstNode>, Vec<AstNode>),
    Array(Vec<AstNode>),
    ArrayElement(Box<AstNode>, Vec<AstNode>),
    Null
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum CalcOp {
    Plus,
    Minus,
    Times,
    Divide,
    Modulus,
    Power
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum ComparisonlOperatorType {
    AND,
    OR
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum LogicalOperatorType {
    Equal,
    BiggerThan,
    SmallerThan,
    EbiggerThan,
    EsmallerThan
}

impl From<AstNode> for String {
    fn from(val: AstNode) -> Self {
        match val {
            AstNode::FunctionDefine(ref _name, ref arg, ref _body, ref _fn_return) => {
                String::from(arg.into_iter().nth(0).unwrap().clone())
            }
            AstNode::Argument(ref s, ref _a) => {
                s.to_string()
            }
            AstNode::Str(ref s) => {
                s.to_string()
            }
            AstNode::Number(ref n) => {
                n.to_string()
            }
            AstNode::Ident(ref s) => {
                s.to_string()
            }
            _ => "".to_owned()
        }
    }
}

impl From<&AstNode> for String {
    fn from(val: &AstNode) -> Self {
        match val {
            AstNode::FunctionDefine(ref _name, ref arg, ref _body, ref _fn_return) => {
                String::from(arg.into_iter().nth(0).unwrap().clone())
            }
            AstNode::Argument(ref s, ref _a) => {
                s.to_string()
            }
            AstNode::Str(ref s) => {
                s.to_string()
            }
            AstNode::Number(ref n) => {
                n.to_string()
            }
            AstNode::Ident(ref s) => {
                s.to_string()
            }
            _ => "".to_owned()
        }
    }
}

impl From<AstNode> for f64 {
    fn from(val: AstNode) -> Self {
        match val {
            AstNode::Number(n) => {
                n
            }
            _ => panic!("Failed to parse: {:?}", val)
        }
    }
}

impl AstNode {
    pub fn calculation<L, R>(op: CalcOp, lhs: L, rhs: R) -> Self
    where
        L: Into<AstNode>,
        R: Into<AstNode>,
    {
        AstNode::Calc(op.into(), Box::new(lhs.into()), Box::new(rhs.into()))
    }
}

impl AstNode {
    pub fn condition<L, R>(op: ComparisonlOperatorType, lhs: L, rhs: R) -> Self
    where
        L: Into<AstNode>,
        R: Into<AstNode>,
    {
        AstNode::Condition(op.into(), Box::new(lhs.into()), Box::new(rhs.into()))
    }
}
