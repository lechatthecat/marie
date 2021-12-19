use pest::iterators::Pair;
use crate::parser::Rule;
use crate::value::var_type::VarType;

#[derive(PartialEq, Debug)]
pub enum AstNode<'a> {
    Assign(VarType, String, Box<AstNode<'a>>),
    ArrayElementAssign(VarType, String, Box<AstNode<'a>>, Box<AstNode<'a>>),
    FunctionDefine(String, Vec<AstNode<'a>>, Vec<(AstNode<'a>, Pair<'a, Rule>)>, Box<AstNode<'a>>),
    FunctionCall(String, Vec<AstNode<'a>>),
    Ident(String),
    Argument(String, Box<AstNode<'a>>),
    Str(String),
    Strs(Vec<AstNode<'a>>),
    Number(f64),
    Calc(CalcOp, Box<AstNode<'a>>, Box<AstNode<'a>>),
    Bool(bool),
    IF(Box<AstNode<'a>>, Vec<(AstNode<'a>, Pair<'a, Rule>)>, Vec<(Vec<AstNode<'a>>, Vec<(AstNode<'a>, Pair<'a, Rule>)>)>, Vec<(AstNode<'a>, Pair<'a, Rule>)>),
    Condition(ComparisonlOperatorType, Box<AstNode<'a>>, Box<AstNode<'a>>),
    Comparison(Box<AstNode<'a>>, LogicalOperatorType, Box<AstNode<'a>>),
    ForLoop(bool, VarType, String, Box<AstNode<'a>>, Box<AstNode<'a>>, Vec<(AstNode<'a>, Pair<'a, Rule>)>),
    ForLoopIdent(VarType, String, String, Vec<AstNode<'a>>),
    ForLoopArray(VarType, String, Vec<AstNode<'a>>, Vec<AstNode<'a>>),
    Array(Vec<AstNode<'a>>),
    ArrayElement(Box<AstNode<'a>>, Vec<AstNode<'a>>),
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

impl<'a> From<AstNode<'a>> for String {
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

impl<'a> From<&AstNode<'a>> for String {
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

impl<'a> From<AstNode<'a>> for f64 {
    fn from(val: AstNode) -> Self {
        match val {
            AstNode::Number(n) => {
                n
            }
            _ => panic!("Failed to parse: {:?}", val)
        }
    }
}

impl<'a> AstNode<'a> {
    pub fn calculation<L, R>(op: CalcOp, lhs: L, rhs: R) -> Self
    where
        L: Into<AstNode<'a>>,
        R: Into<AstNode<'a>>,
    {
        AstNode::Calc(op.into(), Box::new(lhs.into()), Box::new(rhs.into()))
    }
}

impl<'a> AstNode<'a> {
    pub fn condition<L, R>(op: ComparisonlOperatorType, lhs: L, rhs: R) -> Self
    where
        L: Into<AstNode<'a>>,
        R: Into<AstNode<'a>>,
    {
        AstNode::Condition(op.into(), Box::new(lhs.into()), Box::new(rhs.into()))
    }
}
