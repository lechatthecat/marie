use crate::value::var_type::VarType;
use super::Rule;
use pest::iterators::Pair;

#[derive(PartialEq, Debug)]
pub enum AstNode<'a> {
    Assign(Pair<'a, Rule>, VarType, String, Box<AstNode<'a>>),
    ArrayElementAssign(Pair<'a, Rule>, VarType, String, Box<AstNode<'a>>, Box<AstNode<'a>>),
    FunctionDefine(Pair<'a, Rule>, String, Vec<AstNode<'a>>, Vec<AstNode<'a>>, Box<AstNode<'a>>),
    FunctionCall(Pair<'a, Rule>, String, Vec<AstNode<'a>>),
    Ident(Pair<'a, Rule>, String),
    Argument(Pair<'a, Rule>, String, Box<AstNode<'a>>),
    Str(Pair<'a, Rule>, String),
    Strs(Pair<'a, Rule>, Vec<AstNode<'a>>),
    Number(Pair<'a, Rule>, f64),
    Calc(CalcOp, Box<AstNode<'a>>, Box<AstNode<'a>>),
    Bool(Pair<'a, Rule>, bool),
    IF(Pair<'a, Rule>, Box<AstNode<'a>>, Vec<AstNode<'a>>, Vec<(Vec<AstNode<'a>>, Vec<AstNode<'a>>)>, Vec<AstNode<'a>>),
    Condition(ComparisonlOperatorType, Box<AstNode<'a>>, Box<AstNode<'a>>),
    Comparison(Pair<'a, Rule>, Box<AstNode<'a>>, LogicalOperatorType, Box<AstNode<'a>>),
    ForLoop(Pair<'a, Rule>, bool, VarType, String, Box<AstNode<'a>>, Box<AstNode<'a>>, Vec<AstNode<'a>>),
    ForLoopIdent(Pair<'a, Rule>, VarType, String, String, Vec<AstNode<'a>>),
    ForLoopArray(Pair<'a, Rule>, VarType, String, Vec<AstNode<'a>>, Vec<AstNode<'a>>),
    Array(Pair<'a, Rule>,Vec<AstNode<'a>>),
    ArrayElement(Pair<'a, Rule>, Box<AstNode<'a>>, Vec<AstNode<'a>>),
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
    fn from(val: AstNode<'a>) -> Self {
        match val {
            AstNode::FunctionDefine(ref _loc, ref _name, ref arg, ref _body, ref _fn_return) => {
                String::from(arg.into_iter().nth(0).unwrap().clone())
            }
            AstNode::Argument(ref _loc, ref s, ref _a) => {
                s.to_string()
            }
            AstNode::Str(ref _loc, ref s) => {
                s.to_string()
            }
            AstNode::Number(ref _loc, ref n) => {
                n.to_string()
            }
            AstNode::Ident(ref _loc, ref s) => {
                s.to_string()
            }
            _ => "".to_owned()
        }
    }
}

impl<'a> From<&AstNode<'a>> for String {
    fn from(val: &AstNode<'a>) -> Self {
        match val {
            AstNode::FunctionDefine(ref _loc, ref _name, ref arg, ref _body, ref _fn_return) => {
                String::from(arg.into_iter().nth(0).unwrap().clone())
            }
            AstNode::Argument(ref _loc, ref s, ref _a) => {
                s.to_string()
            }
            AstNode::Str(ref _loc, ref s) => {
                s.to_string()
            }
            AstNode::Number(ref _loc, ref n) => {
                n.to_string()
            }
            AstNode::Ident(ref _loc, ref s) => {
                s.to_string()
            }
            _ => "".to_owned()
        }
    }
}

impl<'a> From<AstNode<'a>> for f64 {
    fn from(val: AstNode<'a>) -> Self {
        match val {
            AstNode::Number(ref _loc, n) => {
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
