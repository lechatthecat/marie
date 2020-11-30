use crate::value::var_type::VarType;
use std::collections::LinkedList;

#[derive(PartialEq, Debug)]
pub enum AstNode {
    Assign(VarType, String, Box<AstNode>),
    FunctionDefine(String, Vec<AstNode>, Vec<AstNode>, Box<AstNode>),
    FunctionCall(String, Vec<AstNode>),
    Ident(String),
    Argument(String, Box<AstNode>),
    Str(String),
    Strs(Vec<AstNode>),
    Number(f64),
    Calc(CalcOp, Box<AstNode>, Box<AstNode>),
    Bool(bool),
    IF(Box<AstNode>, Vec<AstNode>, LinkedList<(Vec<AstNode>, Vec<AstNode>)>, Vec<AstNode>),
    Condition(ComparisonlOperatorType, Box<AstNode>, Box<AstNode>),
    Comparison(Box<AstNode>, LogicalOperatorType, Box<AstNode>),
    ForLoop(bool, VarType, String, Box<AstNode>, Box<AstNode>, Vec<AstNode>),
    Null
}

impl Clone for AstNode {
    fn clone(&self) -> Self {
        match self {
            AstNode::Assign(v, s, b) => AstNode::Assign(*v, s.clone(), b.clone()),
            AstNode::FunctionDefine(s, va, va2, b) =>  AstNode::FunctionDefine(s.clone(), va.clone(), va2.clone(), b.clone()),
            AstNode::FunctionCall(s, va) => AstNode::FunctionCall(s.clone(), va.clone()),
            AstNode::Ident(s) => AstNode::Ident(s.clone()),
            AstNode::Argument(s, b) => AstNode::Argument(s.clone(), b.clone()),
            AstNode::Str(s) => AstNode::Str(s.clone()),
            AstNode::Strs(va) => AstNode::Strs(va.clone()),
            AstNode::Number(f) => AstNode::Number(*f),
            AstNode::Calc(c, ba, ba2) => AstNode::Calc(*c, ba.clone(), ba2.clone()),
            AstNode::Bool(b) => AstNode::Bool(*b),
            AstNode::IF(ba, va, llist, va2) => AstNode::IF(ba.clone(), va.clone(), llist.clone(), va2.clone()),
            AstNode::Condition(c, ba, ba2) => AstNode::Condition(*c, ba.clone(), ba2.clone()),
            AstNode::Comparison(ba, lot, ba2) => AstNode::Comparison(ba.clone(), *lot, ba2.clone()),
            AstNode::ForLoop(b, vt, s, ba, ba2, va) => AstNode::ForLoop(*b, *vt, s.clone(), ba.clone(), ba2.clone(), va.clone()),
            AstNode::Null => AstNode::Null
        }
    }
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
            _ => "".to_string()
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
            _ => "".to_string()
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
