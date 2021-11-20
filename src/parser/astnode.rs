use crate::value::var_type::VarType;
use std::collections::LinkedList;

#[derive(PartialEq, Debug)]
pub enum AstNode {
    Assign((String, usize, usize), VarType, String, Box<AstNode>),
    ArrayElementAssign((String, usize, usize), VarType, String, Box<AstNode>, Box<AstNode>),
    FunctionDefine((String, usize, usize), String, Vec<AstNode>, Vec<AstNode>, Box<AstNode>),
    FunctionCall((String, usize, usize), String, Vec<AstNode>),
    Ident((String, usize, usize), String),
    Argument((String, usize, usize), String, Box<AstNode>),
    Str((String, usize, usize), String),
    Strs((String, usize, usize), Vec<AstNode>),
    Number((String, usize, usize), f64),
    Calc(CalcOp, Box<AstNode>, Box<AstNode>),
    Bool((String, usize, usize), bool),
    IF((String, usize, usize), Box<AstNode>, Vec<AstNode>, LinkedList<(Vec<AstNode>, Vec<AstNode>)>, Vec<AstNode>),
    Condition(ComparisonlOperatorType, Box<AstNode>, Box<AstNode>),
    Comparison((String, usize, usize), Box<AstNode>, LogicalOperatorType, Box<AstNode>),
    ForLoop((String, usize, usize), bool, VarType, String, Box<AstNode>, Box<AstNode>, Vec<AstNode>),
    Array((String, usize, usize),Vec<AstNode>),
    ArrayElement((String, usize, usize), Box<AstNode>, Vec<AstNode>),
    Null
}

impl Clone for AstNode {
    fn clone(&self) -> Self {
        match self {
            AstNode::Assign(loc, v, s, b) => AstNode::Assign(loc.clone(), *v, s.clone(), b.clone()),
            AstNode::ArrayElementAssign(loc, v, str, a, b) => AstNode::ArrayElementAssign(loc.clone(), *v, str.clone(), a.clone(), b.clone()),
            AstNode::FunctionDefine(loc, s, va, va2, b) =>  AstNode::FunctionDefine(loc.clone(), s.clone(), va.clone(), va2.clone(), b.clone()),
            AstNode::FunctionCall(loc, s, va) => AstNode::FunctionCall(loc.clone(), s.clone(), va.clone()),
            AstNode::Ident(loc, s) => AstNode::Ident(loc.clone(), s.clone()),
            AstNode::Argument(loc, s, b) => AstNode::Argument(loc.clone(), s.clone(), b.clone()),
            AstNode::Str(loc, s) => AstNode::Str(loc.clone(), s.clone()),
            AstNode::Strs(loc, va) => AstNode::Strs(loc.clone(), va.clone()),
            AstNode::Number(loc, f) => AstNode::Number(loc.clone(), *f),
            AstNode::Calc(c, ba, ba2) => AstNode::Calc(*c, ba.clone(), ba2.clone()),
            AstNode::Bool(loc, b) => AstNode::Bool(loc.clone(), *b),
            AstNode::IF(loc, ba, va, llist, va2) => AstNode::IF(loc.clone(), ba.clone(), va.clone(), llist.clone(), va2.clone()),
            AstNode::Condition(c, ba, ba2) => AstNode::Condition(*c, ba.clone(), ba2.clone()),
            AstNode::Comparison(loc, ba, lot, ba2) => AstNode::Comparison(loc.clone(), ba.clone(), *lot, ba2.clone()),
            AstNode::ForLoop(loc, b, vt, s, ba, ba2, va) => AstNode::ForLoop(loc.clone(), *b, *vt, s.clone(), ba.clone(), ba2.clone(), va.clone()),
            AstNode::Array(loc, b) => AstNode::Array(loc.clone(), b.clone()),
            AstNode::ArrayElement(loc, an, i) => AstNode::ArrayElement(loc.clone(), an.clone(), i.clone()),
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

impl From<&AstNode> for String {
    fn from(val: &AstNode) -> Self {
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

impl From<AstNode> for f64 {
    fn from(val: AstNode) -> Self {
        match val {
            AstNode::Number(ref _loc, n) => {
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
