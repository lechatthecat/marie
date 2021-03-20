use crate::value::var_type::VarType;
use std::collections::LinkedList;

#[derive(PartialEq, Debug)]
pub enum AstNode {
    Assign((usize, usize),VarType, String, Box<AstNode>),
    FunctionDefine((usize, usize), String, Vec<AstNode>, Vec<AstNode>, Box<AstNode>),
    FunctionCall((usize, usize), String, Vec<AstNode>),
    Ident((usize, usize), String),
    Argument((usize, usize), String, Box<AstNode>),
    Str((usize, usize), String),
    Strs((usize, usize), Vec<AstNode>),
    Number((usize, usize), f64),
    Calc(CalcOp, Box<AstNode>, Box<AstNode>),
    Bool((usize, usize), bool),
    IF((usize, usize), Box<AstNode>, Vec<AstNode>, LinkedList<(Vec<AstNode>, Vec<AstNode>)>, Vec<AstNode>),
    Condition(ComparisonlOperatorType, Box<AstNode>, Box<AstNode>),
    Comparison((usize, usize), Box<AstNode>, LogicalOperatorType, Box<AstNode>),
    ForLoop((usize, usize), bool, VarType, String, Box<AstNode>, Box<AstNode>, Vec<AstNode>),
    Null
}

impl Clone for AstNode {
    fn clone(&self) -> Self {
        match self {
            AstNode::Assign(loc, v, s, b) => AstNode::Assign(*loc, *v, s.clone(), b.clone()),
            AstNode::FunctionDefine(loc, s, va, va2, b) =>  AstNode::FunctionDefine(*loc, s.clone(), va.clone(), va2.clone(), b.clone()),
            AstNode::FunctionCall(loc, s, va) => AstNode::FunctionCall(*loc, s.clone(), va.clone()),
            AstNode::Ident(loc, s) => AstNode::Ident(*loc, s.clone()),
            AstNode::Argument(loc, s, b) => AstNode::Argument(*loc, s.clone(), b.clone()),
            AstNode::Str(loc, s) => AstNode::Str(*loc, s.clone()),
            AstNode::Strs(loc, va) => AstNode::Strs(*loc, va.clone()),
            AstNode::Number(loc, f) => AstNode::Number(*loc, *f),
            AstNode::Calc(c, ba, ba2) => AstNode::Calc(*c, ba.clone(), ba2.clone()),
            AstNode::Bool(loc, b) => AstNode::Bool(*loc, *b),
            AstNode::IF(loc, ba, va, llist, va2) => AstNode::IF(*loc, ba.clone(), va.clone(), llist.clone(), va2.clone()),
            AstNode::Condition(c, ba, ba2) => AstNode::Condition(*c, ba.clone(), ba2.clone()),
            AstNode::Comparison(loc, ba, lot, ba2) => AstNode::Comparison(*loc, ba.clone(), *lot, ba2.clone()),
            AstNode::ForLoop(loc, b, vt, s, ba, ba2, va) => AstNode::ForLoop(*loc, *b, *vt, s.clone(), ba.clone(), ba2.clone(), va.clone()),
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
            _ => "".to_string()
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
            _ => "".to_string()
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
