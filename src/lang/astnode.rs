#[derive(PartialEq, Debug, Clone)]
pub enum AstNode {
    Assign(i32, String, Box<AstNode>),
    FunctionDefine(String, Vec<AstNode>, Vec<AstNode>, Vec<AstNode>),
    FunctionCall(Function, String, Vec<AstNode>),
    Ident(String),
    Str(String),
    Strs(Vec<AstNode>),
    Number(f64),
    Calc(CalcOp, Box<AstNode>, Box<AstNode>),
    Null
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum CalcOp {
    Plus,
    Minus,
    Times,
    Divide,
    Modulus,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Function {
    NotDefault,
    Print,
    Println
}

impl AstNode {
    pub fn calc<L, R>(op: CalcOp, lhs: L, rhs: R) -> Self
    where
        L: Into<AstNode>,
        R: Into<AstNode>,
    {
        AstNode::Calc(op.into(), Box::new(lhs.into()), Box::new(rhs.into()))
    }
}