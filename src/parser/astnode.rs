#[derive(PartialEq, Debug, Clone)]
pub enum AstNode {
    Assign(i32, String, Box<AstNode>),
    FunctionDefine(String, Vec<AstNode>, Vec<AstNode>, Box<AstNode>),
    FunctionCall(Function, String, Vec<AstNode>),
    Ident(String),
    Argument(String, Box<AstNode>),
    Str(String),
    Strs(Vec<AstNode>),
    Number(f64),
    Calc(CalcOp, Box<AstNode>, Box<AstNode>),
    Bool(bool),
    Comparison(Box<AstNode>, i32, Box<AstNode>),
    IF(Vec<AstNode>, Vec<AstNode>, Vec<Vec<AstNode>>, Vec<Vec<AstNode>>, Vec<AstNode>),
    // ElSEIF(Vec<(Vec<AstNode>, Vec<AstNode>)>),
    // ELSE(Vec<AstNode>),
    Null
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
            _ => "".to_string()
        }
    }
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