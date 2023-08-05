use std::fmt;

use super::functions::Type;

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    This(SourceLocation),
    Unary(UnaryOp, Box<Expr>),
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Call(Box<Expr>, SourceLocation, Vec<Expr>),
    Get(Box<Expr>, Symbol),
    Grouping(Box<Expr>),
    Variable(Symbol),
    Assign(Symbol, Box<Expr>),
    Logical(Box<Expr>, LogicalOp, Box<Expr>),
    Set(Box<Expr>, Symbol, Box<Expr>),
    Super(SourceLocation, Symbol),
    List(Vec<Expr>),
    Subscript {
        value: Box<Expr>,
        slice: Box<Expr>,
        source_location: SourceLocation,
    },
    SetItem {
        lhs: Box<Expr>,
        slice: Box<Expr>,
        rhs: Box<Expr>,
        source_location: SourceLocation,
    },
    Lambda(LambdaDecl),
}

#[derive(Debug, Clone, Copy)]
pub struct SourceLocation {
    pub line: usize,
    pub col: i64,
}

#[derive(Debug, Clone)]
pub enum LogicalOp {
    Or,
    And,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Symbol {
    pub is_mutable: bool,
    pub name: String,
    pub val_type: Type, 
    pub line: usize,
    pub col: i64,
}

#[derive(Debug, Clone)]
pub struct FunDecl {
    pub name: Symbol,
    pub params: Vec<Symbol>,
    pub body: Vec<Stmt>,
    pub function_type: Type,
}

#[derive(Debug, Clone)]
pub struct LambdaDecl {
    pub params: Vec<Symbol>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct ClassDecl {
    pub name: Symbol,
    pub superclass: Option<Symbol>,
    pub methods: Vec<FunDecl>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Expr),
    FunDecl(FunDecl),
    ClassDecl(ClassDecl),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    VarDecl(Symbol, Option<Expr>),
    Block(Vec<Stmt>),
    Return(SourceLocation, Option<Expr>),
    While(Expr, Box<Stmt>),
    Println(Expr),
    Print(Expr),
}

#[derive(Debug, Copy, Clone)]
pub enum UnaryOpTy {
    Minus,
    Bang,
}

#[derive(Debug, Copy, Clone)]
pub struct UnaryOp {
    pub ty: UnaryOpTy,
    pub line: usize,
    pub col: i64,
}

#[derive(Debug, Copy, Clone)]
pub enum BinaryOpTy {
    EqualEqual,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Plus,
    Minus,
    Star,
    Slash,
}

#[derive(Debug, Copy, Clone)]
pub struct BinaryOp {
    pub ty: BinaryOpTy,
    pub line: usize,
    pub col: i64,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Number(f64),
    String(String),
    True,
    False,
    Nil,
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Number(n) => write!(f, "{}", n),
            Literal::String(s) => write!(f, "{}", s),
            Literal::True => write!(f, "true"),
            Literal::False => write!(f, "false"),
            Literal::Nil => write!(f, "nil"),
        }
    }
}
