use std::fmt;

use crate::parser::{scanner, parser::FunctionKind};

pub enum Error {
    UnexpectedToken(scanner::Token),
    TokenMismatch {
        expected: scanner::TokenType,
        found: scanner::Token,
        maybe_on_err_string: Option<String>,
    },
    MaxParamsExceeded {
        kind: FunctionKind,
        line: usize,
        col: i64,
    },
    ReturnNotInFun {
        line: usize,
        col: i64,
    },
    InvalidAssignment {
        line: usize,
        col: i64,
    },
    TooManyArguments {
        line: usize,
        col: i64,
    },
    ExpectedExpression {
        token_type: scanner::TokenType,
        line: usize,
        col: i64,
    },
    InvalidTokenInUnaryOp {
        token_type: scanner::TokenType,
        line: usize,
        col: i64,
    },
    InvalidTokenInBinaryOp {
        token_type: scanner::TokenType,
        line: usize,
        col: i64,
    },
    ParseError {
        message: String,
        line: usize,
        col: i64,
    },
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Error::UnexpectedToken(tok) => write!(
                f,
                "Unexpected token {:?} at line:{},col:{}",
                tok.ty, tok.line, tok.col
            ),
            Error::TokenMismatch {
                maybe_on_err_string,
                expected,
                found,
            } => {
                write!(
                    f,
                    "Expected token {:?} but found {:?} at line:{},col:{}",
                    expected, found.ty, found.line, found.col
                )?;
                if let Some(on_err_string) = maybe_on_err_string {
                    write!(f, ": {}", on_err_string)?;
                }
                fmt::Result::Ok(())
            }
            Error::MaxParamsExceeded { kind, line, col } => write!(
                f,
                "Cannot have more than 255 parameters in a {:?} declaration. line:{},col:{}",
                kind, line, col
            ),
            Error::ReturnNotInFun { line, col } => write!(
                f,
                "return statement not enclosed in a FunDecl at line:{},col:{}",
                line, col
            ),
            Error::InvalidAssignment { line, col } => {
                write!(f, "invalid assignment target at line:{},col:{}", line, col)
            }
            Error::TooManyArguments { line, col } => write!(
                f,
                "Cannot have more than 255 arguments to a function call. line:{},col:{}",
                line, col
            ),
            Error::ExpectedExpression {
                token_type,
                line,
                col,
            } => write!(
                f,
                "Expected expression, but found token {:?} at line:{},col:{}",
                token_type, line, col
            ),
            Error::InvalidTokenInUnaryOp {
                token_type,
                line,
                col,
            } => write!(
                f,
                "invalid token in unary op {:?} at line:{},col:{}",
                token_type, line, col
            ),
            Error::InvalidTokenInBinaryOp {
                token_type,
                line,
                col,
            } => write!(
                f,
                "invalid token in binary op {:?} at line:{},col:{}",
                token_type, line, col
            ),
            Error::ParseError {
                message,
                line,
                col,
            } => write!(
                f,
                "{} at line:{},col:{}",
                message, line, col
            ),
        }
    }
}