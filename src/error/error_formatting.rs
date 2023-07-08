use crate::input;
use colored::*;

fn format_input(input: &input::Input, line: usize, col: i64) {
    eprintln!(
        "in {}, at line {}, column {}:",
        match &input.source {
            input::Source::Literal => "<command-line input>",
            input::Source::File(filename) => filename,
        },
        line,
        col
    );
    eprintln!("{}", input.content.lines().nth(line - 1).unwrap());
    eprint!("{:~<1$}", "".blue().bold(), col as usize);
    eprintln!("{}", "^".blue().bold());
}

enum CompilerErrorKind {
    Parse,
    Semantic,
}


#[derive(Debug)]
pub struct ErrorInfo {
    pub what: String,
    pub line: usize,
    pub col: i64,
}

#[derive(Debug)]
pub enum Error {
    Lexical(super::scanner_error::Error),
    Parse(ErrorInfo),
    Semantic(ErrorInfo),
    Internal(String),
}

fn format_compiler_error_info(
    err: &ErrorInfo,
    input: &input::Input,
    kind: CompilerErrorKind,
) {
    eprintln!(
        "{}: {}",
        match kind {
            CompilerErrorKind::Parse => "parse error",
            CompilerErrorKind::Semantic => "semantic error",
        }
        .to_string()
        .red()
        .bold(),
        err.what.white().bold(),
    );

    format_input(input, err.line, err.col);
}

pub fn format_compiler_error(err: &Error, input: &input::Input) {
    match err {
        Error::Lexical(err) => format_lexical_error(err, input),
        Error::Parse(err) => {
            format_compiler_error_info(err, input, CompilerErrorKind::Parse)
        }
        Error::Semantic(err) => {
            format_compiler_error_info(err, input, CompilerErrorKind::Semantic)
        }
        Error::Internal(err) => {
            eprintln!(
                "{}: {}",
                "internal error".red().bold(),
                err.white().bold()
            );
        }
    }
}

pub fn format_parse_error(err: &super::parser_error::Error, input: &input::Input) {
    let err_str = format!("{:?}", err);
    eprintln!(
        "{}: {}",
        "parse error".red().bold(),
        err_str.white().bold()
    );

    let (line, col) = match err {
        super::parser_error::Error::UnexpectedToken(tok) => (&tok.line, &tok.col),
        super::parser_error::Error::TokenMismatch { found, .. } => (&found.line, &found.col),
        super::parser_error::Error::MaxParamsExceeded { line, col, .. } => (line, col),
        super::parser_error::Error::ReturnNotInFun { line, col, .. } => (line, col),
        super::parser_error::Error::InvalidAssignment { line, col, .. } => (line, col),
        super::parser_error::Error::TooManyArguments { line, col, .. } => (line, col),
        super::parser_error::Error::ExpectedExpression { line, col, .. } => (line, col),
        super::parser_error::Error::InvalidTokenInUnaryOp { line, col, .. } => (line, col),
        super::parser_error::Error::InvalidTokenInBinaryOp { line, col, .. } => (line, col),
        super::parser_error::Error::ParseError { line, col, .. } => (line, col),
    };

    format_input(input, *line, *col);
}

pub fn format_lexical_error(err: &super::scanner_error::Error, input: &input::Input) {
    eprintln!(
        "{}: {}",
        "lexical error".red().bold(),
        err.what.white().bold(),
    );

    format_input(input, err.line, err.col);
}
