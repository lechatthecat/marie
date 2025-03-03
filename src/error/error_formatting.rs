use crate::compiler;
use crate::reader::input;
use crate::reader::scanner;

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
    let line = if line > 0 {
        line - 1 
    } else {
        0
    };
    if let Some(line) = input.content.lines().nth(line) {
        eprintln!("{}", line);
    } else {
        eprintln!("{}", "");
    }
    if col > 0 {
        eprint!("{:~<1$}", "".blue().bold(), col as usize);
        eprintln!("{}", "^".blue().bold());
    } else {
        eprint!("{:~<1$}", "".blue().bold(), 1);
        eprintln!("{}", "^".blue().bold());
    }
}

enum CompilerErrorKind {
    Parse,
    Semantic,
}

fn format_compiler_error_info(
    err: &compiler::ErrorInfo,
    input: &input::Input,
    kind: CompilerErrorKind,
) {
    eprintln!(
        "marie: {}: {}",
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

pub fn format_compiler_error(err: &compiler::Error, input: &input::Input) {
    match err {
        compiler::Error::Lexical(err) => format_lexical_error(err, input),
        compiler::Error::Parse(err) => {
            format_compiler_error_info(err, input, CompilerErrorKind::Parse)
        }
        compiler::Error::Semantic(err) => {
            format_compiler_error_info(err, input, CompilerErrorKind::Semantic)
        }
        compiler::Error::Internal(err) => {
            eprintln!(
                "marie: {}: {}",
                "internal error".red().bold(),
                err.white().bold()
            );
        }
    }
}

pub fn format_lexical_error(err: &scanner::Error, input: &input::Input) {
    eprintln!(
        "marie: {}: {}",
        "lexical error".red().bold(),
        err.what.white().bold(),
    );

    format_input(input, err.line, err.col);
}
