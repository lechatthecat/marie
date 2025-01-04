use clap::{Arg, ArgMatches, Command};
use std::fs;

mod bytecode;
mod compiler;
mod debugger;
mod error;
mod gc;
mod reader;
mod value;

const INPUT_STR: &str = "INPUT";
const SHOW_TOKENS_STR: &str = "tokens";
const SHOW_AST_STR: &str = "ast";
const DISASSEMBLE_STR: &str = "disassemble";
const DEBUG_STR: &str = "debug";
const LITERAL_INPUT: &str = "c";

fn get_input(matches: &ArgMatches) -> Option<reader::input::Input> {
    if let Some(literal_input) = matches.get_one::<String>(LITERAL_INPUT) {
        return Some(reader::input::Input {
            source: reader::input::Source::Literal,
            content: literal_input.to_string(),
        });
    }
    if let Some(input_file) = matches.get_one::<String>(INPUT_STR) {
        match fs::read_to_string(input_file) {
            Ok(input) => {
                return Some(reader::input::Input {
                    source: reader::input::Source::File(input_file.to_string()),
                    content: input,
                });
            }
            Err(err) => {
                println!("Error reading {}: {}", input_file, err);
                std::process::exit(-1);
            }
        }
    }

    None
}

fn main() {
    let matches = Command::new("marie")
        .version("0.1.0")
        .about("marie language interpreter")
        .author("lechat thecat")
        .arg(
            Arg::new(INPUT_STR)
                .help("sets input file to use")
                .required(false)
                .index(1),
        )
        .arg(
            Arg::new(SHOW_TOKENS_STR)
                .long(SHOW_TOKENS_STR)
                .action(clap::ArgAction::SetTrue)
                .help("show the token stream"),
        )
        .arg(
            Arg::new(SHOW_AST_STR)
                .long(SHOW_AST_STR)
                .action(clap::ArgAction::SetTrue)
                .help("show the AST"),
        )
        .arg(
            Arg::new(DISASSEMBLE_STR)
                .long(DISASSEMBLE_STR)
                .action(clap::ArgAction::SetTrue)
                .help("show the bytecode"),
        )
        .arg(
            Arg::new(DEBUG_STR)
                .long(DEBUG_STR)
                .action(clap::ArgAction::SetTrue)
                .help("run in the debugger"),
        )
        .arg(
            Arg::new(LITERAL_INPUT)
                .long("c")
                .action(clap::ArgAction::Set)
                .help("provide a literal string of marie code"),
        )
        .get_matches();

    if let Some(input) = get_input(&matches) {
        let func_or_err = compiler::Compiler::compile(input.content.clone());

        match func_or_err {
            Ok(func) => {
                if matches.get_flag(DISASSEMBLE_STR) {
                    println!(
                        "{}",
                        bytecode::bytecode_interpreter::disassemble_chunk(&func.chunk, "")
                    );
                    std::process::exit(0);
                }
                if matches.get_flag(DEBUG_STR) {
                    debugger::debugger::Debugger::new(func, input.content).debug();
                    std::process::exit(0);
                }
                let mut interpreter = bytecode::bytecode_interpreter::Interpreter::default();
                let res = interpreter.interpret(func);
                match res {
                    Ok(()) => {
                        std::process::exit(0);
                    }
                    Err(bytecode::bytecode_interpreter::InterpreterError::Runtime(err)) => {
                        println!(
                            "Runtime error: {}\n\n{}",
                            err,
                            interpreter.format_backtrace()
                        );

                        std::process::exit(1);
                    }
                }
            }
            Err(err) => {
                error::error_formatting::format_compiler_error(&err, &input);
                std::process::exit(1);
            }
        }
    }
}
