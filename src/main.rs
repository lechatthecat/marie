// This software is created upon this project "tdp2110/crafting-interpreters-rs":
// https://github.com/tdp2110/crafting-interpreters-rs
// License: https://github.com/tdp2110/crafting-interpreters-rs/blob/bf621b7eb57c0307e9f20af30bab4b318faa8f4b/LICENSE

extern crate clap;
extern crate ctrlc;
extern crate itertools;

use bytecode_interpreter::Interpreter;
use clap::{Command, Arg};
use std::{fs, env};
mod builtins;
mod bytecode;
mod bytecode_interpreter;
mod compiler;
mod debugger;
mod error_formatting;
mod extensions;
mod gc;
mod input;
mod line_reader;
mod scanner;
mod value;
mod jit;
mod foreign;
mod step;

const INPUT_STR: &str = "INPUT";
const SHOW_TOKENS_STR: &str = "tokens";
const SHOW_AST_STR: &str = "ast";
//const DISASSEMBLE_STR: &str = "disassemble";
const DEBUG_STR: &str = "debug";
const LITERAL_INPUT: &str = "c";
const EXTENSION_LAMBDAS: &str = "Xlambdas";

fn get_input(matches: &clap::ArgMatches) -> Option<input::Input> {
    if let Some(literal_input) = matches.value_of(LITERAL_INPUT) {
        return Some(input::Input {
            source: input::Source::Literal,
            content: literal_input.to_string(),
        });
    }
    if let Some(input_file) = matches.value_of(INPUT_STR) {
        match fs::read_to_string(input_file) {
            Ok(input) => {
                return Some(input::Input {
                    source: input::Source::File(input_file.to_string()),
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
                .long("--show-tokens")
                .takes_value(false)
                .help("show the token stream"),
        )
        .arg(
            Arg::new(SHOW_AST_STR)
                .long("--show-ast")
                .takes_value(false)
                .help("show the AST"),
        )
        // .arg(
        //     Arg::new(DISASSEMBLE_STR)
        //         .long("--disassemble")
        //         .takes_value(false)
        //         .help("show the bytecode"),
        // )
        .arg(
            Arg::new(DEBUG_STR)
                .long("--debug")
                .takes_value(false)
                .help("run in the debugger"),
        )
        .arg(
            Arg::new(LITERAL_INPUT)
                .long("-c")
                .takes_value(true)
                .help("provide a literal string of marie code"),
        )
        .arg(
            Arg::new(EXTENSION_LAMBDAS)
                .long(&format!["--{}", EXTENSION_LAMBDAS])
                .takes_value(false)
                .help("use the lambdas extension"),
        )
        .get_matches();
    
    let extensions = extensions::Extensions {
        lambdas: matches.is_present(EXTENSION_LAMBDAS),
    };

    if let Some(input) = get_input(&matches) {
        let file_name = if let input::Source::File(file_name) = &input.source {
            Some(file_name.to_string())
        } else {
            None
        };
        let func_or_err = compiler::Compiler::compile(
            input.content.clone(), 
            extensions,
            file_name,
        );
        match func_or_err {
            Ok(func) => {
                // if matches.is_present(DISASSEMBLE_STR) {
                //     println!(
                //         "{}",
                //         bytecode_interpreter::disassemble_chunk(&func.chunk, "")
                //     );
                //     std::process::exit(0);
                // }
                if matches.is_present(DEBUG_STR) {
                    debugger::Debugger::new(func, input.content).debug();
                    std::process::exit(0);
                }
                let mut interp = Interpreter::default();
                let res = interp.interpret(func);
                match res {
                    Ok(()) => {
                        std::process::exit(0);
                    }
                    Err(bytecode_interpreter::InterpreterError::Runtime(err)) => {
                        println!(
                            "Runtime error: {}\n\n{}",
                            err,
                            interp.format_backtrace()
                        );

                        std::process::exit(1);
                    }
                }
            }
            Err(err) => {
                error_formatting::format_compiler_error(&err, &input);
                std::process::exit(1);
            }
        }
    }
}
