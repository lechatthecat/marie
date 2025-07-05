use clap::{Arg, ArgMatches, Command};
use std::{env, fs, path::{Path, PathBuf}};

mod bytecode;
mod error;
mod extensions;
mod gc;
mod reader;

use reader::compiler;

const INPUT_STR: &str = "INPUT";
const DEBUG_STR: &str = "debug";
const LITERAL_INPUT: &str = "c";

fn get_input(matches: &ArgMatches) -> Option<(PathBuf, reader::input::Input, String)> {
    if let Some(literal_input) = matches.get_one::<String>(LITERAL_INPUT) {
        return Some((
            env::current_dir().expect("Failed to get the file directory"),
            reader::input::Input {
                source: reader::input::Source::Literal,
                content: literal_input.to_string(),
            },
            "".to_string(),
        ));
    }
    if let Some(input_file) = matches.get_one::<String>(INPUT_STR) {
        match fs::read_to_string(input_file) {
            Ok(input) => {
                return Some((
                    Path::new(input_file).parent().expect("Failed to get the file directory").to_path_buf(),
                    reader::input::Input {
                        source: reader::input::Source::File(input_file.clone().to_string()),
                        content: input,
                    },
                    input_file.to_string(),
                ));
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

    let extensions = extensions::Extensions {
        lambdas: false,
    };

    if let Some(input) = get_input(&matches) {
        let func_or_err = compiler::Compiler::compile(
            input.1.content.clone(),
            extensions,
            input.0.clone()
        );

        match func_or_err {
            Ok(mut func) => {
                func.name = input.2;
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
                error::error_formatting::format_compiler_error(&err, &input.1);
                std::process::exit(1);
            }
        }
    }
}
