extern crate clap;
extern crate ctrlc;

use clap::{App, Arg};

use std::fs;

mod bytecode;
mod compiler;
mod error_formatting;
mod expr;
mod extensions;
mod input;
mod line_reader;
mod parser;
mod scanner;
mod treewalk_interpreter;

const INPUT_STR: &str = "INPUT";
const TREEWALK_STR: &str = "treewalk";

fn get_input(matches: &clap::ArgMatches<'_>) -> Option<input::Input> {
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
    let matches = App::new("loxi")
        .version("0.1.0")
        .about("marie language compiler")
        .author("Thomas Peters")
        .arg(
            Arg::with_name(INPUT_STR)
                .help("sets input file to use")
                .required(false)
                .index(1),
        )
        .arg(
            Arg::with_name(TREEWALK_STR)
                .long("--treewalk")
                .takes_value(false)
                .help("run the tree-walk interpreter instead of the bytecode interpreter"),
        )
        .get_matches();

    if let Some(input) = get_input(&matches) {
        match scanner::scan_tokens(input.content.clone()) {
            Ok(tokens) => {

                let stmts_maybe = parser::parse(tokens);

                match stmts_maybe {
                    Ok(stmts) => {
                        let mut interpreter: treewalk_interpreter::Interpreter =
                            Default::default();
                        let interpret_result = interpreter.interpret(&stmts);

                        match interpret_result {
                            Ok(_) => {
                                std::process::exit(0);
                            }
                            Err(err) => {
                                println!(
                                    "Runtime Error: {}\n\n{}",
                                    err,
                                    interpreter.format_backtrace()
                                );
                                std::process::exit(-1);
                            }
                        }
                    }
                    Err(err) => {
                        error_formatting::format_parse_error(&err, &input);
                        std::process::exit(-1)
                    }
                }
            }
            Err(err) => {
                error_formatting::format_lexical_error(&err, &input);
                std::process::exit(-1);
            }
        }
    }
}
