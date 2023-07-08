extern crate clap;
extern crate ctrlc;

use clap::{App, Arg};
use error::error_formatting;
use interpreter::treewalk_interpreter;
use parser::scanner;
use crate::parser::parser::parse;

use std::{fs, path::Path, io};

mod error;
mod input;
mod line_reader;
mod parser;
mod interpreter;
mod value;

const INPUT_STR: &str = "INPUT";
const PROJECT_PATH: &'static str = env!("CARGO_MANIFEST_DIR");

fn get_input(matches: &clap::ArgMatches) -> Option<input::Input> {
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

fn empty_output_directory() -> io::Result<()> {
    let dir_path = format!("{}/rustcode", PROJECT_PATH);
    let gitignore_file_path = dir_path.to_owned() + "/.gitignore";
    // Iterate over each entry in directory
    for entry in fs::read_dir(dir_path)? {
        let entry = entry?;
        let path = entry.path();

        // Skip the gitignore file
        if path.to_string_lossy() != gitignore_file_path {
            // If the entry is a file (not a directory), delete it
            if path.is_file() {
                fs::remove_file(path)?;
            }
        }
    }
    Ok(())
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
        .get_matches();

    if let Some(input) = get_input(&matches) {
        let full_path = if let input::Source::File(file_name) = &input.source {
            Some(file_name.to_string())
        } else {
            None
        };
        let file_name_only = full_path.and_then(|path| {
            Path::new(&path).file_name()
                .and_then(|name| name.to_str())
                .map(|name| name.to_string())
        }).unwrap();
        match scanner::scan_tokens(input.content.clone()) {
            Ok(tokens) => {

                let stmts_maybe = parse(tokens);
                match empty_output_directory() {
                    Ok(_) => {},
                    Err(err) => println!("Cannot empty the output directory. Error: {}", err)
                }

                match stmts_maybe {
                    Ok(stmts) => {
                        let mut interpreter: treewalk_interpreter::Interpreter = Default::default();
                        let interpret_result = interpreter.interpret(file_name_only, &stmts);
                        if !interpreter.has_main_function() {
                            println!(
                                "Runtime Error: {}",
                                "Please define a main Function.",
                            );
                            std::process::exit(-1);
                        }

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


