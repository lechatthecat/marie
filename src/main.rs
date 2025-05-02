use clap::{Arg, ArgMatches, Command as ClapCommand};
use std::process::Command;
use std::{env, fs, path::{Path, PathBuf}, io};

mod bytecode;
mod error;
mod extensions;
mod gc;
mod reader;

use reader::compiler;

const PROJECT_PATH: &'static str = env!("CARGO_MANIFEST_DIR");

fn get_input(matches: &clap::ArgMatches) -> Option<reader::input::Input> {
    if let Some(input_file) = matches.get_one::<String>("input") {
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

fn empty_output_directory() -> io::Result<()> {
    let dir_path = format!("{}/output", PROJECT_PATH);
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

fn init_rustcode() -> io::Result<()> {
    let dir_path = format!("{}/output", PROJECT_PATH);
    let target_dir_path = format!("{}/output/target", PROJECT_PATH);

    let init_output = std::process::Command::new("cargo")
        .current_dir(dir_path.clone())
        .arg("init")
        .arg("--name=marie_compiled") 
        .env("CARGO_TARGET_DIR", target_dir_path) 
        .arg(dir_path.clone())
        .output()?;

    // Handle errors during initialization
    if !init_output.stderr.is_empty() {
        println!("{}", String::from_utf8_lossy(&init_output.stderr));
    }

    let dir_path = format!("{}/output/src", PROJECT_PATH);
    // Iterate over each entry in directory
    for entry in fs::read_dir(dir_path)? {
        let entry = entry?;
        let path = entry.path();

        if path.is_file() {
            fs::remove_file(path)?;
        }
        
    }
    Ok(())
}

fn run_rustcode() -> std::io::Result<()> {
    let dir_path = format!("{}/output", PROJECT_PATH);

    // Build and run the Rust file
    let run_output = Command::new("cargo")
        .arg("run")
        .arg("--quiet")
        .arg("--release")
        .current_dir(dir_path)
        .output()?;

    // Print the output
    if !run_output.stdout.is_empty() {
        println!("{}", String::from_utf8_lossy(&run_output.stdout));
    }

    // Handle errors during execution
    if !run_output.stderr.is_empty() {
        println!("{}", String::from_utf8_lossy(&run_output.stderr));
    }

    Ok(())
}

fn main() {
    let matches = ClapCommand::new("marie")
        .version("0.1.0")
        .about("marie language interpreter")
        .author("lechat thecat")
        .arg(
            Arg::new("input")
                .help("sets input file to use")
                .required(false)
                .index(1),
        )
        .get_matches();


    // TODO RUSTのバージョンの確認
    // TODO rustへのトランスパイルのみ、binaryの実行のみのオプションを追加
    
    if let Some(input) = get_input(&matches) {
        let full_path = if let reader::input::Source::File(file_name) = &input.source {
            Some(file_name.to_string())
        } else {
            None
        };
        let _file_name_only = full_path.and_then(|path| {
            Path::new(&path).file_name()
                .and_then(|name| name.to_str())
                .map(|name| name.to_string())
        }).unwrap();

        match empty_output_directory() {
            Ok(_) => {},
            Err(err) => println!(
                "Cannot empty the output directory. Please check if the output directory exists in the project root. If it does, maybe Permission error or CARGO_MANIFEST_DIR env variable is not set to the project root? Error: {}",
                err
            )
        }

        let func_or_err = reader::compiler::Compiler::compile(input.content.clone());

        match func_or_err {
            Ok(func) => {
                match init_rustcode() {
                    Ok(_) => {},
                    Err(err) => println!("Error: {}", err)
                }
                let mut interpreter = bytecode::bytecode_interpreter::Interpreter::default();
                let res = interpreter.interpret(func);
                match res {
                    Ok(()) => {
                        match run_rustcode() {
                            Ok(_) => {},
                            Err(err) => println!("Error: {}", err)
                        }
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