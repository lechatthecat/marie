extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate clap;
extern crate num_traits;
mod interpreter;
mod parser;
mod value;
mod error;
use value::scope::ROOT_SCOPE;
use clap::{Arg, Command};
use std::{fs, process, panic};
use std::time::Instant;

fn main() {
    use std::collections::HashMap;
    let matches = Command::new("oran")
    .version("0.1.0")
    .author("shu nakanishi <shu845@gmail.com>")
    .about("A scripting language made by rust.")
    .arg(Arg::new("file")
         .short('f')
         .long("file")
         .value_name("FILE")
         .help("Sets a oran file to parse")
         .required(true)
         .takes_value(true))
    .arg(Arg::new("time")
         .short('t')
         .long("time")
         .value_name("TIME")
         .help("Print the execution time")
         .required(false)
         .takes_value(false))
    .get_matches();
    let start = Instant::now();
    let file = matches.value_of("file");
    // TODO: show error message without panicking
    let string_in_file = fs::read_to_string(&file.unwrap()).unwrap_or_else(|_|
        {
            println!("Unable to read the specified oran file by -f command: {}",
            &file.unwrap());
            process::exit(1);
        });
    //println!("---{:?}---", ast);
    let mut oran_env = HashMap::new();
    for reduced_expr in &parser::parse(&file.unwrap(),&string_in_file).unwrap_or_else(|e| panic!("{}", e)) {
        let result = interpreter::interp_expr(ROOT_SCOPE, &mut oran_env, &*reduced_expr.0);
        match result {
            Ok(_) => {},
            Err(err) => {
                match err.pair {
                    Some(p) => {
                        error::show_error_message(err.message.to_owned(), &file.unwrap(), p);
                        break;
                    },
                    None => {
                        error::show_error_message(err.message.to_owned(), &file.unwrap(), &reduced_expr.1);
                        break;
                    }
                }
            }
        }
    }
    if matches.is_present("time") {
        let execution_time = Instant::now().duration_since(start);
        println!("{:?}", execution_time);
    }

}
