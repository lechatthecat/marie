extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate clap;
use clap::{Arg, App};
use std::fs;
mod lang;
use lang::{interpreter, parser, constant::SCOPE_MAIN_FUNCTION};


fn main() {
    use std::collections::HashMap;
    let matches = App::new("oran")
    .version("0.1.0")
    .author("shu nakanishi <shu845@gmail.com>")
    .about("A scripting language made by rust.")
    .arg(Arg::with_name("file")
         .short("f")
         .long("file")
         .value_name("FILE")
         .help("Sets a oran file to parse.")
         .takes_value(true))
    .get_matches();

    let file = matches.value_of("file");
    let string_in_file = fs::read_to_string(&file.unwrap()).expect("Unable to read file");
    let ast = parser::parse(&string_in_file).unwrap_or_else(|e| panic!("{}", e));
    //println!("---{:?}---", ast);
    let mut oran_env = HashMap::new();
    for reduced_expr in &ast {
        interpreter::interp_expr(SCOPE_MAIN_FUNCTION, &mut oran_env, reduced_expr);
    }
}

