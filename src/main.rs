#![feature(proc_macro_hygiene)]
use std::io::Read;
mod lang;

//crtl + D
fn main() {
    let mut s = String::new();
    std::io::stdin().read_to_string(&mut s).unwrap();
    let lexer = lang::lexer::base::Lexer::new(&s).inspect(|tok| eprintln!("tok: {:?}", tok));
    let program = lang::parser::base::parse(lexer).unwrap();
    lang::interp::base::interp(&program);
}
