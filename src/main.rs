extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate clap;
use clap::{Arg, App, SubCommand};
use std::fs;
use std::path::Path;
mod lang;

fn main() {
    use std::collections::HashMap;
    let matches = App::new("oran")
    .version("0.0.1")
    .author("lechatthecat <shu845@gmail.com>")
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
    let ast = lang::parser::parse(&string_in_file).expect("unsuccessful parse");
    println!("---{:?}---", ast);
    let mut oran_env = HashMap::new();
    for reduced_expr in &ast {
        match reduced_expr {
            _ => interp_expr(&mut oran_env, reduced_expr)
        };
    }

    fn interp_expr<'a>(env: &mut HashMap<&'a str, lang::oran_value::OranValue>, reduced_expr: &'a lang::astnode::AstNode) -> lang::oran_value::OranValue {
        use lang::astnode::AstNode;
        use lang::astnode::CalcOp;
        use lang::astnode::DefaultFunction;
        use lang::oran_value::OranValue;

        match reduced_expr {
            AstNode::Number(double) => OranValue::Float(*double),
            AstNode::Ident(ref var) => {
                let val = &*env.get(&var[..]).unwrap();
                val.clone()
            },
            AstNode::Assign(ref var_type, ref ident, ref expr) => {
                let val = &interp_expr(env, expr);
                env.insert(ident, val.clone());
                val.clone()
            }
            AstNode::Calc (ref verb, ref lhs, ref rhs ) => {
                match verb {
                    CalcOp::Plus => { interp_expr(env, lhs) + interp_expr(env, rhs) }
                    CalcOp::Minus => { interp_expr(env, lhs) - interp_expr(env, rhs) }
                    CalcOp::Times => { interp_expr(env, lhs) * interp_expr(env, rhs) }
                    CalcOp::Divide => { interp_expr(env, lhs) / interp_expr(env, rhs) }
                    CalcOp::Modulus => { interp_expr(env, lhs) % interp_expr(env, rhs) }
                }
            },
            AstNode::FunctionCall(ref func, ref e) => {
                match func {
                    DefaultFunction::Print => {
                        let val = interp_expr(env, e);
                        println!("{}", val);
                        val
                    },
                }
            },
            AstNode::Str (str) => {
                OranValue::Str(str.to_string())
            }
            AstNode::Strs (strs) => {
                let mut text = "".to_owned();
                for str in strs {
                    text.push_str(&String::from(interp_expr(env, &str)))
                }
                OranValue::Str(text)
            }
            _ => {
                OranValue::Boolean(true)
            },
        }
    }
}

