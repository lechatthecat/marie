extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate clap;
use clap::{Arg, App, SubCommand};
use std::fs;
use std::path::Path;
mod lang;
use lang::{parser, astnode, oran_value::OranValue, oran_variable::OranVariable, oran_variable::OranVariableValue};

fn main() {
    use std::collections::HashMap;
    let matches = App::new("oran")
    .version("0.0.1")
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
        match reduced_expr {
            _ => interp_expr(&mut oran_env, reduced_expr)
        };
    }

    fn interp_expr<'a>(env: &mut HashMap<&'a str, OranValue>, reduced_expr: &'a astnode::AstNode) -> OranValue {
        use astnode::AstNode;
        use astnode::CalcOp;
        use astnode::DefaultFunction;

        match reduced_expr {
            AstNode::Number(double) => OranValue::Float(*double),
            AstNode::Ident(ref var) => {
                let val = &*env.get(&var[..]).unwrap();
                val.clone()
            },
            AstNode::Assign(ref is_const, ref ident, ref expr) => {
                if env.contains_key(&ident[..]) {
                    match env.get(&ident[..]).unwrap() {
                        OranValue::Variable(ref v) => { 
                            if v.is_const {
                                panic!("You can't assign value twice to a constant variable.")
                            }
                        }
                        _ => {}
                    }
                }
                let val = &interp_expr(env, expr);
                let oran_val = OranValue::Variable(OranVariable {
                    is_const: *is_const,
                    name: ident.to_owned(),
                    value: OranVariableValue::from(val.to_owned()),
                });
                env.insert(ident, oran_val.clone());
                oran_val.clone()
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
        }
    }
}

