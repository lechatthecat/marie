extern crate pest;
#[macro_use]
extern crate pest_derive;
use std::env;

mod lang;

fn main() {
    use std::collections::HashMap;
    let s = "
    //4*2+4+5*2
    let test1 = 4*2+4+5*2
    let test2 = 4/2+1
    const test = 0
    let test3 = test1 + test2 + 1
    print((test1 + 1) . '+' . test2)
    print('Answer is ' . test3)
    //const test2 = 2
    //test1 + test2
    //const test = 5+5*10
    //let str = 'a' //aaaaaaaaaauhiih dfgtdt
    //str = 'abc' //aaabbbccc
    ";
    let ast = lang::parser::parse(&s).expect("unsuccessful parse");
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

