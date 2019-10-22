extern crate plex;

pub mod base {
    use crate::lang::ast::base::*;
    use std::collections::HashMap;

    pub fn interp<'a>(p: &'a Program) {
        let mut env = HashMap::new();
        for expr in &p.stmts {
            interp_expr(&mut env, expr);
        }
    }
    fn interp_expr<'a>(env: &mut HashMap<&'a str, i64>, expr: &'a Expr) -> i64 {
        use crate::lang::ast::base::Expr_::*;
        match expr.node {
            Add(ref a, ref b) => interp_expr(env, a) + interp_expr(env, b),
            Sub(ref a, ref b) => interp_expr(env, a) - interp_expr(env, b),
            Mul(ref a, ref b) => interp_expr(env, a) * interp_expr(env, b),
            Div(ref a, ref b) => interp_expr(env, a) / interp_expr(env, b),
            Assign(ref var, ref b) => {
                let val = interp_expr(env, b);
                env.insert(var, val);
                val
            }
            Var(ref var) => *env.get(&var[..]).unwrap(),
            Literal(lit) => lit,
            Print(ref e) => {
                let val = interp_expr(env, e);
                println!("{}", val);
                val
            }
        }
    }
}