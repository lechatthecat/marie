extern crate plex;

pub mod base {
    use crate::lang::ast::base::*;
    use crate::lang::lexer::base::Token::*;
    use crate::lang::lexer::base::*;
    use plex::parser;
    parser! {
        fn parse_(Token, Span);

        // combine two spans
        (a, b) {
            Span {
                lo: a.lo,
                hi: b.hi,
            }
        }

        program: Program {
            statements[s] => Program { stmts: s }
        }

        statements: Vec<Expr> {
            => vec![],
            statements[mut st] assign[e] Semi => {
                st.push(e);
                st
            }
        }

        assign: Expr {
            Print assign[a] => Expr {
                span: span!(),
                node: Expr_::Print(Box::new(a)),
            },
            Ident(var) Equals assign[rhs] => Expr {
                span: span!(),
                node: Expr_::Assign(var, Box::new(rhs)),
            },
            term[t] => t,
        }

        term: Expr {
            term[lhs] Plus fact[rhs] => Expr {
                span: span!(),
                node: Expr_::Add(Box::new(lhs), Box::new(rhs)),
            },
            term[lhs] Minus fact[rhs] => Expr {
                span: span!(),
                node: Expr_::Sub(Box::new(lhs), Box::new(rhs)),
            },
            fact[x] => x
        }

        fact: Expr {
            fact[lhs] Star atom[rhs] => Expr {
                span: span!(),
                node: Expr_::Mul(Box::new(lhs), Box::new(rhs)),
            },
            fact[lhs] Slash atom[rhs] => Expr {
                span: span!(),
                node: Expr_::Div(Box::new(lhs), Box::new(rhs)),
            },
            atom[x] => x
        }

        atom: Expr {
            // round brackets to destructure tokens
            Ident(i) => Expr {
                span: span!(),
                node: Expr_::Var(i),
            },
            Integer(i) => Expr {
                span: span!(),
                node: Expr_::Literal(i),
            },
            LParen assign[a] RParen => a
        }
    }

    pub fn parse<I: Iterator<Item = (Token, Span)>>(
        i: I,
    ) -> Result<Program, (Option<(Token, Span)>, &'static str)> {
        parse_(i)
    }
}
