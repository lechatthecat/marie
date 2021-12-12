pub mod astnode;
pub mod function;
pub mod calculation;
mod ast_build;
use astnode::AstNode;
use pest::iterators::Pair;
use pest::Parser;
use pest::error::Error;

#[derive(Parser)]
#[grammar = "grammer/oran.pest"]
pub struct OParser;

pub fn parse<'a>(filename: &'a str, source: &'a str) -> Result<Vec<(Box<AstNode>, Pair<'a, Rule>)>, Error<Rule>> {
    let mut ast = vec![];

    let result = OParser::parse(Rule::program, source);
    let pairs = ast_build::get_pairs(filename, result);
    if pairs != None {
        for pair in pairs {
            for inner_pair in pair {
                match inner_pair.as_rule() {
                    Rule::expr | Rule::expr_without_end_mark => {
                        for expr in inner_pair.into_inner() {
                            let pair_for_interpreter = expr.clone();
                            ast.push((Box::new(ast_build::build_ast_from_expr(filename, expr)), pair_for_interpreter));
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    Ok(ast)
}
