pub mod astnode;
pub mod function;
pub mod calculation;
mod ast_build;
use astnode::AstNode;
use pest::Parser;
use pest::error::Error;

#[derive(Parser)]
#[grammar = "grammer/oran.pest"]
pub struct OParser;

pub fn parse(filename: &str, source: &str) -> Result<Vec<Box<AstNode>>, Error<Rule>> {
    let mut ast = vec![];

    let result = OParser::parse(Rule::program, source);
    let pairs = ast_build::get_pairs(filename.to_string(), result);
    if pairs != None {
        for pair in pairs {
            for inner_pair in pair {
                match inner_pair.as_rule() {
                    Rule::expr | Rule::expr_without_end_mark => {
                        for expr in inner_pair.into_inner() {
                            let span = expr.as_span();
                            let location = span.start_pos().line_col();
                            let location = (filename.to_owned(), location.0, location.1);
                            ast.push(Box::new(ast_build::build_ast_from_expr(location, expr)));
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    Ok(ast)
}
