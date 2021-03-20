
use pest::iterators::Pair;
use super::astnode::AstNode;
use super::{Rule, ast_build};

pub fn parse_arguments(arguments: Pair<Rule>) -> Vec<AstNode> {
    let mut args: Vec<AstNode> = Vec::new();

    for arg in arguments.into_inner() {
        args.push(ast_build::build_ast_from_expr(arg));
    }

    args
}

pub fn function_call (location: (usize, usize), fn_name: Pair<'_, Rule>, arg_values: Vec<AstNode>) -> AstNode {
    AstNode::FunctionCall(location, fn_name.as_str().to_string(), arg_values)
}
