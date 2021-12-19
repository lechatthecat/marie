
use pest::iterators::Pair;
use super::astnode::AstNode;
use super::{Rule, ast_build};

pub fn parse_arguments<'a>(filename: &'a str, arguments: Pair<'a, Rule>) -> Vec<AstNode<'a>> {
    let mut args: Vec<AstNode> = Vec::new();

    for arg in arguments.into_inner() {
        args.push(ast_build::build_ast_from_expr(filename, arg));
    }

    args
}

pub fn function_call<'a> (fn_name: Pair<'a, Rule>, arg_values: Vec<AstNode<'a>>) -> AstNode<'a> {
    AstNode::FunctionCall(fn_name.as_str().to_string(), arg_values)
}
