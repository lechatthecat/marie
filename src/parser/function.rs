
use pest::iterators::Pair;
use super::astnode::{AstNode, Function};
use super::{Rule, ast_build};

pub fn parse_arguments(arguments: Pair<Rule>) -> Vec<AstNode> {
    let mut args: Vec<AstNode> = Vec::new();

    for arg in arguments.into_inner() {
        args.push(ast_build::build_ast_from_expr(arg));
    }

    args
}

pub fn function_call (fn_name: Pair<'_, Rule>, arg_values: Vec<AstNode>) -> AstNode {
    match fn_name.as_str() {
        "print" => AstNode::FunctionCall(Function::Print, "".to_owned(), arg_values),
        "println" => AstNode::FunctionCall(Function::Println, "".to_owned(), arg_values),
        _ => AstNode::FunctionCall(Function::NotDefault, fn_name.as_str().to_string(), arg_values),
    }
}
