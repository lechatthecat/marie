use pest::error::{Error, ErrorVariant};
use pest::iterators::Pair;
use crate::parser::Rule;

pub fn show_error_message<'a>(message: String, filename: &'a str, pair: &'a Pair<'a, Rule>) {
    let error: Error<Rule> = Error::new_from_span(
        ErrorVariant::CustomError{
            message: message
        },
        pair.as_span()
    ).with_path(filename);
    println!("Error!{}", error);
}