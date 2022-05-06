use pest::iterators::Pair;
use crate::parser::Rule;

#[derive(Clone, Debug)]
pub struct OranError<'a> {
    pub message: String,
    pub pair: Option<&'a Pair<'a, Rule>>
}

impl PartialEq for OranError<'_> {
    fn eq(&self, other: &OranError) -> bool {
        self.pair == other.pair
    }
}