use std::{cmp::Ordering, ops::{Add, Sub}};

#[derive(Copy, Clone, Eq, Hash, Debug)]
pub struct OranScope {
    pub vertical_scope: usize,
    pub horizontal_scope: usize,
}

impl PartialEq for OranScope {
    fn eq(&self, other: &Self) -> bool {
        self.vertical_scope == other.vertical_scope &&
            self.horizontal_scope == other.horizontal_scope
    }
}

impl Add<usize> for OranScope {
    type Output = Self;

    fn add(self, other: usize) -> OranScope {
        OranScope {
            vertical_scope: self.vertical_scope + other,
            horizontal_scope: 0
        }
    }
}

impl Sub<usize> for OranScope {
    type Output = Self;

    fn sub(self, other: usize) -> OranScope {
        OranScope {
            vertical_scope: self.vertical_scope - other,
            horizontal_scope: 0
        }
    }
}

impl Ord for OranScope {
    fn cmp(&self, other: &Self) -> Ordering {
        self.vertical_scope.cmp(&other.vertical_scope)
    }
}

impl PartialOrd for OranScope {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
