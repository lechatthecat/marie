use std::{collections::HashMap, fmt, ops::{Deref, DerefMut}};

use smallvec::SmallVec;

use crate::gc::gc;

/// specialised JIT = 引数タグ列ごとに別ポインタ
#[derive(Clone, Debug, Default)]
pub struct JitCache {
    map: HashMap<(gc::HeapId, TagVec), *const u8>,
    hot: HashMap<(gc::HeapId, TagVec), usize>,   // optional hot-counter
}

impl JitCache {
    pub fn get(&self, h: gc::HeapId, tv: &TagVec) -> Option<*const u8> {
        self.map.get(&(h, tv.clone())).copied()
    }
    pub fn insert(&mut self, h: gc::HeapId, tv: TagVec, ptr: *const u8) {
        self.map.insert((h, tv), ptr);
    }
    pub fn inc_hot(&mut self, h: gc::HeapId, tv: &TagVec) -> usize {
        let c = self.hot.entry((h, tv.clone())).or_insert(0);
        *c += 1;
        *c
    }
}

/// 自クレート固有の新型 ― 孤児規則を回避
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct TagVec(SmallVec<[i64; 4]>);

/// == 便利のために Deref を生やす ==
impl Deref for TagVec {
    type Target = SmallVec<[i64; 4]>;
    fn deref(&self) -> &Self::Target { &self.0 }
}
impl DerefMut for TagVec {
    fn deref_mut(&mut self) -> &mut Self::Target { &mut self.0 }
}

/// From 変換
impl From<SmallVec<[i64; 4]>> for TagVec {
    fn from(v: SmallVec<[i64; 4]>) -> Self { TagVec(v) }
}
impl From<&Vec<i64>> for TagVec {
    fn from(v: &Vec<i64>) -> Self { TagVec(v.iter().copied().collect()) }
}

/// Display 実装は **この新型** に対して行う
impl fmt::Display for TagVec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, b) in self.0.iter().enumerate() {
            if i != 0 { write!(f, " ")?; }
            write!(f, "{:02X}", b)?;
        }
        Ok(())
    }
}

/* ───────── 1. FromIterator<u8> ───────── */
impl FromIterator<i64> for TagVec {
    fn from_iter<I: IntoIterator<Item = i64>>(iter: I) -> Self {
        TagVec(SmallVec::<[i64; 4]>::from_iter(iter))
    }
}
