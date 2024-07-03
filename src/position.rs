use crate::parser::ast::identifiers::IdentifierLookup;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Pos {
    line: u32,
}

impl Pos {
    pub fn new(line: u32) -> Self {
        Self { line }
    }
    pub fn line_add(&mut self, change: u32) {
        self.line += change;
    }
}

impl std::fmt::Display for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:04}", self.line)
    }
}

#[derive(Copy, Clone)]
pub struct PosRange {
    start: Pos,
    end: Pos,
}

impl PosRange {
    pub fn new(start: Pos, end: Pos) -> Self {
        Self { start, end }
    }
}

pub trait Position {
    fn left_pos(&self, lookup: &IdentifierLookup) -> Pos;
    fn right_pos(&self, lookup: &IdentifierLookup) -> Pos;
    fn pos_range(&self, lookup: &IdentifierLookup) -> PosRange {
        PosRange::new(self.left_pos(lookup), self.right_pos(lookup))
    }
}
