#[derive(Clone, Copy, Default)]
pub struct Item(u64);

impl Item {
    pub const KIND_BITS: u64 = 3;
    pub const KIND_SHIFT: u64 = 64 - Self::KIND_BITS;
    pub const KIND_MASK: u64 = 0b111 << Self::KIND_SHIFT;

    pub const VIS_BITS: u64 = 2;
    pub const VIS_SHIFT: u64 = 64 - Self::KIND_BITS - Self::VIS_BITS;
    pub const VIS_MASK: u64 = 0b11 << Self::VIS_SHIFT;

    pub const INDEX_MASK: u64 = !(Self::KIND_MASK | Self::VIS_MASK);

    // Possible kinds

    /// 0b_00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000
    pub const PKG: u64 = 0 << Self::KIND_SHIFT;
    /// 0b_00100000_00000000_00000000_00000000_00000000_00000000_00000000_00000000
    pub const UNIT_STRUCT: u64 = 1 << Self::KIND_SHIFT;
    /// 0b_01000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000
    pub const TUPLE_STRUCT: u64 = 2 << Self::KIND_SHIFT;
    /// 0b_01100000_00000000_00000000_00000000_00000000_00000000_00000000_00000000
    pub const FIELDS_STRUCT: u64 = 3 << Self::KIND_SHIFT;
    /// 0b_10000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000
    pub const CONST: u64 = 4 << Self::KIND_SHIFT;
    /// 0b_10100000_00000000_00000000_00000000_00000000_00000000_00000000_00000000
    pub const STATIC: u64 = 5 << Self::KIND_SHIFT;
    /// 0b_11000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000
    pub const FN: u64 = 6 << Self::KIND_SHIFT;
    /// 0b_11100000_00000000_00000000_00000000_00000000_00000000_00000000_00000000
    pub const LOCAL_VAR: u64 = 7 << Self::KIND_SHIFT;

    // Possible visibilties

    /// 0b_00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000
    pub const DEFAULT: u64 = 0 << Self::VIS_SHIFT;
    /// 0b_00001000_00000000_00000000_00000000_00000000_00000000_00000000_00000000
    pub const PRIVATE: u64 = 1 << Self::VIS_SHIFT;
    /// 0b_00010000_00000000_00000000_00000000_00000000_00000000_00000000_00000000
    pub const PUBLIC: u64 = 2 << Self::VIS_SHIFT;

    // Create a new encoded value for a given kind and index
    pub fn new(kind: u64, visibility: u64, index: usize) -> Self {
        Self(kind | visibility | index as u64)
    }

    // Decode the kind of the item
    pub fn kind(self) -> u64 {
        self.0 & Self::KIND_MASK
    }

    // Decode the visibility of the item
    pub fn visibility(self) -> u64 {
        self.0 & Self::VIS_MASK
    }

    // Decode the index of the item
    pub fn index(self) -> usize {
        (self.0 & Self::INDEX_MASK) as usize
    }
}
