use derive_more::{From, Into};
pub use item::Item;
use nazmc_data_pool::{
    new_data_pool_key, typed_index_collections::TiVec, DataPoolBuilder, IdKey, StrKey,
};
use nazmc_diagnostics::span::{Span, SpanCursor};
use std::collections::HashMap;
use thin_vec::ThinVec;
mod item;

new_data_pool_key! { FileKey }

new_data_pool_key! { PkgKey }

pub type PkgPoolBuilder = DataPoolBuilder<PkgKey, ThinVec<IdKey>>;

#[derive(Default)]
pub struct AST {
    /// The list of maps of items names and their kind, visibility and index
    pub pkgs_to_items: TiVec<PkgKey, HashMap<IdKey, Item>>,
    /// The list of imports stms for each file
    pub imports: TiVec<FileKey, ThinVec<ImportStm>>,
    /// The list of star imports for each file
    pub star_imports: TiVec<FileKey, ThinVec<PkgPath>>,
    /// The list of all types paths
    pub types_paths: ThinVec<ItemPath>,
    /// The list of all unit struct expressions paths
    pub unit_structs_paths_exprs: ThinVec<ItemPath>,
    /// The list of all tuple struct expressions paths
    pub tuple_structs_paths_exprs: ThinVec<ItemPath>,
    /// The list of all fields struct expressions paths
    pub field_structs_paths_exprs: ThinVec<ItemPath>,
    /// The list of all paths expressions
    pub paths_exprs: ThinVec<ItemPath>,
    /// All unit structs
    pub unit_structs: ThinVec<UnitStruct>,
    /// All tuple structs
    pub tuple_structs: ThinVec<TupleStruct>,
    /// All fields structs
    pub fields_structs: ThinVec<FieldsStruct>,
    /// All fns
    pub fns: ThinVec<Fn>,
}

#[derive(Clone)]
pub struct PkgPath {
    /// The pkg idx where this path is located
    pub pkg_key: PkgKey,
    /// The file idx where this path is located
    pub file_key: FileKey,
    /// The segmentes of the path
    pub ids: ThinVec<IdKey>,
    /// The spans of the segments of the path
    pub spans: ThinVec<Span>,
}

#[derive(Clone)]
pub struct ItemPath {
    pub pkg_path: PkgPath,
    pub item: ASTId,
}

pub struct ImportStm {
    pub item_path: ItemPath,
    pub alias: ASTId,
}

#[derive(Clone, Copy)]
pub struct ASTId {
    pub span: Span,
    pub id: IdKey,
}

#[derive(Clone)]
pub struct Binding {
    pub kind: BindingKind,
    pub typ: Option<Type>,
}

#[derive(Clone)]
pub enum BindingKind {
    Id(ASTId),
    MutId { id: ASTId, mut_span: Span },
    Tuple(ThinVec<BindingKind>, Span),
}

#[derive(Clone)]
pub enum Type {
    /// Holds the ItemPath index in `AST::types_paths`
    Path(usize),
    Unit(Span),
    Tuple(ThinVec<Type>, Span),
    Paren(Box<Type>, Span),
    Slice(Box<Type>, Span),
    Array(Box<Type>, Box<Expr>, Span),
    Ptr(Box<Type>, Span),
    Ref(Box<Type>, Span),
    PtrMut(Box<Type>, Span),
    RefMut(Box<Type>, Span),
    Lambda(ThinVec<Type>, Box<Type>, Span),
}

#[derive(Clone, Copy)]
pub enum VisModifier {
    Default,
    Public,
    Private,
}

#[derive(Clone, Copy)]
pub struct ItemInfo {
    pub file_key: FileKey,
    pub id_span: Span,
}

#[derive(Clone)]
pub struct UnitStruct {
    pub info: ItemInfo,
}

#[derive(Clone)]
pub struct TupleStruct {
    pub info: ItemInfo,
    pub types: ThinVec<(VisModifier, Type)>,
}

#[derive(Clone)]
pub struct FieldsStruct {
    pub info: ItemInfo,
    pub fields: ThinVec<(VisModifier, ASTId, Type)>,
}

#[derive(Clone)]
pub struct Fn {
    pub info: ItemInfo,
    pub params: ThinVec<(ASTId, Type)>,
    pub return_type: Type,
    pub body: Scope,
}

#[derive(Clone)]
pub struct Scope {
    pub stms: ThinVec<Stm>,
    pub return_expr: Option<Expr>,
}

#[derive(Clone)]
pub enum Stm {
    Let(Box<LetStm>),
    While(Box<(Expr, Scope)>),
    If(Box<IfExpr>),
    Expr(Box<Expr>),
}

#[derive(Clone)]
pub struct LetStm {
    pub binding: Binding,
    pub assign: Option<Box<Expr>>,
}

#[derive(Clone)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Clone)]
pub enum ExprKind {
    Literal(LiteralExpr),
    Parens(Box<Expr>),
    /// Holds the ItemPath index
    Path(usize),
    Call(Box<CallExpr>),
    /// Holds the ItemPath index
    UnitStruct(usize),
    TupleStruct(Box<TupleStructExpr>),
    FieldsStruct(Box<FieldsStructExpr>),
    Field(Box<FieldExpr>),
    Idx(Box<IdxExpr>),
    TupleIdx(Box<TupleIdxExpr>),
    Tuple(ThinVec<Expr>),
    ArrayElemnts(ThinVec<Expr>),
    ArrayElemntsSized(Box<ArrayElementsSizedExpr>),
    If(Box<IfExpr>),
    Lambda(Box<LambdaExpr>),
    UnaryOp(Box<UnaryOpExpr>),
    BinaryOp(Box<BinaryOpExpr>),
    Return(Option<Box<Expr>>),
    Break(Option<Box<Expr>>),
    Continue,
    On,
}

#[derive(Clone, Copy)]
pub enum LiteralExpr {
    Str(StrKey),
    Char(char),
    Bool(bool),
    Num(NumKind),
}

#[derive(Clone, Copy)]
pub enum NumKind {
    F4(f32),
    F8(f64),
    I(isize),
    I1(i8),
    I2(i16),
    I4(i32),
    I8(i64),
    U(usize),
    U1(u8),
    U2(u16),
    U4(u32),
    U8(u64),
    UnspecifiedInt(u64),
    UnspecifiedFloat(f64),
}

#[derive(Clone)]
pub struct CallExpr {
    pub on: Expr,
    pub args: ThinVec<Expr>,
    pub parens_span: Span,
}

#[derive(Clone)]
pub struct TupleStructExpr {
    pub item_path_idx: usize,
    pub args: ThinVec<Expr>,
}

#[derive(Clone)]
pub struct FieldsStructExpr {
    pub item_path_idx: usize,
    pub fields: ThinVec<(ASTId, Expr)>,
}

#[derive(Clone)]
pub struct FieldExpr {
    pub on: Expr,
    pub name: ASTId,
}

#[derive(Clone)]
pub struct TupleIdxExpr {
    pub on: Expr,
    pub idx: usize,
    pub idx_span: Span,
}

#[derive(Clone)]
pub struct IdxExpr {
    pub on: Expr,
    pub idx: Expr,
    pub brackets_span: Span,
}

#[derive(Clone)]
pub struct ArrayElementsSizedExpr {
    pub repeat: Expr,
    pub size: Expr,
}

#[derive(Clone)]
pub struct IfExpr {
    pub if_: (Expr, Scope),
    pub else_ifs: ThinVec<(Expr, Scope)>,
    pub else_: Option<Box<Scope>>,
}

#[derive(Clone)]
pub struct LambdaExpr {
    pub params: ThinVec<Binding>,
    pub body: Scope,
}

#[derive(Clone)]
pub struct UnaryOpExpr {
    pub op: UnaryOp,
    pub op_span: Span,
    pub expr: Expr,
}

#[derive(Clone)]
pub enum UnaryOp {
    Minus,
    LNot,
    BNot,
    Deref,
    Borrow,
    BorrowMut,
}

#[derive(Clone)]
pub struct BinaryOpExpr {
    pub op: BinOp,
    pub op_span_cursor: SpanCursor,
    pub left: Expr,
    pub right: Expr,
}

#[derive(Clone)]
pub enum BinOp {
    LOr,
    LAnd,
    EqualEqual,
    NotEqual,
    GE,
    GT,
    LE,
    LT,
    OpenOpenRange,
    CloseOpenRange,
    OpenCloseRange,
    CloseCloseRange,
    BOr,
    Xor,
    BAnd,
    Shr,
    Shl,
    Plus,
    Minus,
    Times,
    Div,
    Mod,
    Assign,
    PlusAssign,
    MinusAssign,
    TimesAssign,
    DivAssign,
    ModAssign,
    BAndAssign,
    BOrAssign,
    XorAssign,
    ShlAssign,
    ShrAssign,
}
