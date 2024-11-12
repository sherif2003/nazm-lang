use derive_more::{From, Into};
pub use item::*;
use nazmc_data_pool::{
    new_data_pool_key,
    typed_index_collections::{ti_vec, TiVec},
    DataPoolBuilder, IdKey, StrKey,
};
use nazmc_diagnostics::span::{Span, SpanCursor};
use std::collections::HashMap;
use thin_vec::ThinVec;
mod item;

new_data_pool_key! { FileKey }
new_data_pool_key! { PkgKey }
new_data_pool_key! { TypePathKey }
new_data_pool_key! { UnitStructPathKey }
new_data_pool_key! { TupleStructPathKey }
new_data_pool_key! { FieldsStructPathKey }
new_data_pool_key! { PathNoPkgKey }
new_data_pool_key! { PathWithPkgKey }

new_data_pool_key! { UnitStructKey }
new_data_pool_key! { TupleStructKey }
new_data_pool_key! { FieldsStructKey }
new_data_pool_key! { ConstKey }
new_data_pool_key! { StaticKey }
new_data_pool_key! { FnKey }
new_data_pool_key! { ScopeKey }
new_data_pool_key! { LetStmKey }

pub type PkgPoolBuilder = DataPoolBuilder<PkgKey, ThinVec<IdKey>>;

pub const TOP_PKG_KEY: PkgKey = PkgKey(0);

#[derive(Default)]
pub struct Unresolved {
    /// The list of maps of items names and their kind, visibility and index
    pub pkgs_to_items: TiVec<PkgKey, HashMap<IdKey, Item>>,
    /// All paths that should be resolved
    pub paths: ASTPaths,
}

/// Holds resolved paths
pub struct Resolved {
    /// The list of all types paths
    pub types_paths: TiVec<TypePathKey, Item>,
    /// The list of all unit struct expressions paths
    pub unit_structs_paths_exprs: TiVec<UnitStructPathKey, UnitStructKey>,
    /// The list of all tuple struct expressions paths
    pub tuple_structs_paths_exprs: TiVec<TupleStructPathKey, TupleStructKey>,
    /// The list of all fields struct expressions paths
    pub field_structs_paths_exprs: TiVec<FieldsStructPathKey, FieldsStructKey>,
    /// The list of all paths expressions that have no leading pkgs paths
    pub paths_no_pkgs_exprs: TiVec<PathNoPkgKey, Item>,
    /// The list of all paths expressions that have leading pkgs paths
    pub paths_with_pkgs_exprs: TiVec<PathWithPkgKey, Item>,
}

#[derive(Default)]
pub struct AST<S> {
    /// The state of AST: may be `Unresolved` or `Resolved`
    pub state: S,
    /// All consts
    pub consts: TiVec<ConstKey, Const>,
    /// All statics
    pub statics: TiVec<StaticKey, Static>,
    /// All unit structs
    pub unit_structs: TiVec<UnitStructKey, UnitStruct>,
    /// All tuple structs
    pub tuple_structs: TiVec<TupleStructKey, TupleStruct>,
    /// All fields structs
    pub fields_structs: TiVec<FieldsStructKey, FieldsStruct>,
    /// All fns
    pub fns: TiVec<FnKey, Fn>,
    /// All fns scopse
    pub fns_scopes: TiVec<FnKey, ScopeKey>,
    /// All scopes
    pub scopes: TiVec<ScopeKey, Scope>,
    /// All let stms
    pub lets: TiVec<LetStmKey, LetStm>,
}

impl AST<Unresolved> {
    pub fn new(pkgs_len: usize, files_len: usize) -> Self {
        let state = Unresolved {
            pkgs_to_items: ti_vec![HashMap::new(); pkgs_len],
            paths: ASTPaths {
                imports: ti_vec![ThinVec::new(); files_len],
                star_imports: ti_vec![ThinVec::new(); files_len],
                ..Default::default()
            },
        };

        Self {
            state,
            ..Default::default()
        }
    }
}

#[derive(Default)]
pub struct ASTPaths {
    /// The list of imports stms for each file
    pub imports: TiVec<FileKey, ThinVec<ImportStm>>,
    /// The list of star imports for each file
    pub star_imports: TiVec<FileKey, ThinVec<StarImportStm>>,
    /// The list of all types paths
    pub types_paths: TiVec<TypePathKey, ItemPath>,
    /// The list of all unit struct expressions paths
    pub unit_structs_paths_exprs: TiVec<UnitStructPathKey, ItemPath>,
    /// The list of all tuple struct expressions paths
    pub tuple_structs_paths_exprs: TiVec<TupleStructPathKey, ItemPath>,
    /// The list of all fields struct expressions paths
    pub field_structs_paths_exprs: TiVec<FieldsStructPathKey, ItemPath>,
    /// The list of all paths expressions that have no leading pkgs paths. The PkgKey here represent where this path is loctaed
    pub paths_no_pkgs_exprs: TiVec<PathNoPkgKey, (ASTId, PkgKey)>,
    /// The list of all paths expressions that have leading pkgs paths
    pub paths_with_pkgs_exprs: TiVec<PathWithPkgKey, ItemPath>,
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
    pub top_pkg_span: Option<Span>,
    pub pkg_path: PkgPath,
    pub item: ASTId,
}

#[derive(Clone)]
pub struct StarImportStm {
    pub top_pkg_span: Option<Span>,
    pub pkg_path: PkgPath,
}

#[derive(Clone)]
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

pub fn expand_names_binding<'b>(kind: &'b BindingKind, bound_names: &mut Vec<&'b ASTId>) {
    match kind {
        BindingKind::Id(id) => {
            bound_names.push(id);
        }
        BindingKind::MutId { id, .. } => {
            bound_names.push(id);
        }
        BindingKind::Tuple(bindings, ..) => {
            for binding_kind in bindings {
                expand_names_binding(binding_kind, bound_names);
            }
        }
    }
}

pub fn expand_names_binding_owned(kind: &BindingKind, bound_names: &mut Vec<IdKey>) {
    match kind {
        BindingKind::Id(ast_id) => {
            bound_names.push(ast_id.id);
        }
        BindingKind::MutId { id, .. } => {
            bound_names.push(id.id);
        }
        BindingKind::Tuple(bindings, ..) => {
            for binding_kind in bindings {
                expand_names_binding_owned(binding_kind, bound_names);
            }
        }
    }
}

#[derive(Clone)]
pub enum Type {
    Path(TypePathKey),
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

#[derive(Clone, Copy, Default)]
pub enum VisModifier {
    #[default]
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
pub struct Const {
    pub info: ItemInfo,
    pub typ: Type,
    pub expr: Expr,
}

#[derive(Clone)]
pub struct Static {
    pub info: ItemInfo,
    pub typ: Type,
    pub expr: Expr,
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
    pub fields: HashMap<IdKey, FieldInfo>,
}

#[derive(Clone)]
pub struct FieldInfo {
    pub vis: VisModifier,
    pub id_span: Span,
    pub typ: Type,
}

#[derive(Clone)]
pub struct Fn {
    pub info: ItemInfo,
    pub params: ThinVec<(ASTId, Type)>,
    pub return_type: Type,
}

#[derive(Clone, Default)]
pub struct Scope {
    pub extra_params: Vec<IdKey>,
    pub events: ThinVec<ScopeEvent>,
    pub stms: ThinVec<Stm>,
    pub return_expr: Option<Expr>,
}

#[derive(Clone)]
pub enum ScopeEvent {
    Let(LetStmKey),
    Path(PathNoPkgKey),
    Scope(ScopeKey),
}

#[derive(Clone)]
pub enum Stm {
    Let(LetStmKey),
    While(Box<(Expr, ScopeKey)>),
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
    PathNoPkg(PathNoPkgKey),
    PathInPkg(PathWithPkgKey),
    Call(Box<CallExpr>),
    UnitStruct(UnitStructPathKey),
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
    pub path_key: TupleStructPathKey,
    pub args: ThinVec<Expr>,
}

#[derive(Clone)]
pub struct FieldsStructExpr {
    pub path_key: FieldsStructPathKey,
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
    pub if_: (Expr, ScopeKey),
    pub else_ifs: ThinVec<(Expr, ScopeKey)>,
    pub else_: Option<ScopeKey>,
}

#[derive(Clone)]
pub struct LambdaExpr {
    pub params: ThinVec<Binding>,
    pub body: ScopeKey,
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
