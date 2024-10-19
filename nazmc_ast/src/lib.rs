use exprs::{Expr, Exprs};
use nazmc_data_pool::PoolIdx;
use nazmc_diagnostics::span::Span;
use stms::{Stm, Stms};
use thin_vec::ThinVec;
use types::{Type, Types};

mod exprs;
mod stms;
mod types;

pub struct AST {
    pub imports: ThinVec<ModPathWithItem>,
    pub star_imports: ThinVec<ModPath>,
    pub types: Types,
    pub unit_structs: ThinVec<UnitStruct>,
    pub tuple_structs: ThinVec<TupleStruct>,
    pub fields_structs: ThinVec<FieldsStruct>,
    pub fns: ThinVec<Fn>,
    pub scopes: ThinVec<ScopeBody>,
    pub stms: Stms,
    pub exprs: Exprs,
}

pub struct ModPath {
    pub ids: ThinVec<PoolIdx>,
    pub spans: ThinVec<Span>,
}

pub struct ModPathWithItem {
    pub mod_path: ModPath,
    pub item: ASTId,
}

pub struct ASTId {
    pub span: Span,
    pub id: PoolIdx,
}

pub enum VisModifier {
    Default,
    Public,
    Private,
}

pub struct UnitStruct {
    pub vis: VisModifier,
    pub name: ASTId,
}

pub struct TupleStruct {
    pub vis: VisModifier,
    pub name: ASTId,
    pub types: ThinVec<(VisModifier, Type)>,
}

pub struct FieldsStruct {
    pub vis: VisModifier,
    pub name: ASTId,
    pub fields: ThinVec<(VisModifier, ASTId, Type)>,
}

pub struct Fn {
    pub vis: VisModifier,
    pub name: ASTId,
    pub params: ThinVec<(ASTId, Type)>,
    pub return_type: Type,
    pub body: Scope,
}

pub struct ScopeBody {
    pub stms: ThinVec<Stm>,
    pub return_expr: Option<Expr>,
}

pub struct Scope {
    pub index: usize,
}

pub struct ConditionalScope {
    pub condition: Expr,
    pub scope: Scope,
}
