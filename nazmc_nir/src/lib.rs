pub use exprs::*;
use nazmc_data_pool::IdKey;
use nazmc_diagnostics::span::Span;
pub use stms::*;
pub use thin_vec::ThinVec;
pub use types::*;

mod exprs;
mod stms;
mod types;

#[derive(Default)]
pub struct NIR {
    // pub imports: ThinVec<PkgPathWithItem>,
    // pub star_imports: ThinVec<PkgPath>,
    pub types: Types,
    pub unit_structs: ThinVec<UnitStruct>,
    pub tuple_structs: ThinVec<TupleStruct>,
    pub fields_structs: ThinVec<FieldsStruct>,
    pub fns: ThinVec<Fn>,
    pub scopes: ThinVec<ScopeBody>,
    pub stms: Stms,
    pub exprs: Exprs,
}

pub struct ItemInPkg {
    pub pkg_idx: usize,
    pub id: IdKey,
}

pub struct NIRId {
    pub span: Span,
    pub id: IdKey,
}

pub enum VisModifier {
    Default,
    Public,
    Private,
}

pub struct UnitStruct {
    pub vis: VisModifier,
    pub name: NIRId,
}

pub struct TupleStruct {
    pub vis: VisModifier,
    pub name: NIRId,
    pub types: ThinVec<(VisModifier, Type)>,
}

pub struct FieldsStruct {
    pub vis: VisModifier,
    pub name: NIRId,
    pub fields: ThinVec<(VisModifier, NIRId, Type)>,
}

pub struct Fn {
    pub vis: VisModifier,
    pub name: NIRId,
    pub params: ThinVec<(NIRId, Type)>,
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
