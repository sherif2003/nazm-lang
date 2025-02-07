use crate::*;

#[derive(Default)]
pub struct TypedAST {
    pub consts: HashMap<ConstKey, Const>,
    pub statics: HashMap<StaticKey, Static>,
    pub tuple_structs: HashMap<TupleStructKey, TupleStruct>,
    pub fields_structs: HashMap<FieldsStructKey, FieldsStruct>,
    /// Maps to fn ptr types
    pub fns_signatures: HashMap<FnKey, Type>,
    pub lets: HashMap<LetStmKey, LetStm>,
    pub lambdas_params: HashMap<ScopeKey, LambdaParams>,
    pub exprs: HashMap<ExprKey, Type>,
}

#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub struct TupleType {
    pub types: ThinVec<Type>,
}

#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub struct ArrayType {
    pub underlying_typ: Type,
    pub size: u32,
}

#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub struct LambdaType {
    pub params_types: ThinVec<Type>,
    pub return_type: Type,
}

#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub struct FnPtrType {
    pub params_types: ThinVec<Type>,
    pub return_type: Type,
}

pub struct Const {
    pub typ: Type,
    pub value: Vec<u8>,
}

pub struct Static {
    pub typ: Type,
}

#[derive(Default)]
pub struct TupleStruct {
    pub types: ThinVec<FieldInfo>,
    pub size: u32,
    pub align: u8,
}

#[derive(Default)]
pub struct FieldsStruct {
    pub fields: HashMap<IdKey, FieldInfo>,
    pub size: u32,
    pub align: u8,
}

#[derive(Clone, Default, Debug, PartialEq)]
pub struct FieldInfo {
    pub offset: u32,
    pub idx: u32,
    pub typ: Type,
}

#[derive(Debug, Default, Clone)]
pub struct LetStm {
    pub bindings: HashMap<IdKey, Type>,
    pub ty: Type,
}

pub struct LambdaParams {
    pub bindings: HashMap<IdKey, Type>,
}
