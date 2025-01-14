use crate::*;
use derive_more::{From, Into};
use nazmc_data_pool::new_data_pool_key;
new_data_pool_key! { TypeKey }

#[derive(Default)]
pub struct TypedAST {
    pub types: TiVec<TypeKey, Type>,
    pub consts: HashMap<ConstKey, Const>,
    pub statics: HashMap<StaticKey, Static>,
    pub tuple_structs: HashMap<TupleStructKey, TupleStruct>,
    pub fields_structs: HashMap<FieldsStructKey, FieldsStruct>,
    pub fns: HashMap<FnKey, Fn>,
    pub lets: HashMap<LetStmKey, LetStm>,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Type {
    pub kind: TypeKind,
    pub size: u32,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeKind {
    I,
    I1,
    I2,
    I4,
    I8,
    U,
    U1,
    U2,
    U4,
    U8,
    F4,
    F8,
    Bool,
    Char,
    Str,
    UnitStruct(UnitStructKey),
    TupleStruct(TupleStructKey),
    FieldsStruct(FieldsStructKey),
    Slice(TypeKey),
    Ptr(TypeKey),
    Ref(TypeKey),
    PtrMut(TypeKey),
    RefMut(TypeKey),
    Tuple(TupleTypeKey),
    Array(ArrayTypeKey),
    Lambda(LambdaTypeKey),
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TupleType {
    pub types: ThinVec<TypeKey>,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ArrayType {
    pub underlying_typ: TypeKey,
    pub size: u32,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LambdaType {
    pub params_types: ThinVec<TypeKey>,
    pub return_type: TypeKey,
}

pub struct Const {
    pub typ: TypeKey,
    pub value: Vec<u8>,
}

pub struct Static {
    pub typ: TypeKey,
}

pub struct TupleStruct {
    pub types: ThinVec<TypeKey>,
}

pub struct FieldsStruct {
    pub fields: HashMap<IdKey, FieldInfo>,
}

pub struct FieldInfo {
    pub offset: u32,
    pub typ: TypeKey,
}

pub struct Fn {
    pub params: ThinVec<TypeKey>,
    pub return_typ: TypeKey,
}

pub struct LetStm {
    pub bindings: HashMap<IdKey, Binding>,
}

pub struct Binding {
    pub typ: TypeKey,
    pub tuple_indexes: ThinVec<u8>,
}
