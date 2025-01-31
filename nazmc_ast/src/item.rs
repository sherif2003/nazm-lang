use nazmc_data_pool::IdKey;

use crate::{
    ConstKey, FieldsStructKey, FnKey, LetStmKey, ScopeKey, StaticKey, TupleStructKey,
    UnitStructKey, VisModifier,
};

#[derive(Clone, Copy, Default, Debug)]
pub enum Item {
    #[default]
    Pkg,
    UnitStruct {
        vis: VisModifier,
        key: UnitStructKey,
    },
    TupleStruct {
        vis: VisModifier,
        key: TupleStructKey,
    },
    FieldsStruct {
        vis: VisModifier,
        key: FieldsStructKey,
    },
    Const {
        vis: VisModifier,
        key: ConstKey,
    },
    Static {
        vis: VisModifier,
        key: StaticKey,
    },
    Fn {
        vis: VisModifier,
        key: FnKey,
    },
    LocalVar {
        id: IdKey,
        key: LetStmKey,
    },
    FnParam {
        idx: u32,
        fn_key: FnKey,
    },
    LambdaParam {
        id: IdKey,
        scope_key: ScopeKey,
    },
}
