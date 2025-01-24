use std::{cell::RefCell, rc::Rc};

use crate::*;

#[derive(Clone, Default, Debug, PartialEq)]
pub struct RcCell<T: Clone> {
    data: Rc<RefCell<T>>,
}

impl<T: Clone> RcCell<T> {
    #[inline]
    pub fn new(data: T) -> Self {
        Self {
            data: Rc::new(RefCell::new(data)),
        }
    }

    #[inline]
    pub fn borrow(&self) -> std::cell::Ref<T> {
        self.data.borrow()
    }

    #[inline]
    pub fn borrow_mut(&self) -> std::cell::RefMut<T> {
        self.data.borrow_mut()
    }

    #[inline]
    pub fn inner(&self) -> T {
        self.borrow().clone()
    }
}

pub type Ty = RcCell<Type>;

#[derive(Default)]
pub struct TypedAST {
    pub consts: HashMap<ConstKey, Const>,
    pub statics: HashMap<StaticKey, Static>,
    pub tuple_structs: HashMap<TupleStructKey, TupleStruct>,
    pub fields_structs: HashMap<FieldsStructKey, FieldsStruct>,
    pub fns_signatures: HashMap<FnKey, FnPtrType>,
    pub lets: HashMap<LetStmKey, LetStm>,
    pub exprs: HashMap<ExprKey, Ty>,
}

#[derive(Clone, Default, Debug, PartialEq)]
pub enum Type {
    #[default]
    Unknown,
    Never,
    Unit,
    UnspecifiedUnsignedInt,
    UnspecifiedSignedInt,
    UnspecifiedFloat,
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
    Slice(Ty),
    Ptr(Ty),
    Ref(Ty),
    PtrMut(Ty),
    RefMut(Ty),
    Array(ArrayType),
    Tuple(TupleType),
    Lambda(LambdaType),
    FnPtr(FnPtrType),
}

#[derive(Clone, Default, Debug, PartialEq)]
pub struct TupleType {
    pub types: ThinVec<FieldInfo>,
}

#[derive(Clone, Default, Debug, PartialEq)]
pub struct ArrayType {
    pub underlying_typ: Ty,
    pub size: u32,
}

#[derive(Clone, Default, Debug, PartialEq)]
pub struct LambdaType {
    pub params_types: ThinVec<Ty>,
    pub return_type: Ty,
}

#[derive(Clone, Default, Debug, PartialEq)]
pub struct FnPtrType {
    pub params: ThinVec<Ty>,
    pub return_typ: Ty,
}

pub struct Const {
    pub typ: Ty,
    pub value: Vec<u8>,
}

pub struct Static {
    pub typ: Ty,
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
    pub typ: Ty,
}

pub struct LetStm {
    pub bindings: HashMap<IdKey, Binding>,
}

pub struct Binding {
    pub typ: Ty,
    pub tuple_indexes: ThinVec<u8>,
}
