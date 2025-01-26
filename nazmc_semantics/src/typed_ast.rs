use std::{cell::RefCell, rc::Rc};

use crate::*;

#[derive(Clone, Default, Debug, PartialEq)]
pub struct RcCell<T: Clone> {
    data: Rc<RefCell<T>>,
}

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
pub type InfTy = RcCell<InferedType>;
pub type ConTy = RcCell<ConcreteType>;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Infered(InfTy),
    Concrete(ConTy),
}

impl Default for Type {
    fn default() -> Self {
        Self::Infered(InfTy::default())
    }
}

impl Ty {
    pub fn new_unknown() -> Self {
        Self::default()
    }

    pub fn new_unspecified_unsigned_int() -> Self {
        Self::new(Type::Infered(InfTy::new(
            InferedType::UnspecifiedUnsignedInt,
        )))
    }

    pub fn new_unspecified_signed_int() -> Self {
        Self::new(Type::Infered(InfTy::new(InferedType::UnspecifiedSignedInt)))
    }

    pub fn new_unspecified_float() -> Self {
        Self::new(Type::Infered(InfTy::new(InferedType::UnspecifiedFloat)))
    }

    pub fn new_concrete(con_type: ConcreteType) -> Self {
        Self::new(Type::Concrete(ConTy::new(con_type)))
    }
}

#[derive(Clone, Default, Debug, PartialEq)]
pub enum ConcreteType {
    #[default]
    Never,
    Unit,
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
pub enum InferedType {
    #[default]
    Unknown,
    UnspecifiedUnsignedInt,
    UnspecifiedSignedInt,
    UnspecifiedFloat,
    Known(ConTy),
}

#[derive(Clone, Default, Debug, PartialEq)]
pub struct TupleType {
    pub types: ThinVec<Ty>,
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
    pub bindings: HashMap<IdKey, Ty>,
}
