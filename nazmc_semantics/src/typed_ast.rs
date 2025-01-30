use std::{cell::RefCell, rc::Rc};

use crate::*;
use derive_more::{From, Into};
use nazmc_data_pool::new_data_pool_key;

new_data_pool_key! { TypeVarKey }

#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub struct RcCell<T: Clone> {
    data: Rc<RefCell<T>>,
}

#[derive(Default)]
pub struct TypedAST {
    pub consts: HashMap<ConstKey, Const>,
    pub statics: HashMap<StaticKey, Static>,
    pub tuple_structs: HashMap<TupleStructKey, TupleStruct>,
    pub fields_structs: HashMap<FieldsStructKey, FieldsStruct>,
    /// Maps to fn ptr types
    pub fns_signatures: HashMap<FnKey, Ty>,
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

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub enum Type {
    #[default]
    Unknown,
    Never,
    UnspecifiedUnsignedInt,
    UnspecifiedSignedInt,
    UnspecifiedFloat,
    TypeVar(TypeVarKey),
    Slice(Ty),
    Ptr(Ty),
    Ref(Ty),
    PtrMut(Ty),
    RefMut(Ty),
    Array(ArrayType),
    Tuple(TupleType),
    Lambda(LambdaType),
    FnPtr(FnPtrType),
    Concrete(ConcreteType),
}

#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub enum ConcreteType {
    #[default]
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
}

#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub struct TupleType {
    pub types: ThinVec<Ty>,
}

#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub struct ArrayType {
    pub underlying_typ: Ty,
    pub size: u32,
}

#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub struct LambdaType {
    pub params_types: ThinVec<Ty>,
    pub return_type: Ty,
}

#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub struct FnPtrType {
    pub params_types: ThinVec<Ty>,
    pub return_type: Ty,
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
    pub idx: u32,
    pub typ: Ty,
}

pub struct LetStm {
    pub bindings: HashMap<IdKey, Ty>,
}

impl Ty {
    /// Create a new `Unknown` type.
    pub fn unknown() -> Self {
        Self::new(Type::Unknown)
    }

    /// Create a `Never` type.
    pub fn never() -> Self {
        Self::new(Type::Never)
    }

    /// Create a new `UnspecifiedUnsignedInt` type.
    pub fn unspecified_unsigned_int() -> Self {
        Self::new(Type::UnspecifiedUnsignedInt)
    }

    /// Create a new `UnspecifiedSignedInt` type.
    pub fn unspecified_signed_int() -> Self {
        Self::new(Type::UnspecifiedSignedInt)
    }

    /// Create a new `UnspecifiedFloat` type.
    pub fn unspecified_float() -> Self {
        Self::new(Type::UnspecifiedFloat)
    }

    /// Create a new `TypeVar` type with the given key.
    pub fn type_var(ty_var_key: TypeVarKey) -> Self {
        Self::new(Type::TypeVar(ty_var_key))
    }

    /// Create a new `Concrete` type.
    pub fn concrete(concrete_type: ConcreteType) -> Self {
        Self::new(Type::Concrete(concrete_type))
    }

    /// Create a `Slice` type.
    pub fn slice(inner: Ty) -> Self {
        Self::new(Type::Slice(inner))
    }

    /// Create a `Ptr` type.
    pub fn ptr(inner: Ty) -> Self {
        Self::new(Type::Ptr(inner))
    }

    /// Create a `Ref` type.
    pub fn reference(inner: Ty) -> Self {
        Self::new(Type::Ref(inner))
    }

    /// Create a `PtrMut` type.
    pub fn ptr_mut(inner: Ty) -> Self {
        Self::new(Type::PtrMut(inner))
    }

    /// Create a `RefMut` type.
    pub fn ref_mut(inner: Ty) -> Self {
        Self::new(Type::RefMut(inner))
    }

    /// Create a `Array` type.
    pub fn array(underlying: Ty, size: u32) -> Self {
        Self::new(Type::Array(ArrayType {
            underlying_typ: underlying,
            size,
        }))
    }

    /// Create a `Tuple` type.
    pub fn tuple(types: impl IntoIterator<Item = Ty>) -> Self {
        Self::new(Type::Tuple(TupleType {
            types: types.into_iter().collect(),
        }))
    }

    /// Create a `Lambda` type.
    pub fn lambda(params: impl IntoIterator<Item = Ty>, return_type: Ty) -> Self {
        Self::new(Type::Lambda(LambdaType {
            params_types: params.into_iter().collect(),
            return_type,
        }))
    }

    /// Create a `FnPtr` type.
    pub fn fn_ptr(params: impl IntoIterator<Item = Ty>, return_type: Ty) -> Self {
        Self::new(Type::FnPtr(FnPtrType {
            params_types: params.into_iter().collect(),
            return_type,
        }))
    }

    /// Create a `ConcreteType::Unit` type.
    pub fn unit() -> Self {
        Self::concrete(ConcreteType::Unit)
    }

    /// Create a `ConcreteType::I` type.
    pub fn i() -> Self {
        Self::concrete(ConcreteType::I)
    }

    /// Create a `ConcreteType::I1` type.
    pub fn i1() -> Self {
        Self::concrete(ConcreteType::I1)
    }

    /// Create a `ConcreteType::I2` type.
    pub fn i2() -> Self {
        Self::concrete(ConcreteType::I2)
    }

    /// Create a `ConcreteType::I4` type.
    pub fn i4() -> Self {
        Self::concrete(ConcreteType::I4)
    }

    /// Create a `ConcreteType::I8` type.
    pub fn i8() -> Self {
        Self::concrete(ConcreteType::I8)
    }
    /// Create a `ConcreteType::U` type.
    pub fn u() -> Self {
        Self::concrete(ConcreteType::U)
    }

    /// Create a `ConcreteType::U1` type.
    pub fn u1() -> Self {
        Self::concrete(ConcreteType::U1)
    }

    /// Create a `ConcreteType::U2` type.
    pub fn u2() -> Self {
        Self::concrete(ConcreteType::U2)
    }

    /// Create a `ConcreteType::U4` type.
    pub fn u4() -> Self {
        Self::concrete(ConcreteType::U4)
    }

    /// Create a `ConcreteType::U8` type.
    pub fn u8() -> Self {
        Self::concrete(ConcreteType::U8)
    }

    /// Create a `ConcreteType::F4` type.
    pub fn f4() -> Self {
        Self::concrete(ConcreteType::F4)
    }

    /// Create a `ConcreteType::F8` type.
    pub fn f8() -> Self {
        Self::concrete(ConcreteType::F8)
    }

    /// Create a `ConcreteType::Bool` type.
    pub fn boolean() -> Self {
        Self::concrete(ConcreteType::Bool)
    }

    /// Create a `ConcreteType::Char` type.
    pub fn character() -> Self {
        Self::concrete(ConcreteType::Char)
    }

    /// Create a `ConcreteType::Str` type.
    pub fn string() -> Self {
        Self::concrete(ConcreteType::Str)
    }

    /// Create a `ConcreteType::UnitStruct` type.
    pub fn unit_struct(key: UnitStructKey) -> Self {
        Self::concrete(ConcreteType::UnitStruct(key))
    }

    /// Create a `ConcreteType::TupleStruct` type.
    pub fn tuple_struct(key: TupleStructKey) -> Self {
        Self::concrete(ConcreteType::TupleStruct(key))
    }

    /// Create a `ConcreteType::FieldsStruct` type.
    pub fn fields_struct(key: FieldsStructKey) -> Self {
        Self::concrete(ConcreteType::FieldsStruct(key))
    }
}

impl Type {
    pub(crate) fn contains_var(&self, var_key: TypeVarKey) -> bool {
        match self {
            Type::TypeVar(key) => *key == var_key,
            Type::Slice(ty)
            | Type::Ptr(ty)
            | Type::Ref(ty)
            | Type::PtrMut(ty)
            | Type::RefMut(ty)
            | Type::Array(ArrayType {
                underlying_typ: ty, ..
            }) => ty.inner().contains_var(var_key),
            Type::Tuple(TupleType { types }) => {
                types.iter().any(|ty| ty.inner().contains_var(var_key))
            }
            Type::Lambda(LambdaType {
                params_types: types,
                return_type,
            })
            | Type::FnPtr(FnPtrType {
                params_types: types,
                return_type,
            }) => {
                types.iter().any(|ty| ty.inner().contains_var(var_key))
                    || return_type.inner().contains_var(var_key)
            }
            _ => false, // Unknown, Unspecified types and primitives cannot contain variables
        }
    }
}
