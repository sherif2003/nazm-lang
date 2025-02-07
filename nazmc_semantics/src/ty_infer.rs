use nazmc_ast::{FieldsStructKey, TupleStructKey, UnitStructKey};
use nazmc_data_pool::typed_index_collections::TiVec;
use std::rc::Rc;
use thin_vec::thin_vec;
use thin_vec::ThinVec;

use derive_more::{From, Into};
use nazmc_data_pool::new_data_pool_key;

new_data_pool_key! { TypeVarKey }

#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub struct RcCell<T: Clone> {
    data: Rc<T>,
}

impl<T: Clone> RcCell<T> {
    #[inline]
    pub fn new(data: T) -> Self {
        Self {
            data: Rc::new(data),
        }
    }

    #[inline]
    pub fn borrow(&self) -> &T {
        self.data.as_ref()
    }

    #[inline]
    pub fn inner(&self) -> T {
        self.borrow().clone()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub(crate) enum NumberConstraints {
    #[default]
    Any,
    Signed,
    Int,
    SignedInt,
    Float,
}

impl NumberConstraints {
    fn get_intersection(
        one: NumberConstraints,
        two: NumberConstraints,
    ) -> Option<NumberConstraints> {
        use NumberConstraints::*;
        match (one, two) {
            // Any intersected with anything is that thing.
            (Any, x) | (x, Any) => Some(x),

            (Signed, Signed) => Some(Signed),
            (Int, Int) => Some(Int),

            (Signed, Int)
            | (Int, Signed)
            | (Signed, SignedInt)
            | (SignedInt, Signed)
            | (Int, SignedInt)
            | (SignedInt, Int)
            | (SignedInt, SignedInt) => Some(SignedInt),

            (Signed, Float) | (Float, Signed) | (Float, Float) => Some(Float),

            (Int, Float) | (Float, Int) | (SignedInt, Float) | (Float, SignedInt) => None,
        }
    }

    fn contains(&self, concrete_ty: &ConcreteType) -> bool {
        use NumberConstraints::*;
        use PrimitiveType::*;

        // We only handle Primitive types.
        let ConcreteType::Primitive(prim_ty) = concrete_ty else {
            return false;
        };
        match self {
            Any => matches!(
                prim_ty,
                I | I1 | I2 | I4 | I8 | U | U1 | U2 | U4 | U8 | F4 | F8
            ),
            Signed => matches!(prim_ty, I | I1 | I2 | I4 | I8 | F4 | F8),
            Int => matches!(prim_ty, I | I1 | I2 | I4 | I8 | U | U1 | U2 | U4 | U8),
            SignedInt => matches!(prim_ty, I | I1 | I2 | I4 | I8),
            Float => matches!(prim_ty, F4 | F8),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub(crate) enum TyVarSubstitution {
    #[default]
    Any,
    Never,
    Error,
    ConstrainedNumber(NumberConstraints),
    Determined(Ty),
}

#[derive(Debug, Default)]
pub(crate) struct TyInfer {
    pub(crate) ty_vars: TiVec<TypeVarKey, TyVarSubstitution>,
}

pub(crate) type Ty = RcCell<Type>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    TyVar(TypeVarKey),
    Concrete(ConcreteType),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ConcreteType {
    Composite(CompositeType),
    UnitStruct(UnitStructKey),
    TupleStruct(TupleStructKey),
    FieldsStruct(FieldsStructKey),
    Primitive(PrimitiveType),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CompositeType {
    Slice(Ty),
    Ptr(Ty),
    Ref(Ty),
    PtrMut(Ty),
    RefMut(Ty),
    Array {
        underlying_typ: Ty,
        size: u32,
    },
    Tuple {
        types: ThinVec<Ty>,
    },
    Lambda {
        params_types: ThinVec<Ty>,
        return_type: Ty,
    },
    FnPtr {
        params_types: ThinVec<Ty>,
        return_type: Ty,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub enum PrimitiveType {
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
}

impl Type {
    fn occurs_check(&self, of: TypeVarKey) -> bool {
        match self {
            Type::TyVar(type_var_key) => *type_var_key == of,
            Type::Concrete(concrete_typ) => concrete_typ.occurs_check(of),
        }
    }
}

impl ConcreteType {
    fn occurs_check(&self, of: TypeVarKey) -> bool {
        match self {
            ConcreteType::Composite(composite_typ) => composite_typ.occurs_check(of),
            _ => false,
        }
    }
}

impl CompositeType {
    pub fn occurs_check(&self, of: TypeVarKey) -> bool {
        match self {
            CompositeType::Slice(underlying_typ)
            | CompositeType::Ptr(underlying_typ)
            | CompositeType::Ref(underlying_typ)
            | CompositeType::PtrMut(underlying_typ)
            | CompositeType::RefMut(underlying_typ)
            | CompositeType::Array {
                underlying_typ,
                size: _,
            } => underlying_typ.borrow().occurs_check(of),
            CompositeType::Tuple { types } => types.iter().any(|ty| ty.borrow().occurs_check(of)),
            CompositeType::Lambda {
                params_types,
                return_type,
            }
            | CompositeType::FnPtr {
                params_types,
                return_type,
            } => {
                params_types.iter().any(|ty| ty.borrow().occurs_check(of))
                    || return_type.borrow().occurs_check(of)
            }
        }
    }
}

impl Ty {
    /// Create a new `TyVar` type with the given key.
    pub fn type_var(ty_var_key: TypeVarKey) -> Self {
        Self::new(Type::TyVar(ty_var_key))
    }

    /// Create a new `Concrete` type.
    pub fn concrete(concrete_type: ConcreteType) -> Self {
        Self::new(Type::Concrete(concrete_type))
    }

    pub fn composite(composite_type: CompositeType) -> Self {
        Self::concrete(ConcreteType::Composite(composite_type))
    }

    pub fn unit_struct(unit_struct_key: UnitStructKey) -> Self {
        Self::concrete(ConcreteType::UnitStruct(unit_struct_key))
    }

    pub fn tuple_struct(tuple_struct_key: TupleStructKey) -> Self {
        Self::concrete(ConcreteType::TupleStruct(tuple_struct_key))
    }

    pub fn fields_struct(fields_struct_key: FieldsStructKey) -> Self {
        Self::concrete(ConcreteType::FieldsStruct(fields_struct_key))
    }

    pub fn primitive(primitive_type: PrimitiveType) -> Self {
        Self::concrete(ConcreteType::Primitive(primitive_type))
    }

    /// Create a `Slice` type.
    pub fn slice(inner: Ty) -> Self {
        Self::composite(CompositeType::Slice(inner))
    }

    /// Create a `Ptr` type.
    pub fn ptr(inner: Ty) -> Self {
        Self::composite(CompositeType::Ptr(inner))
    }

    /// Create a `Ref` type.
    pub fn reference(inner: Ty) -> Self {
        Self::composite(CompositeType::Ref(inner))
    }

    /// Create a `PtrMut` type.
    pub fn ptr_mut(inner: Ty) -> Self {
        Self::composite(CompositeType::PtrMut(inner))
    }

    /// Create a `RefMut` type.
    pub fn ref_mut(inner: Ty) -> Self {
        Self::composite(CompositeType::RefMut(inner))
    }

    /// Create a `Array` type.
    pub fn array(underlying: Ty, size: u32) -> Self {
        Self::composite(CompositeType::Array {
            underlying_typ: underlying,
            size,
        })
    }

    /// Create a `Tuple` type.
    pub fn tuple(types: impl IntoIterator<Item = Ty>) -> Self {
        Self::composite(CompositeType::Tuple {
            types: types.into_iter().collect(),
        })
    }

    /// Create a `Lambda` type.
    pub fn lambda(params: impl IntoIterator<Item = Ty>, return_type: Ty) -> Self {
        Self::composite(CompositeType::Lambda {
            params_types: params.into_iter().collect(),
            return_type,
        })
    }

    /// Create a `FnPtr` type.
    pub fn fn_ptr(params: impl IntoIterator<Item = Ty>, return_type: Ty) -> Self {
        Self::composite(CompositeType::FnPtr {
            params_types: params.into_iter().collect(),
            return_type,
        })
    }

    /// Create a `ConcreteType::Unit` type.
    pub fn unit() -> Self {
        Self::primitive(PrimitiveType::Unit)
    }

    /// Create a `ConcreteType::I` type.
    pub fn i() -> Self {
        Self::primitive(PrimitiveType::I)
    }

    /// Create a `ConcreteType::I1` type.
    pub fn i1() -> Self {
        Self::primitive(PrimitiveType::I1)
    }

    /// Create a `ConcreteType::I2` type.
    pub fn i2() -> Self {
        Self::primitive(PrimitiveType::I2)
    }

    /// Create a `ConcreteType::I4` type.
    pub fn i4() -> Self {
        Self::primitive(PrimitiveType::I4)
    }

    /// Create a `ConcreteType::I8` type.
    pub fn i8() -> Self {
        Self::primitive(PrimitiveType::I8)
    }
    /// Create a `ConcreteType::U` type.
    pub fn u() -> Self {
        Self::primitive(PrimitiveType::U)
    }

    /// Create a `ConcreteType::U1` type.
    pub fn u1() -> Self {
        Self::primitive(PrimitiveType::U1)
    }

    /// Create a `ConcreteType::U2` type.
    pub fn u2() -> Self {
        Self::primitive(PrimitiveType::U2)
    }

    /// Create a `ConcreteType::U4` type.
    pub fn u4() -> Self {
        Self::primitive(PrimitiveType::U4)
    }

    /// Create a `ConcreteType::U8` type.
    pub fn u8() -> Self {
        Self::primitive(PrimitiveType::U8)
    }

    /// Create a `ConcreteType::F4` type.
    pub fn f4() -> Self {
        Self::primitive(PrimitiveType::F4)
    }

    /// Create a `ConcreteType::F8` type.
    pub fn f8() -> Self {
        Self::primitive(PrimitiveType::F8)
    }

    /// Create a `ConcreteType::Bool` type.
    pub fn boolean() -> Self {
        Self::primitive(PrimitiveType::Bool)
    }

    /// Create a `ConcreteType::Char` type.
    pub fn character() -> Self {
        Self::primitive(PrimitiveType::Char)
    }

    /// Create a `ConcreteType::Str` type.
    pub fn string() -> Self {
        Self::primitive(PrimitiveType::Str)
    }
}

impl Default for Ty {
    fn default() -> Self {
        Self::unit()
    }
}

impl TyInfer {
    pub(crate) fn new_ty_var(&mut self) -> Ty {
        let ty_var_key = self.ty_vars.push_and_get_key(TyVarSubstitution::Any);
        Ty::type_var(ty_var_key)
    }

    pub(crate) fn new_never_ty_var(&mut self) -> Ty {
        let ty_var_key = self.ty_vars.push_and_get_key(TyVarSubstitution::Never);
        Ty::type_var(ty_var_key)
    }

    pub(crate) fn new_int_ty_var(&mut self) -> Ty {
        let ty_var_key = self
            .ty_vars
            .push_and_get_key(TyVarSubstitution::ConstrainedNumber(NumberConstraints::Int));
        Ty::type_var(ty_var_key)
    }

    pub(crate) fn new_float_ty_var(&mut self) -> Ty {
        let ty_var_key = self
            .ty_vars
            .push_and_get_key(TyVarSubstitution::ConstrainedNumber(
                NumberConstraints::Float,
            ));
        Ty::type_var(ty_var_key)
    }

    pub(crate) fn apply(&self, ty: &Ty) -> Ty {
        match &*ty.borrow() {
            Type::Concrete(concrete_type) => Ty::concrete(self.apply_on_concrete(concrete_type)),

            // Replace a type variable if it has a substitution
            Type::TyVar(type_var_key) => {
                let substitution = &self.ty_vars[*type_var_key];

                match substitution {
                    TyVarSubstitution::Determined(rc_cell) => self.apply(&rc_cell),
                    TyVarSubstitution::Any
                    | TyVarSubstitution::Never
                    | TyVarSubstitution::Error
                    | TyVarSubstitution::ConstrainedNumber(_) => ty.clone(),
                }
            }
        }
    }

    pub(crate) fn apply_on_concrete(&self, concrete_type: &ConcreteType) -> ConcreteType {
        match concrete_type {
            ConcreteType::Composite(composite_type) => {
                ConcreteType::Composite(self.apply_on_composite(composite_type))
            }
            concrete_type @ _ => concrete_type.clone(),
        }
    }

    pub(crate) fn apply_on_composite(&self, composite_type: &CompositeType) -> CompositeType {
        match composite_type {
            // Recursively apply substitutions to concrete types
            CompositeType::Slice(inner) => CompositeType::Slice(self.apply(inner)),
            CompositeType::Ptr(inner) => CompositeType::Ptr(self.apply(inner)),
            CompositeType::Ref(inner) => CompositeType::Ref(self.apply(inner)),
            CompositeType::PtrMut(inner) => CompositeType::PtrMut(self.apply(inner)),
            CompositeType::RefMut(inner) => CompositeType::RefMut(self.apply(inner)),

            CompositeType::Array {
                underlying_typ,
                size,
            } => CompositeType::Array {
                underlying_typ: self.apply(&underlying_typ),
                size: *size,
            },

            CompositeType::Tuple { types } => CompositeType::Tuple {
                types: types.iter().map(|inner| self.apply(inner)).collect(),
            },

            CompositeType::Lambda {
                params_types,
                return_type,
            } => CompositeType::Lambda {
                params_types: params_types.iter().map(|param| self.apply(param)).collect(),
                return_type: self.apply(&return_type),
            },

            CompositeType::FnPtr {
                params_types,
                return_type,
            } => CompositeType::FnPtr {
                params_types: params_types.iter().map(|param| self.apply(param)).collect(),
                return_type: self.apply(&return_type),
            },
        }
    }

    pub(crate) fn unify(&mut self, t1: &Ty, t2: &Ty) -> Result<(), ()> {
        let t1 = self.apply(t1);
        let t2 = self.apply(t2);

        match (t1.inner(), t2.inner()) {
            // If both are concrete types, compare them recursively
            (Type::Concrete(c1), Type::Concrete(c2)) => self.unify_concrete(&c1, &c2),

            // If both are the same type variable, they're already unified
            (Type::TyVar(key1), Type::TyVar(key2)) if key1 == key2 => Ok(()),

            (Type::TyVar(key1), Type::TyVar(key2)) => {
                match (&self.ty_vars[key1], &self.ty_vars[key2]) {
                    (TyVarSubstitution::Error, _)
                    | (
                        TyVarSubstitution::ConstrainedNumber(_),
                        TyVarSubstitution::Any | TyVarSubstitution::Never,
                    ) => {
                        self.ty_vars[key2] = TyVarSubstitution::Determined(Ty::type_var(key1));
                        Ok(())
                    }
                    (_, TyVarSubstitution::Error)
                    | (
                        TyVarSubstitution::Any | TyVarSubstitution::Never,
                        TyVarSubstitution::Any | TyVarSubstitution::Never,
                    )
                    | (
                        TyVarSubstitution::Any | TyVarSubstitution::Never,
                        TyVarSubstitution::ConstrainedNumber(_),
                    ) => {
                        self.ty_vars[key1] = TyVarSubstitution::Determined(Ty::type_var(key2));
                        Ok(())
                    }
                    (
                        TyVarSubstitution::ConstrainedNumber(constraints1),
                        TyVarSubstitution::ConstrainedNumber(constraints2),
                    ) => {
                        if let Some(intersection) =
                            NumberConstraints::get_intersection(*constraints1, *constraints2)
                        {
                            // More than one possibility remains: update one variable with the intersected constraint.
                            self.ty_vars[key1] = TyVarSubstitution::ConstrainedNumber(intersection);

                            // Link the other variable to key1.
                            self.ty_vars[key2] = TyVarSubstitution::Determined(Ty::type_var(key1));

                            Ok(())
                        } else {
                            Err(())
                        }
                    }
                    // Should not reach here if Determined states are fully applied via `apply`.
                    _ => Err(()),
                }
            }

            (Type::TyVar(key), Type::Concrete(concrete_ty))
            | (Type::Concrete(concrete_ty), Type::TyVar(key)) => {
                // Ensure we do not introduce an infinite type.
                if concrete_ty.occurs_check(key) {
                    return Err(());
                }

                match &self.ty_vars[key] {
                    TyVarSubstitution::Error => Ok(()), // Just unify, and it will be reported
                    TyVarSubstitution::Any | TyVarSubstitution::Never => {
                        // No constraints; simply substitute.
                        self.ty_vars[key] =
                            TyVarSubstitution::Determined(Ty::concrete(concrete_ty));
                        Ok(())
                    }
                    TyVarSubstitution::ConstrainedNumber(constraints) => {
                        // Check if the concrete type is one of the allowed costraints.
                        if constraints.contains(&concrete_ty) {
                            self.ty_vars[key] =
                                TyVarSubstitution::Determined(Ty::concrete(concrete_ty));
                            Ok(())
                        } else {
                            // The concrete type does not satisfy the constraints.
                            Err(())
                        }
                    }
                    TyVarSubstitution::Determined(_) => unreachable!(),
                }
            }
        }
    }

    pub(crate) fn unify_concrete(
        &mut self,
        c1: &ConcreteType,
        c2: &ConcreteType,
    ) -> Result<(), ()> {
        match (c1, c2) {
            (ConcreteType::Composite(c1), ConcreteType::Composite(c2)) => {
                self.unify_composite(c1, c2)
            }
            (ConcreteType::UnitStruct(k1), ConcreteType::UnitStruct(k2)) if k1 == k2 => Ok(()),
            (ConcreteType::TupleStruct(k1), ConcreteType::TupleStruct(k2)) if k1 == k2 => Ok(()),
            (ConcreteType::FieldsStruct(k1), ConcreteType::FieldsStruct(k2)) if k1 == k2 => Ok(()),
            (ConcreteType::Primitive(p1), ConcreteType::Primitive(p2)) if p1 == p2 => Ok(()),
            _ => Err(()),
        }
    }

    pub(crate) fn unify_composite(
        &mut self,
        c1: &CompositeType,
        c2: &CompositeType,
    ) -> Result<(), ()> {
        match (c1, c2) {
            (CompositeType::Slice(t1), CompositeType::Slice(t2))
            | (CompositeType::Ptr(t1), CompositeType::Ptr(t2))
            | (CompositeType::Ref(t1), CompositeType::Ref(t2))
            | (CompositeType::PtrMut(t1), CompositeType::PtrMut(t2))
            | (CompositeType::RefMut(t1), CompositeType::RefMut(t2)) => self.unify(&t1, &t2),

            (
                CompositeType::Array {
                    underlying_typ: t1,
                    size: s1,
                },
                CompositeType::Array {
                    underlying_typ: t2,
                    size: s2,
                },
            ) if s1 == s2 => self.unify(&t1, &t2),

            (CompositeType::Tuple { types: t1 }, CompositeType::Tuple { types: t2 })
                if t1.len() == t2.len() =>
            {
                for (t1, t2) in t1.iter().zip(t2.iter()) {
                    self.unify(t1, t2)?;
                }
                Ok(())
            }

            (
                CompositeType::Lambda {
                    params_types: params_types1,
                    return_type: return_type1,
                },
                CompositeType::Lambda {
                    params_types: params_types2,
                    return_type: return_type2,
                },
            )
            | (
                CompositeType::FnPtr {
                    params_types: params_types1,
                    return_type: return_type1,
                },
                CompositeType::FnPtr {
                    params_types: params_types2,
                    return_type: return_type2,
                },
            ) if params_types1.len() == params_types2.len() => {
                for (t1, t2) in params_types1.iter().zip(params_types2.iter()) {
                    self.unify(t1, t2)?;
                }
                self.unify(return_type1, return_type2)?;
                Ok(())
            }
            _ => Err(()),
        }
    }

    /// Constrain the given type variable (or type) to the allowed set of concrete types.
    /// Returns Ok(()) if the constraint is compatible, or Err(()) if the determined type
    /// is not among the allowed alternatives or the intersection is empty.
    pub fn constrain_type_var(
        &mut self,
        ty: &Ty,
        constraints: NumberConstraints,
    ) -> Result<(), ()> {
        // First, apply all substitutions to get the current “resolved” type.
        let applied = self.apply(ty);

        let result = match &*applied.borrow() {
            // If we have a type variable…
            Type::TyVar(key) => {
                let substitution = self.ty_vars[*key].clone();

                match substitution {
                    TyVarSubstitution::Error => Ok(()), // The error will be reported
                    // If unconstrained, just set the allowed set.
                    TyVarSubstitution::Any | TyVarSubstitution::Never => {
                        self.ty_vars[*key] = TyVarSubstitution::ConstrainedNumber(constraints);
                        Ok(())
                    }
                    // If already constrained, intersect the new allowed set with the existing one.
                    TyVarSubstitution::ConstrainedNumber(existing_constraints) => {
                        if let Some(intersection) =
                            NumberConstraints::get_intersection(constraints, existing_constraints)
                        {
                            // More than one possibility remains: update one variable with the intersected constraint.
                            self.ty_vars[*key] = TyVarSubstitution::ConstrainedNumber(intersection);
                            Ok(())
                        } else {
                            Err(())
                        }
                    }
                    // If already determined, check that the concrete type is one of the allowed alternatives.
                    TyVarSubstitution::Determined(determined) => match &*determined.borrow() {
                        Type::Concrete(c) if constraints.contains(c) => Ok(()),
                        _ => Err(()),
                    },
                }
            }
            // If the type is already concrete, verify that it is one of the allowed alternatives.
            Type::Concrete(c) if constraints.contains(c) => Ok(()),

            _ => Err(()),
        };

        result
    }

    pub(crate) fn unspecified_number_constraints() -> ThinVec<ConcreteType> {
        thin_vec![
            ConcreteType::Primitive(PrimitiveType::I),
            ConcreteType::Primitive(PrimitiveType::I1),
            ConcreteType::Primitive(PrimitiveType::I2),
            ConcreteType::Primitive(PrimitiveType::I4),
            ConcreteType::Primitive(PrimitiveType::I8),
            ConcreteType::Primitive(PrimitiveType::U),
            ConcreteType::Primitive(PrimitiveType::U1),
            ConcreteType::Primitive(PrimitiveType::U2),
            ConcreteType::Primitive(PrimitiveType::U4),
            ConcreteType::Primitive(PrimitiveType::U8),
            ConcreteType::Primitive(PrimitiveType::F4),
            ConcreteType::Primitive(PrimitiveType::F8),
        ]
    }

    pub(crate) fn unspecified_signed_number_constraints() -> ThinVec<ConcreteType> {
        thin_vec![
            ConcreteType::Primitive(PrimitiveType::I),
            ConcreteType::Primitive(PrimitiveType::I1),
            ConcreteType::Primitive(PrimitiveType::I2),
            ConcreteType::Primitive(PrimitiveType::I4),
            ConcreteType::Primitive(PrimitiveType::I8),
            ConcreteType::Primitive(PrimitiveType::F4),
            ConcreteType::Primitive(PrimitiveType::F8),
        ]
    }

    pub(crate) fn unspecified_int_constraints() -> ThinVec<ConcreteType> {
        thin_vec![
            ConcreteType::Primitive(PrimitiveType::I),
            ConcreteType::Primitive(PrimitiveType::I1),
            ConcreteType::Primitive(PrimitiveType::I2),
            ConcreteType::Primitive(PrimitiveType::I4),
            ConcreteType::Primitive(PrimitiveType::I8),
            ConcreteType::Primitive(PrimitiveType::U),
            ConcreteType::Primitive(PrimitiveType::U1),
            ConcreteType::Primitive(PrimitiveType::U2),
            ConcreteType::Primitive(PrimitiveType::U4),
            ConcreteType::Primitive(PrimitiveType::U8),
        ]
    }

    pub(crate) fn unspecified_float_constraints() -> ThinVec<ConcreteType> {
        thin_vec![
            ConcreteType::Primitive(PrimitiveType::F4),
            ConcreteType::Primitive(PrimitiveType::F8),
        ]
    }
    pub(crate) fn make_ty_var_error(&mut self, key: TypeVarKey) -> bool {
        if matches!(
            self.ty_vars[key],
            TyVarSubstitution::Determined(_) | TyVarSubstitution::ConstrainedNumber(_)
        ) {
            false
        } else {
            self.ty_vars[key] = TyVarSubstitution::Error;
            true
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_simple() {
        let mut infer = TyInfer::default();

        let t0 = infer.new_ty_var();
        let t1 = infer.new_ty_var();
        let t2 = infer.new_ty_var();
        let t3 = infer.new_ty_var();
        let expected_ty = Ty::i4();

        assert!(infer.unify(&t0, &t3).is_ok());
        assert!(infer.unify(&t1, &t2).is_ok());
        assert!(infer.unify(&t3, &t1).is_ok());

        assert!(infer
            .constrain_type_var(
                &t0,
                thin_vec![
                    ConcreteType::Primitive(PrimitiveType::I1),
                    ConcreteType::Primitive(PrimitiveType::I2),
                    ConcreteType::Primitive(PrimitiveType::I4),
                    ConcreteType::Primitive(PrimitiveType::I8),
                ],
            )
            .is_ok());

        assert!(infer.unify(&expected_ty, &t1).is_ok());

        assert_eq!(infer.apply(&t0), expected_ty);
        assert_eq!(infer.apply(&t1), expected_ty);
        assert_eq!(infer.apply(&t2), expected_ty);
        assert_eq!(infer.apply(&t3), expected_ty);
    }

    #[test]
    fn test_constraint_to_one() {
        let mut infer = TyInfer::default();

        let t0 = infer.new_ty_var();
        let t1 = infer.new_ty_var();
        let t2 = infer.new_ty_var();
        let t3 = infer.new_ty_var();
        let expected_ty = Ty::i4();

        assert!(infer.unify(&t0, &t3).is_ok());
        assert!(infer.unify(&t1, &t2).is_ok());
        assert!(infer.unify(&t3, &t1).is_ok());

        assert!(infer
            .constrain_type_var(&t0, thin_vec![ConcreteType::Primitive(PrimitiveType::I4),],)
            .is_ok());

        assert!(infer.unify(&expected_ty, &t1).is_ok());

        assert_eq!(infer.apply(&t0), expected_ty);
        assert_eq!(infer.apply(&t1), expected_ty);
        assert_eq!(infer.apply(&t2), expected_ty);
        assert_eq!(infer.apply(&t3), expected_ty);
    }

    #[test]
    fn test_constraint_to_multiple_composites() {
        let mut infer = TyInfer::default();

        let t0 = infer.new_ty_var();
        let t1 = infer.new_ty_var();
        let t2 = infer.new_ty_var();
        let t3 = infer.new_ty_var();
        let t4 = infer.new_ty_var();

        let params_types = thin_vec![t1.clone(), t2.clone()];
        let return_type = t3;

        assert!(infer
            .constrain_type_var(
                &t0,
                thin_vec![
                    ConcreteType::Composite(CompositeType::FnPtr {
                        params_types: params_types.clone(),
                        return_type: return_type.clone()
                    }),
                    ConcreteType::Composite(CompositeType::Lambda {
                        params_types,
                        return_type
                    }),
                ]
            )
            .is_ok());

        assert!(infer.unify(&t1, &Ty::boolean()).is_ok());
        assert!(infer.unify(&t2, &Ty::i4()).is_ok());
        assert!(infer.unify(&t0, &Ty::i4()).is_err());
        assert!(infer
            .unify(
                &t0,
                &Ty::fn_ptr(thin_vec![Ty::boolean(), t4.clone()], t4.clone())
            )
            .is_ok());
        assert!(infer.unify(&Ty::i4(), &t4).is_ok());

        println!("{:#?}", infer.ty_vars)
    }

    #[test]
    fn test_constraint_to_multiple_composites_complex() {
        let mut infer = TyInfer::default();

        let t0 = infer.new_ty_var();
        let t1 = infer.new_ty_var();
        let t2 = infer.new_ty_var();
        let t3 = infer.new_ty_var();
        let t4 = infer.new_ty_var();

        assert!(infer
            .constrain_type_var(&t1, TyInfer::unspecified_int_constraints())
            .is_ok());
        assert!(infer
            .constrain_type_var(&t2, TyInfer::unspecified_int_constraints())
            .is_ok());
        assert!(infer.unify(&t1, &t2).is_ok());

        let params_types = thin_vec![t1.clone(), t2.clone()];
        let return_type = t3;

        assert!(infer
            .constrain_type_var(
                &t0,
                thin_vec![
                    ConcreteType::Composite(CompositeType::FnPtr {
                        params_types: params_types.clone(),
                        return_type: return_type.clone()
                    }),
                    ConcreteType::Composite(CompositeType::Lambda {
                        params_types,
                        return_type
                    }),
                ]
            )
            .is_ok());

        assert!(infer
            .unify(&t0, &Ty::fn_ptr(thin_vec![Ty::i4(), Ty::i8()], Ty::unit()))
            .is_err());

        assert!(infer.unify(&t1, &Ty::i4()).is_ok());
        assert!(infer.unify(&t2, &Ty::i4()).is_ok());
        println!("{:#?}", infer.ty_vars)
    }

    #[test]
    fn test_constraint_to_multiple_composites_like_in_if_conditions() {
        let mut infer = TyInfer::default();

        let t0 = infer.new_ty_var();
        let t1 = infer.new_ty_var();
        let t2 = infer.new_ty_var();

        assert!(infer
            .constrain_type_var(
                &t0,
                thin_vec![
                    ConcreteType::Composite(CompositeType::Ptr(t1.clone())),
                    ConcreteType::Composite(CompositeType::PtrMut(t1.clone())),
                    ConcreteType::Primitive(PrimitiveType::Bool),
                ]
            )
            .is_ok());

        assert!(infer.unify(&t1, &Ty::character()).is_ok());
        assert!(infer.unify(&t0, &Ty::i4()).is_err());
        assert!(infer.unify(&t0, &Ty::boolean()).is_ok());

        println!("{:#?}", infer.ty_vars)
    }
}
