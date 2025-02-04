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

pub type Ty = RcCell<Type>;

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

    /// Create a `ConcreteTyp::Never` type.
    pub fn never() -> Self {
        Self::primitive(PrimitiveType::Never)
    }

    /// Create a `ConcreteTyp::Unit` type.
    pub fn unit() -> Self {
        Self::primitive(PrimitiveType::Unit)
    }

    /// Create a `ConcreteTyp::I` type.
    pub fn i() -> Self {
        Self::primitive(PrimitiveType::I)
    }

    /// Create a `ConcreteTyp::I1` type.
    pub fn i1() -> Self {
        Self::primitive(PrimitiveType::I1)
    }

    /// Create a `ConcreteTyp::I2` type.
    pub fn i2() -> Self {
        Self::primitive(PrimitiveType::I2)
    }

    /// Create a `ConcreteTyp::I4` type.
    pub fn i4() -> Self {
        Self::primitive(PrimitiveType::I4)
    }

    /// Create a `ConcreteTyp::I8` type.
    pub fn i8() -> Self {
        Self::primitive(PrimitiveType::I8)
    }
    /// Create a `ConcreteTyp::U` type.
    pub fn u() -> Self {
        Self::primitive(PrimitiveType::U)
    }

    /// Create a `ConcreteTyp::U1` type.
    pub fn u1() -> Self {
        Self::primitive(PrimitiveType::U1)
    }

    /// Create a `ConcreteTyp::U2` type.
    pub fn u2() -> Self {
        Self::primitive(PrimitiveType::U2)
    }

    /// Create a `ConcreteTyp::U4` type.
    pub fn u4() -> Self {
        Self::primitive(PrimitiveType::U4)
    }

    /// Create a `ConcreteTyp::U8` type.
    pub fn u8() -> Self {
        Self::primitive(PrimitiveType::U8)
    }

    /// Create a `ConcreteTyp::F4` type.
    pub fn f4() -> Self {
        Self::primitive(PrimitiveType::F4)
    }

    /// Create a `ConcreteTyp::F8` type.
    pub fn f8() -> Self {
        Self::primitive(PrimitiveType::F8)
    }

    /// Create a `ConcreteTyp::Bool` type.
    pub fn boolean() -> Self {
        Self::primitive(PrimitiveType::Bool)
    }

    /// Create a `ConcreteTyp::Char` type.
    pub fn character() -> Self {
        Self::primitive(PrimitiveType::Char)
    }

    /// Create a `ConcreteTyp::Str` type.
    pub fn string() -> Self {
        Self::primitive(PrimitiveType::Str)
    }
}

impl Default for Ty {
    fn default() -> Self {
        Self::never()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub enum TyVarSubstitution {
    #[default]
    Any,
    AnyOf(ThinVec<ConcreteType>),
    Determined(Ty),
}

#[derive(Debug, Default)]
pub(crate) struct TyInfer {
    pub(crate) ty_vars: TiVec<TypeVarKey, TyVarSubstitution>,
}

impl TyInfer {
    pub(crate) fn new_ty_var(&mut self) -> Ty {
        let ty_var_key = self.ty_vars.push_and_get_key(TyVarSubstitution::Any);
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
                    TyVarSubstitution::Any | TyVarSubstitution::AnyOf(_) => ty.clone(),
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

    pub(crate) fn try_unify(&mut self, t1: &Ty, t2: &Ty) -> Result<(), ()> {
        let old_state = self.ty_vars.clone();
        if let Err(err) = self.unify(t1, t2) {
            self.ty_vars = old_state;
            Err(err)
        } else {
            Ok(())
        }
    }

    pub(crate) fn try_unify_concrete(
        &mut self,
        t1: &ConcreteType,
        t2: &ConcreteType,
    ) -> Result<(), ()> {
        let old_state = self.ty_vars.clone();
        if let Err(err) = self.unify_concrete(t1, t2) {
            self.ty_vars = old_state;
            Err(err)
        } else {
            Ok(())
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
                // Clone constraints for clarity.
                let sub1 = self.ty_vars[key1].clone();
                let sub2 = self.ty_vars[key2].clone();
                match (sub1, sub2) {
                    (TyVarSubstitution::Any, TyVarSubstitution::Any) => {
                        // Arbitrarily link key1 to key2.
                        self.ty_vars[key1] = TyVarSubstitution::Determined(Ty::type_var(key2));
                        Ok(())
                    }
                    (TyVarSubstitution::Any, TyVarSubstitution::AnyOf(_)) => {
                        // key1 is unconstrained; adopt the constraints from key2.
                        self.ty_vars[key1] = TyVarSubstitution::Determined(Ty::type_var(key2));
                        // Optionally, key2 retains its AnyOf constraint.
                        Ok(())
                    }
                    (TyVarSubstitution::AnyOf(_), TyVarSubstitution::Any) => {
                        // Symmetric case.
                        self.ty_vars[key2] = TyVarSubstitution::Determined(Ty::type_var(key1));
                        Ok(())
                    }
                    (TyVarSubstitution::AnyOf(allowed1), TyVarSubstitution::AnyOf(allowed2)) => {
                        let allowed1 = self.apply_on_constraints(&allowed1);
                        let allowed2 = self.apply_on_constraints(&allowed2);
                        // Compute the intersection of both allowed sets.
                        let intersection = self.get_constraints_intersection(&allowed1, &allowed2);

                        if intersection.is_empty() {
                            // No common alternative exists.
                            Err(())
                        } else if intersection.len() == 1 {
                            // The constraint is now fully determined.
                            self.ty_vars[key1] = TyVarSubstitution::Determined(Ty::concrete(
                                intersection[0].clone(),
                            ));
                            // Link the other variable to key1.
                            self.ty_vars[key2] = TyVarSubstitution::Determined(Ty::type_var(key1));
                            Ok(())
                        } else {
                            // More than one possibility remains: update one variable with the intersected constraint.
                            self.ty_vars[key1] = TyVarSubstitution::AnyOf(
                                intersection.into_iter().map(|c| c.clone()).collect(),
                            );
                            self.ty_vars[key2] = TyVarSubstitution::Determined(Ty::type_var(key1));
                            Ok(())
                        }
                    }
                    // Should not reach here if Determined states are fully applied via `apply`.
                    _ => Err(()),
                }
            }

            (Type::TyVar(key), Type::Concrete(concrete_type))
            | (Type::Concrete(concrete_type), Type::TyVar(key)) => {
                // Ensure we do not introduce an infinite type.
                if concrete_type.occurs_check(key) {
                    return Err(());
                }

                let substiution = std::mem::take(&mut self.ty_vars[key]);

                match substiution {
                    TyVarSubstitution::Any => {
                        // No constraints; simply substitute.
                        self.ty_vars[key] =
                            TyVarSubstitution::Determined(Ty::concrete(concrete_type.clone()));
                        Ok(())
                    }
                    TyVarSubstitution::AnyOf(existing) => {
                        let allowed = self.apply_on_constraints(&existing);
                        // Check if the concrete type is one of the allowed alternatives.
                        if self.is_ty_in_constraints(&concrete_type, &allowed) {
                            self.ty_vars[key] =
                                TyVarSubstitution::Determined(Ty::concrete(concrete_type.clone()));
                            Ok(())
                        } else {
                            self.ty_vars[key] = TyVarSubstitution::AnyOf(existing);
                            // The concrete type does not satisfy the constraint.
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
        allowed: ThinVec<ConcreteType>,
    ) -> Result<(), ()> {
        // First, apply all substitutions to get the current “resolved” type.
        let applied = self.apply(ty);
        let allowed = self.apply_on_constraints(&allowed);

        let result = match &*applied.borrow() {
            // If we have a type variable…
            Type::TyVar(key) => {
                let substitution = std::mem::take(&mut self.ty_vars[*key]);

                match substitution {
                    // If unconstrained, just set the allowed set.
                    TyVarSubstitution::Any => {
                        self.ty_vars[*key] = TyVarSubstitution::AnyOf(allowed);
                        Ok(())
                    }
                    // If already constrained, intersect the new allowed set with the existing one.
                    TyVarSubstitution::AnyOf(existing) => {
                        let new_allowed = self.get_constraints_intersection(&allowed, &existing);
                        if new_allowed.is_empty() {
                            // No candidate is allowed by both constraints.
                            self.ty_vars[*key] = TyVarSubstitution::AnyOf(existing);
                            Err(())
                        } else if new_allowed.len() == 1 {
                            self.ty_vars[*key] =
                                TyVarSubstitution::Determined(Ty::concrete(new_allowed[0].clone()));
                            Ok(())
                        } else {
                            self.ty_vars[*key] = TyVarSubstitution::AnyOf(new_allowed);
                            Ok(())
                        }
                    }
                    // If already determined, check that the concrete type is one of the allowed alternatives.
                    TyVarSubstitution::Determined(determined) => {
                        let result = match &*determined.borrow() {
                            Type::Concrete(c) if self.is_ty_in_constraints(c, &allowed) => Ok(()),
                            _ => Err(()),
                        };

                        self.ty_vars[*key] = TyVarSubstitution::Determined(determined);
                        result
                    }
                }
            }
            // If the type is already concrete, verify that it is one of the allowed alternatives.
            Type::Concrete(c) => {
                if self.is_ty_in_constraints(c, &allowed) {
                    Ok(())
                } else {
                    Err(())
                }
            }
        };

        result
    }

    pub(crate) fn apply_on_constraints(
        &mut self,
        allowed: &[ConcreteType],
    ) -> ThinVec<ConcreteType> {
        allowed
            .into_iter()
            .map(|c| self.apply_on_concrete(&c))
            .collect::<ThinVec<_>>()
    }

    pub(crate) fn get_constraints_intersection(
        &mut self,
        allowed1: &[ConcreteType],
        allowed2: &[ConcreteType],
    ) -> ThinVec<ConcreteType> {
        allowed1
            .into_iter()
            .filter(|ty| self.is_ty_in_constraints(ty, &allowed2))
            .map(|ty| ty.clone())
            .collect::<ThinVec<_>>()
    }

    pub(crate) fn is_ty_in_constraints(
        &mut self,
        ty: &ConcreteType,
        allowed: &[ConcreteType],
    ) -> bool {
        allowed
            .iter()
            .any(|alt| self.try_unify_concrete(&alt, &ty).is_ok())
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
