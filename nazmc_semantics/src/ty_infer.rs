use nazmc_ast::{FieldsStructKey, TupleStructKey, UnitStructKey};
use nazmc_data_pool::typed_index_collections::TiVec;
use thin_vec::{thin_vec, ThinVec};

use crate::typed_ast::{RcCell, TypeVarKey};

pub type RcTyp = RcCell<Typ>;

impl RcTyp {
    /// Create a new `TyVar` type with the given key.
    pub fn type_var(ty_var_key: TypeVarKey) -> Self {
        Self::new(Typ::TyVar(ty_var_key))
    }

    /// Create a new `Concrete` type.
    pub fn concrete(concrete_type: ConcreteTyp) -> Self {
        Self::new(Typ::Concrete(concrete_type))
    }

    pub fn composite(composite_type: CompositeTyp) -> Self {
        Self::concrete(ConcreteTyp::Composite(composite_type))
    }

    pub fn unit_struct(unit_struct_key: UnitStructKey) -> Self {
        Self::concrete(ConcreteTyp::UnitStruct(unit_struct_key))
    }

    pub fn tuple_struct(tuple_struct_key: TupleStructKey) -> Self {
        Self::concrete(ConcreteTyp::TupleStruct(tuple_struct_key))
    }

    pub fn fields_struct(fields_struct_key: FieldsStructKey) -> Self {
        Self::concrete(ConcreteTyp::FieldsStruct(fields_struct_key))
    }

    pub fn primitive(primitive_type: PrimitiveTyp) -> Self {
        Self::concrete(ConcreteTyp::Primitive(primitive_type))
    }

    /// Create a `Slice` type.
    pub fn slice(inner: RcTyp) -> Self {
        Self::composite(CompositeTyp::Slice(inner))
    }

    /// Create a `Ptr` type.
    pub fn ptr(inner: RcTyp) -> Self {
        Self::composite(CompositeTyp::Ptr(inner))
    }

    /// Create a `Ref` type.
    pub fn reference(inner: RcTyp) -> Self {
        Self::composite(CompositeTyp::Ref(inner))
    }

    /// Create a `PtrMut` type.
    pub fn ptr_mut(inner: RcTyp) -> Self {
        Self::composite(CompositeTyp::PtrMut(inner))
    }

    /// Create a `RefMut` type.
    pub fn ref_mut(inner: RcTyp) -> Self {
        Self::composite(CompositeTyp::RefMut(inner))
    }

    /// Create a `Array` type.
    pub fn array(underlying: RcTyp, size: u32) -> Self {
        Self::composite(CompositeTyp::Array {
            underlying_typ: underlying,
            size,
        })
    }

    /// Create a `Tuple` type.
    pub fn tuple(types: impl IntoIterator<Item = RcTyp>) -> Self {
        Self::composite(CompositeTyp::Tuple {
            types: types.into_iter().collect(),
        })
    }

    /// Create a `Lambda` type.
    pub fn lambda(params: impl IntoIterator<Item = RcTyp>, return_type: RcTyp) -> Self {
        Self::composite(CompositeTyp::Lambda {
            params_types: params.into_iter().collect(),
            return_type,
        })
    }

    /// Create a `FnPtr` type.
    pub fn fn_ptr(params: impl IntoIterator<Item = RcTyp>, return_type: RcTyp) -> Self {
        Self::composite(CompositeTyp::FnPtr {
            params_types: params.into_iter().collect(),
            return_type,
        })
    }

    /// Create a `ConcreteTyp::Never` type.
    pub fn never() -> Self {
        Self::primitive(PrimitiveTyp::Never)
    }

    /// Create a `ConcreteTyp::Unit` type.
    pub fn unit() -> Self {
        Self::primitive(PrimitiveTyp::Unit)
    }

    /// Create a `ConcreteTyp::I` type.
    pub fn i() -> Self {
        Self::primitive(PrimitiveTyp::I)
    }

    /// Create a `ConcreteTyp::I1` type.
    pub fn i1() -> Self {
        Self::primitive(PrimitiveTyp::I1)
    }

    /// Create a `ConcreteTyp::I2` type.
    pub fn i2() -> Self {
        Self::primitive(PrimitiveTyp::I2)
    }

    /// Create a `ConcreteTyp::I4` type.
    pub fn i4() -> Self {
        Self::primitive(PrimitiveTyp::I4)
    }

    /// Create a `ConcreteTyp::I8` type.
    pub fn i8() -> Self {
        Self::primitive(PrimitiveTyp::I8)
    }
    /// Create a `ConcreteTyp::U` type.
    pub fn u() -> Self {
        Self::primitive(PrimitiveTyp::U)
    }

    /// Create a `ConcreteTyp::U1` type.
    pub fn u1() -> Self {
        Self::primitive(PrimitiveTyp::U1)
    }

    /// Create a `ConcreteTyp::U2` type.
    pub fn u2() -> Self {
        Self::primitive(PrimitiveTyp::U2)
    }

    /// Create a `ConcreteTyp::U4` type.
    pub fn u4() -> Self {
        Self::primitive(PrimitiveTyp::U4)
    }

    /// Create a `ConcreteTyp::U8` type.
    pub fn u8() -> Self {
        Self::primitive(PrimitiveTyp::U8)
    }

    /// Create a `ConcreteTyp::F4` type.
    pub fn f4() -> Self {
        Self::primitive(PrimitiveTyp::F4)
    }

    /// Create a `ConcreteTyp::F8` type.
    pub fn f8() -> Self {
        Self::primitive(PrimitiveTyp::F8)
    }

    /// Create a `ConcreteTyp::Bool` type.
    pub fn boolean() -> Self {
        Self::primitive(PrimitiveTyp::Bool)
    }

    /// Create a `ConcreteTyp::Char` type.
    pub fn character() -> Self {
        Self::primitive(PrimitiveTyp::Char)
    }

    /// Create a `ConcreteTyp::Str` type.
    pub fn string() -> Self {
        Self::primitive(PrimitiveTyp::Str)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Typ {
    TyVar(TypeVarKey),
    Concrete(ConcreteTyp),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ConcreteTyp {
    Composite(CompositeTyp),
    UnitStruct(UnitStructKey),
    TupleStruct(TupleStructKey),
    FieldsStruct(FieldsStructKey),
    Primitive(PrimitiveTyp),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CompositeTyp {
    Slice(RcTyp),
    Ptr(RcTyp),
    Ref(RcTyp),
    PtrMut(RcTyp),
    RefMut(RcTyp),
    Array {
        underlying_typ: RcTyp,
        size: u32,
    },
    Tuple {
        types: ThinVec<RcTyp>,
    },
    Lambda {
        params_types: ThinVec<RcTyp>,
        return_type: RcTyp,
    },
    FnPtr {
        params_types: ThinVec<RcTyp>,
        return_type: RcTyp,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub enum PrimitiveTyp {
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

impl Typ {
    fn occurs_check(&self, of: TypeVarKey) -> bool {
        match self {
            Typ::TyVar(type_var_key) => *type_var_key == of,
            Typ::Concrete(concrete_typ) => concrete_typ.occurs_check(of),
        }
    }
}

impl ConcreteTyp {
    fn occurs_check(&self, of: TypeVarKey) -> bool {
        match self {
            ConcreteTyp::Composite(composite_typ) => composite_typ.occurs_check(of),
            _ => false,
        }
    }
}

impl CompositeTyp {
    pub fn occurs_check(&self, of: TypeVarKey) -> bool {
        match self {
            CompositeTyp::Slice(underlying_typ)
            | CompositeTyp::Ptr(underlying_typ)
            | CompositeTyp::Ref(underlying_typ)
            | CompositeTyp::PtrMut(underlying_typ)
            | CompositeTyp::RefMut(underlying_typ)
            | CompositeTyp::Array {
                underlying_typ,
                size: _,
            } => underlying_typ.borrow().occurs_check(of),
            CompositeTyp::Tuple { types } => types.iter().any(|ty| ty.borrow().occurs_check(of)),
            CompositeTyp::Lambda {
                params_types,
                return_type,
            }
            | CompositeTyp::FnPtr {
                params_types,
                return_type,
            } => {
                params_types.iter().any(|ty| ty.borrow().occurs_check(of))
                    || return_type.borrow().occurs_check(of)
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub enum TyVarSubstitution {
    #[default]
    Any,
    AnyOf(ThinVec<ConcreteTyp>),
    Determined(RcTyp),
}

#[derive(Debug, Default)]
pub struct TyInfer {
    ty_vars: TiVec<TypeVarKey, TyVarSubstitution>,
}

impl TyInfer {
    pub fn new_ty_var(&mut self) -> RcTyp {
        let ty_var_key = self.ty_vars.push_and_get_key(TyVarSubstitution::Any);
        RcTyp::type_var(ty_var_key)
    }

    pub(crate) fn apply(&self, ty: &RcTyp) -> RcTyp {
        match &*ty.borrow() {
            Typ::Concrete(concrete_type) => RcTyp::concrete(self.apply_on_concrete(concrete_type)),

            // Replace a type variable if it has a substitution
            Typ::TyVar(type_var_key) => {
                let substitution = &self.ty_vars[*type_var_key];

                match substitution {
                    TyVarSubstitution::Determined(rc_cell) => self.apply(&rc_cell),
                    TyVarSubstitution::Any | TyVarSubstitution::AnyOf(_) => ty.clone(),
                }
            }
        }
    }

    pub(crate) fn apply_on_concrete_rc(&self, concrete_type: &ConcreteTyp) -> RcTyp {
        RcTyp::concrete(self.apply_on_concrete(concrete_type))
    }

    pub(crate) fn apply_on_concrete(&self, concrete_type: &ConcreteTyp) -> ConcreteTyp {
        match concrete_type {
            ConcreteTyp::Composite(composite_type) => {
                ConcreteTyp::Composite(self.apply_on_composite(composite_type))
            }
            concrete_type @ _ => concrete_type.clone(),
        }
    }

    pub(crate) fn apply_on_composite(&self, composite_type: &CompositeTyp) -> CompositeTyp {
        match composite_type {
            // Recursively apply substitutions to concrete types
            CompositeTyp::Slice(inner) => CompositeTyp::Slice(self.apply(inner)),
            CompositeTyp::Ptr(inner) => CompositeTyp::Ptr(self.apply(inner)),
            CompositeTyp::Ref(inner) => CompositeTyp::Ref(self.apply(inner)),
            CompositeTyp::PtrMut(inner) => CompositeTyp::PtrMut(self.apply(inner)),
            CompositeTyp::RefMut(inner) => CompositeTyp::RefMut(self.apply(inner)),

            CompositeTyp::Array {
                underlying_typ,
                size,
            } => CompositeTyp::Array {
                underlying_typ: self.apply(&underlying_typ),
                size: *size,
            },

            CompositeTyp::Tuple { types } => CompositeTyp::Tuple {
                types: types.iter().map(|inner| self.apply(inner)).collect(),
            },

            CompositeTyp::Lambda {
                params_types,
                return_type,
            } => CompositeTyp::Lambda {
                params_types: params_types.iter().map(|param| self.apply(param)).collect(),
                return_type: self.apply(&return_type),
            },

            CompositeTyp::FnPtr {
                params_types,
                return_type,
            } => CompositeTyp::FnPtr {
                params_types: params_types.iter().map(|param| self.apply(param)).collect(),
                return_type: self.apply(&return_type),
            },
        }
    }

    pub(crate) fn try_unify(&mut self, t1: &RcTyp, t2: &RcTyp) -> Result<(), ()> {
        let old_state = self.ty_vars.clone();
        if let Err(err) = self.unify(t1, t2) {
            self.ty_vars = old_state;
            Err(err)
        } else {
            Ok(())
        }
    }

    pub(crate) fn unify(&mut self, t1: &RcTyp, t2: &RcTyp) -> Result<(), ()> {
        let t1 = self.apply(t1);
        let t2 = self.apply(t2);

        match (t1.inner(), t2.inner()) {
            // If both are concrete types, compare them recursively
            (Typ::Concrete(c1), Typ::Concrete(c2)) => self.unify_concrete(&c1, &c2),

            // If both are the same type variable, they're already unified
            (Typ::TyVar(key1), Typ::TyVar(key2)) if key1 == key2 => Ok(()),

            (Typ::TyVar(key1), Typ::TyVar(key2)) => {
                // Clone constraints for clarity.
                let sub1 = self.ty_vars[key1].clone();
                let sub2 = self.ty_vars[key2].clone();
                match (sub1, sub2) {
                    (TyVarSubstitution::Any, TyVarSubstitution::Any) => {
                        // Arbitrarily link key1 to key2.
                        self.ty_vars[key1] = TyVarSubstitution::Determined(RcTyp::type_var(key2));
                        Ok(())
                    }
                    (TyVarSubstitution::Any, TyVarSubstitution::AnyOf(allowed2)) => {
                        // key1 is unconstrained; adopt the constraints from key2.
                        self.ty_vars[key1] = TyVarSubstitution::Determined(RcTyp::type_var(key2));
                        // Optionally, key2 retains its AnyOf constraint.
                        Ok(())
                    }
                    (TyVarSubstitution::AnyOf(allowed1), TyVarSubstitution::Any) => {
                        // Symmetric case.
                        self.ty_vars[key2] = TyVarSubstitution::Determined(RcTyp::type_var(key1));
                        Ok(())
                    }
                    (TyVarSubstitution::AnyOf(allowed1), TyVarSubstitution::AnyOf(allowed2)) => {
                        let allowed1 = allowed1
                            .into_iter()
                            .map(|c| self.apply_on_concrete(&c))
                            .collect::<ThinVec<_>>();
                        let allowed2 = allowed2
                            .into_iter()
                            .map(|c| self.apply_on_concrete_rc(&c))
                            .collect::<ThinVec<_>>();

                        // Compute the intersection of both allowed sets.
                        let intersection = allowed1
                            .into_iter()
                            .filter(|typ| {
                                allowed2.iter().any(|alt| {
                                    self.try_unify(&alt, &RcTyp::concrete(typ.clone())).is_ok()
                                })
                            })
                            .collect::<ThinVec<_>>();

                        if intersection.is_empty() {
                            // No common alternative exists.
                            Err(())
                        } else if intersection.len() == 1 {
                            // The constraint is now fully determined.
                            self.ty_vars[key1] = TyVarSubstitution::Determined(RcTyp::concrete(
                                intersection[0].clone(),
                            ));
                            // Link the other variable to key1.
                            self.ty_vars[key2] =
                                TyVarSubstitution::Determined(RcTyp::type_var(key1));
                            Ok(())
                        } else {
                            // More than one possibility remains: update one variable with the intersected constraint.
                            self.ty_vars[key1] = TyVarSubstitution::AnyOf(intersection);
                            self.ty_vars[key2] =
                                TyVarSubstitution::Determined(RcTyp::type_var(key1));
                            Ok(())
                        }
                    }
                    // Should not reach here if Determined states are fully applied via `apply`.
                    _ => Err(()),
                }
            }

            (Typ::TyVar(key), Typ::Concrete(concrete_type))
            | (Typ::Concrete(concrete_type), Typ::TyVar(key)) => {
                // Ensure we do not introduce an infinite type.
                if concrete_type.occurs_check(key) {
                    return Err(());
                }
                match &self.ty_vars[key] {
                    TyVarSubstitution::Any => {
                        // No constraints; simply substitute.
                        self.ty_vars[key] =
                            TyVarSubstitution::Determined(RcTyp::concrete(concrete_type.clone()));
                        Ok(())
                    }
                    TyVarSubstitution::AnyOf(allowed) => {
                        let allowed = allowed
                            .into_iter()
                            .map(|c| self.apply_on_concrete_rc(c))
                            .collect::<ThinVec<_>>();
                        // Check if the concrete type is one of the allowed alternatives.
                        if allowed.into_iter().any(|alt| {
                            self.try_unify(&alt, &RcTyp::concrete(concrete_type.clone()))
                                .is_ok()
                        }) {
                            self.ty_vars[key] = TyVarSubstitution::Determined(RcTyp::concrete(
                                concrete_type.clone(),
                            ));
                            Ok(())
                        } else {
                            // The concrete type does not satisfy the constraint.
                            Err(())
                        }
                    }
                    TyVarSubstitution::Determined(_) => unreachable!(),
                }
            }
        }
    }

    pub(crate) fn unify_concrete(&mut self, c1: &ConcreteTyp, c2: &ConcreteTyp) -> Result<(), ()> {
        match (c1, c2) {
            (ConcreteTyp::Composite(c1), ConcreteTyp::Composite(c2)) => {
                self.unify_composite(c1, c2)
            }
            (ConcreteTyp::UnitStruct(k1), ConcreteTyp::UnitStruct(k2)) if k1 == k2 => Ok(()),
            (ConcreteTyp::TupleStruct(k1), ConcreteTyp::TupleStruct(k2)) if k1 == k2 => Ok(()),
            (ConcreteTyp::FieldsStruct(k1), ConcreteTyp::FieldsStruct(k2)) if k1 == k2 => Ok(()),
            (ConcreteTyp::Primitive(p1), ConcreteTyp::Primitive(p2)) if p1 == p2 => Ok(()),
            _ => Err(()),
        }
    }

    pub(crate) fn unify_composite(
        &mut self,
        c1: &CompositeTyp,
        c2: &CompositeTyp,
    ) -> Result<(), ()> {
        match (c1, c2) {
            (CompositeTyp::Slice(t1), CompositeTyp::Slice(t2))
            | (CompositeTyp::Ptr(t1), CompositeTyp::Ptr(t2))
            | (CompositeTyp::Ref(t1), CompositeTyp::Ref(t2))
            | (CompositeTyp::PtrMut(t1), CompositeTyp::PtrMut(t2))
            | (CompositeTyp::RefMut(t1), CompositeTyp::RefMut(t2)) => self.unify(&t1, &t2),

            (
                CompositeTyp::Array {
                    underlying_typ: t1,
                    size: s1,
                },
                CompositeTyp::Array {
                    underlying_typ: t2,
                    size: s2,
                },
            ) if s1 == s2 => self.unify(&t1, &t2),

            (CompositeTyp::Tuple { types: t1 }, CompositeTyp::Tuple { types: t2 })
                if t1.len() == t2.len() =>
            {
                for (t1, t2) in t1.iter().zip(t2.iter()) {
                    let t1 = &self.apply(t1);
                    let t2 = &self.apply(t2);
                    self.unify(t1, t2)?;
                }
                Ok(())
            }

            (
                CompositeTyp::Lambda {
                    params_types: params_types1,
                    return_type: return_type1,
                },
                CompositeTyp::Lambda {
                    params_types: params_types2,
                    return_type: return_type2,
                },
            )
            | (
                CompositeTyp::FnPtr {
                    params_types: params_types1,
                    return_type: return_type1,
                },
                CompositeTyp::FnPtr {
                    params_types: params_types2,
                    return_type: return_type2,
                },
            ) if params_types1.len() == params_types2.len() => {
                for (t1, t2) in params_types1.iter().zip(params_types2.iter()) {
                    let t1 = &self.apply(t1);
                    let t2 = &self.apply(t2);
                    self.unify(t1, t2)?;
                }
                let return_type1 = &self.apply(&return_type1);
                let return_type2 = &self.apply(&return_type2);
                self.unify(return_type1, return_type2)?;
                Ok(())
            }
            _ => Err(()),
        }
    }

    pub(crate) fn bind_type_var(&mut self, ty_var_key: TypeVarKey, ty: &RcTyp) -> Result<(), ()> {
        let substitution = &self.ty_vars[ty_var_key];

        match substitution {
            TyVarSubstitution::Any => {
                if ty.borrow().occurs_check(ty_var_key) {
                    Err(())
                } else {
                    self.ty_vars[ty_var_key] = TyVarSubstitution::Determined(ty.clone());
                    Ok(())
                }
            }
            TyVarSubstitution::AnyOf(thin_vec) => todo!(),
            TyVarSubstitution::Determined(rc_cell) => unreachable!(), // As calling apply in unify will return this determined type instead
        }
    }

    /// Constrain the given type variable (or type) to the allowed set of concrete types.
    /// Returns Ok(()) if the constraint is compatible, or Err(()) if the determined type
    /// is not among the allowed alternatives or the intersection is empty.
    pub fn constrain_type_var(
        &mut self,
        ty: &RcTyp,
        allowed: ThinVec<ConcreteTyp>,
    ) -> Result<(), ()> {
        // First, apply all substitutions to get the current “resolved” type.
        let applied = self.apply(ty);
        let allowed = allowed
            .into_iter()
            .map(|cand| self.apply_on_concrete(&cand))
            .collect();

        let result = match &*applied.borrow() {
            // If we have a type variable…
            Typ::TyVar(key) => {
                match &self.ty_vars[*key] {
                    // If unconstrained, just set the allowed set.
                    TyVarSubstitution::Any => {
                        self.ty_vars[*key] = TyVarSubstitution::AnyOf(allowed);
                        Ok(())
                    }
                    // If already constrained, intersect the new allowed set with the existing one.
                    TyVarSubstitution::AnyOf(existing) => {
                        let new_allowed: ThinVec<ConcreteTyp> = existing
                            .iter()
                            .filter(|cand| allowed.contains(cand))
                            .cloned()
                            .collect();
                        if new_allowed.is_empty() {
                            // No candidate is allowed by both constraints.
                            Err(())
                        } else {
                            self.ty_vars[*key] = TyVarSubstitution::AnyOf(new_allowed);
                            Ok(())
                        }
                    }
                    // If already determined, check that the concrete type is one of the allowed alternatives.
                    TyVarSubstitution::Determined(determined) => match &*determined.borrow() {
                        Typ::Concrete(c) if allowed.contains(c) => Ok(()),
                        _ => Err(()),
                    },
                }
            }
            // If the type is already concrete, verify that it is one of the allowed alternatives.
            Typ::Concrete(c) => {
                if allowed.contains(c) {
                    Ok(())
                } else {
                    Err(())
                }
            }
        };

        result
    }
}

#[cfg(test)]
mod tests {
    use thin_vec::thin_vec;

    use super::*;

    #[test]
    fn test_simple() {
        let mut infer = TyInfer::default();

        let t0 = infer.new_ty_var();
        let t1 = infer.new_ty_var();
        let t2 = infer.new_ty_var();
        let t3 = infer.new_ty_var();
        let expected_ty = RcTyp::i4();

        assert!(infer.unify(&t0, &t3).is_ok());
        assert!(infer.unify(&t1, &t2).is_ok());
        assert!(infer.unify(&t3, &t1).is_ok());

        assert!(infer
            .constrain_type_var(
                &t0,
                thin_vec![
                    ConcreteTyp::Primitive(PrimitiveTyp::I1),
                    ConcreteTyp::Primitive(PrimitiveTyp::I2),
                    ConcreteTyp::Primitive(PrimitiveTyp::I4),
                    ConcreteTyp::Primitive(PrimitiveTyp::I8),
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
        let expected_ty = RcTyp::i4();

        assert!(infer.unify(&t0, &t3).is_ok());
        assert!(infer.unify(&t1, &t2).is_ok());
        assert!(infer.unify(&t3, &t1).is_ok());

        assert!(infer
            .constrain_type_var(&t0, thin_vec![ConcreteTyp::Primitive(PrimitiveTyp::I4),],)
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
                    ConcreteTyp::Composite(CompositeTyp::FnPtr {
                        params_types: params_types.clone(),
                        return_type: return_type.clone()
                    }),
                    ConcreteTyp::Composite(CompositeTyp::Lambda {
                        params_types,
                        return_type
                    }),
                ]
            )
            .is_ok());

        assert!(infer.unify(&t1, &RcTyp::boolean()).is_ok());
        assert!(infer.unify(&t2, &RcTyp::i4()).is_ok());
        assert!(infer.unify(&t0, &RcTyp::i4()).is_err());
        assert!(infer
            .unify(
                &t0,
                &RcTyp::fn_ptr(thin_vec![RcTyp::boolean(), t4.clone()], t4.clone())
            )
            .is_ok());
        assert!(infer.unify(&RcTyp::i4(), &t4).is_ok());

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
                    ConcreteTyp::Composite(CompositeTyp::Ptr(t1.clone())),
                    ConcreteTyp::Composite(CompositeTyp::PtrMut(t1.clone())),
                    ConcreteTyp::Primitive(PrimitiveTyp::Bool),
                ]
            )
            .is_ok());

        assert!(infer.unify(&t1, &RcTyp::character()).is_ok());
        assert!(infer.unify(&t0, &RcTyp::i4()).is_err());
        assert!(infer.unify(&t0, &RcTyp::boolean()).is_ok());

        println!("{:#?}", infer.ty_vars)
    }
}
