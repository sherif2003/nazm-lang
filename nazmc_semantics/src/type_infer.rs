use std::{
    collections::HashMap,
    ops::{Add, AddAssign},
};

use crate::typed_ast::{ArrayType, FnPtrType, LambdaType, Ty, Type, TypeVarKey};

#[derive(Debug, Default)]
pub struct Substitution {
    substitutions: HashMap<TypeVarKey, Ty>,
}

pub enum TypeUnificationErr {
    CycleDetected { var_key: TypeVarKey, ty: Ty },
    CannotUnify { t1: Ty, t2: Ty },
}

impl Substitution {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, ty_var_key: TypeVarKey, ty: Ty) {
        self.substitutions.insert(ty_var_key, ty);
    }

    /// Apply substitutions to a type, recursively replacing type variables
    /// with their substituted types, if present in the substitution map.
    pub fn apply(&self, ty: &Ty) -> Ty {
        match &*ty.borrow() {
            Type::Unknown | // No substitution for unknown types
            Type::UnspecifiedUnsignedInt |
            Type::UnspecifiedSignedInt |
            Type::UnspecifiedFloat|Type::Concrete(_) => ty.clone(),

            // Replace a type variable if it has a substitution
            Type::TypeVar(type_var_key) => {
                if let Some(substituted_ty) = self.substitutions.get(type_var_key) {
                    self.apply(substituted_ty) // Apply recursively in case of nested substitutions
                } else {
                    ty.clone() // No substitution, return as is
                }
            }

            // Recursively apply substitutions to concrete types
            Type::Slice(inner) => Ty::slice(self.apply(inner)),
                Type::Ptr(inner) => Ty::ptr(self.apply(inner)),
                Type::Ref(inner) => Ty::reference(self.apply(inner)),
                Type::PtrMut(inner) => Ty::ptr_mut(self.apply(inner)),
                Type::RefMut(inner) => Ty::ref_mut(self.apply(inner)),

                Type::Array(array_type) => {
                    Ty::array(self.apply(&array_type.underlying_typ), array_type.size)
                }

                Type::Tuple(tuple_type) => {
                    Ty::tuple(tuple_type.types.iter().map(|inner| self.apply(inner)))
                }

                Type::Lambda(lambda_type) => Ty::lambda(
                    lambda_type
                        .params_types
                        .iter()
                        .map(|param| self.apply(param)),
                    self.apply(&lambda_type.return_type),
                ),

                Type::FnPtr(fn_ptr_type) => Ty::fn_ptr(
                    fn_ptr_type.params_types.iter().map(|param| self.apply(param)),
                    self.apply(&fn_ptr_type.return_type),
                ),

        }
    }

    pub fn unify(t1: &Ty, t2: &Ty) -> Result<Substitution, TypeUnificationErr> {
        match (t1.inner(), t2.inner()) {
            // If both are concrete types, compare them recursively
            (Type::Concrete(c1), Type::Concrete(c2)) if c1 == c2 => Ok(Substitution::new()),

            // If both are the same type variable, they're already unified
            (Type::TypeVar(key1), Type::TypeVar(key2)) if key1 == key2 => Ok(Substitution::new()),

            // If t1 is a type variable, bind it to t2
            (Type::TypeVar(key), _) => Self::bind_type_var(key, t2),

            // If t2 is a type variable, bind it to t1
            (_, Type::TypeVar(key)) => Self::bind_type_var(key, &t1),

            // For composite types, unify their components
            (Type::Slice(t1), Type::Slice(t2))
            | (Type::Ptr(t1), Type::Ptr(t2))
            | (Type::Ref(t1), Type::Ref(t2))
            | (Type::PtrMut(t1), Type::PtrMut(t2))
            | (Type::RefMut(t1), Type::RefMut(t2)) => Substitution::unify(&t1, &t2),

            (
                Type::Array(ArrayType {
                    underlying_typ: t1,
                    size: s1,
                }),
                Type::Array(ArrayType {
                    underlying_typ: t2,
                    size: s2,
                }),
            ) if s1 == s2 => Substitution::unify(&t1, &t2),

            (Type::Tuple(t1), Type::Tuple(t2)) if t1.types.len() == t2.types.len() => {
                let mut substitution = Substitution::new();
                for (t1, t2) in t1.types.iter().zip(t2.types.iter()) {
                    let t1 = &substitution.apply(t1);
                    let t2 = &substitution.apply(t2);
                    substitution += Substitution::unify(t1, t2)?;
                }
                Ok(substitution)
            }

            (
                Type::Lambda(LambdaType {
                    params_types: params_types1,
                    return_type: return_type1,
                }),
                Type::Lambda(LambdaType {
                    params_types: params_types2,
                    return_type: return_type2,
                }),
            )
            | (
                Type::FnPtr(FnPtrType {
                    params_types: params_types1,
                    return_type: return_type1,
                }),
                Type::FnPtr(FnPtrType {
                    params_types: params_types2,
                    return_type: return_type2,
                }),
            ) if params_types1.len() == params_types2.len() => {
                let mut substitution = Substitution::new();
                for (t1, t2) in params_types1.iter().zip(params_types2.iter()) {
                    let t1 = &substitution.apply(t1);
                    let t2 = &substitution.apply(t2);
                    substitution += Substitution::unify(t1, t2)?;
                }
                let return_type1 = &substitution.apply(&return_type1);
                let return_type2 = &substitution.apply(&return_type2);
                substitution += Substitution::unify(return_type1, return_type2)?;
                Ok(substitution)
            }

            // Other incompatible types
            _ => Err(TypeUnificationErr::CannotUnify {
                t1: t1.clone(),
                t2: t2.clone(),
            }),
        }
    }

    fn bind_type_var(ty_var_key: TypeVarKey, ty: &Ty) -> Result<Substitution, TypeUnificationErr> {
        // Check for cyclic references
        if ty.inner().contains_var(ty_var_key) {
            return Err(TypeUnificationErr::CycleDetected {
                var_key: ty_var_key,
                ty: ty.clone(),
            });
        }

        let mut substitution = Substitution::new();

        substitution.substitutions.insert(ty_var_key, ty.clone());

        Ok(substitution)
    }
}

impl Add for Substitution {
    type Output = Substitution;

    fn add(mut self, rhs: Self) -> Self::Output {
        self.add_assign(rhs);
        self
    }
}

impl AddAssign for Substitution {
    fn add_assign(&mut self, rhs: Self) {
        let result = rhs
            .substitutions
            .into_iter()
            .map(|(tv, ty)| (tv, self.apply(&ty)))
            .collect::<HashMap<_, _>>();
        self.substitutions.extend(result);
    }
}
