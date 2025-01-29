use std::collections::HashMap;

use nazmc_data_pool::typed_index_collections::TiVec;

use crate::typed_ast::{ArrayType, ConcreteType, FnPtrType, LambdaType, Ty, Type, TypeVarKey};

#[derive(Debug, Default)]
pub struct Substitution {
    pub all_ty_vars: TiVec<TypeVarKey, Ty>,
    substitutions: HashMap<TypeVarKey, Ty>,
}

pub enum TypeUnificationErr {
    CycleDetected { var_key: TypeVarKey, ty: Ty },
    CannotUnify { t1: Ty, t2: Ty },
}

impl Substitution {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn new_unknown_ty_var(&mut self) -> Ty {
        let ty_var_key = self.all_ty_vars.push_and_get_key(Ty::new(Type::Unknown));
        Ty::type_var(ty_var_key)
    }

    pub(crate) fn new_never_ty_var(&mut self) -> Ty {
        let ty_var_key = self.all_ty_vars.push_and_get_key(Ty::new(Type::Never));
        Ty::type_var(ty_var_key)
    }

    pub(crate) fn new_unspecified_unsigned_int_ty_var(&mut self) -> Ty {
        let ty_var_key = self
            .all_ty_vars
            .push_and_get_key(Ty::new(Type::UnspecifiedUnsignedInt));
        Ty::type_var(ty_var_key)
    }

    pub(crate) fn new_unspecified_float_ty_var(&mut self) -> Ty {
        let ty_var_key = self
            .all_ty_vars
            .push_and_get_key(Ty::new(Type::UnspecifiedFloat));
        Ty::type_var(ty_var_key)
    }

    pub(crate) fn new_callable_ty_var(
        &mut self,
        params: impl IntoIterator<Item = Ty>,
        return_type: Ty,
    ) -> Ty {
        let ty_var_key = self
            .all_ty_vars
            .push_and_get_key(Ty::new(Type::Callable(FnPtrType {
                params_types: params.into_iter().collect(),
                return_type,
            })));
        Ty::type_var(ty_var_key)
    }

    /// Apply substitutions to a type, recursively replacing type variables
    /// with their substituted types, if present in the substitution map.
    pub(crate) fn apply(&self, ty: &Ty) -> Ty {
        match &*ty.borrow() {
            // No substitution for unknown types
            Type::Unknown
            | Type::Never
            | Type::UnspecifiedUnsignedInt
            | Type::UnspecifiedSignedInt
            | Type::UnspecifiedFloat
            | Type::Concrete(_) => ty.clone(),

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
                fn_ptr_type
                    .params_types
                    .iter()
                    .map(|param| self.apply(param)),
                self.apply(&fn_ptr_type.return_type),
            ),

            Type::Callable(fn_ptr_type) => Ty::callable(
                fn_ptr_type
                    .params_types
                    .iter()
                    .map(|param| self.apply(param)),
                self.apply(&fn_ptr_type.return_type),
            ),
        }
    }

    pub(crate) fn unify(&mut self, t1: &Ty, t2: &Ty) -> Result<(), TypeUnificationErr> {
        let t1 = self.apply(t1);
        let t2 = self.apply(t2);

        match (t1.inner(), t2.inner()) {
            // If both are concrete types, compare them recursively
            (Type::Concrete(c1), Type::Concrete(c2)) if c1 == c2 => Ok(()),

            // If both are the same type variable, they're already unified
            (Type::TypeVar(key1), Type::TypeVar(key2)) if key1 == key2 => Ok(()),

            // If t1 is a type variable, bind it to t2
            (Type::TypeVar(key), _) => self.bind_type_var(key, &t2),

            // If t2 is a type variable, bind it to t1
            (_, Type::TypeVar(key)) => self.bind_type_var(key, &t1),

            // For composite types, unify their components
            (Type::Slice(t1), Type::Slice(t2))
            | (Type::Ptr(t1), Type::Ptr(t2))
            | (Type::Ref(t1), Type::Ref(t2))
            | (Type::PtrMut(t1), Type::PtrMut(t2))
            | (Type::RefMut(t1), Type::RefMut(t2)) => self.unify(&t1, &t2),

            (
                Type::Array(ArrayType {
                    underlying_typ: t1,
                    size: s1,
                }),
                Type::Array(ArrayType {
                    underlying_typ: t2,
                    size: s2,
                }),
            ) if s1 == s2 => self.unify(&t1, &t2),

            (Type::Tuple(t1), Type::Tuple(t2)) if t1.types.len() == t2.types.len() => {
                for (t1, t2) in t1.types.iter().zip(t2.types.iter()) {
                    let t1 = &self.apply(t1);
                    let t2 = &self.apply(t2);
                    self.unify(t1, t2)?;
                }
                Ok(())
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

            // Other incompatible types
            _ => Err(TypeUnificationErr::CannotUnify {
                t1: t1.clone(),
                t2: t2.clone(),
            }),
        }
    }

    pub(crate) fn bind_type_var(
        &mut self,
        ty_var_key: TypeVarKey,
        ty: &Ty,
    ) -> Result<(), TypeUnificationErr> {
        // Check for cyclic references
        if ty.inner().contains_var(ty_var_key) {
            return Err(TypeUnificationErr::CycleDetected {
                var_key: ty_var_key,
                ty: ty.clone(),
            });
        }

        match self.all_ty_vars[ty_var_key].inner() {
            Type::Unknown | Type::Never => {
                self.substitutions.insert(ty_var_key, ty.clone());
            }

            Type::UnspecifiedUnsignedInt => {
                let (Type::Concrete(
                    ConcreteType::I
                    | ConcreteType::I1
                    | ConcreteType::I2
                    | ConcreteType::I4
                    | ConcreteType::I8
                    | ConcreteType::U
                    | ConcreteType::U1
                    | ConcreteType::U2
                    | ConcreteType::U4
                    | ConcreteType::U8,
                )
                | Type::UnspecifiedUnsignedInt
                | Type::UnspecifiedSignedInt) = &*ty.borrow()
                else {
                    return Err(TypeUnificationErr::CannotUnify {
                        t1: self.all_ty_vars[ty_var_key].clone(),
                        t2: ty.clone(),
                    });
                };
                self.substitutions.insert(ty_var_key, ty.clone());
            }

            Type::UnspecifiedSignedInt => {
                let (Type::Concrete(
                    ConcreteType::I
                    | ConcreteType::I1
                    | ConcreteType::I2
                    | ConcreteType::I4
                    | ConcreteType::I8,
                )
                | Type::UnspecifiedSignedInt) = &*ty.borrow()
                else {
                    return Err(TypeUnificationErr::CannotUnify {
                        t1: self.all_ty_vars[ty_var_key].clone(),
                        t2: ty.clone(),
                    });
                };
                self.substitutions.insert(ty_var_key, ty.clone());
            }

            Type::UnspecifiedFloat => {
                let (Type::Concrete(ConcreteType::F4 | ConcreteType::F8) | Type::UnspecifiedFloat) =
                    &*ty.borrow()
                else {
                    return Err(TypeUnificationErr::CannotUnify {
                        t1: self.all_ty_vars[ty_var_key].clone(),
                        t2: ty.clone(),
                    });
                };
                self.substitutions.insert(ty_var_key, ty.clone());
            }

            Type::Callable(FnPtrType {
                params_types: callable_params_types,
                return_type: callable_return_type,
            }) => {
                let (Type::FnPtr(FnPtrType {
                    params_types,
                    return_type,
                })
                | Type::Lambda(LambdaType {
                    params_types,
                    return_type,
                })
                | Type::Callable(FnPtrType {
                    params_types,
                    return_type,
                })) = &*ty.borrow()
                else {
                    return Err(TypeUnificationErr::CannotUnify {
                        t1: self.all_ty_vars[ty_var_key].clone(),
                        t2: ty.clone(),
                    });
                };

                let mut r_params = Ok(());

                for (param_ty, callable_param_ty) in params_types.iter().zip(&callable_params_types)
                {
                    let rr = self.unify(param_ty, &callable_param_ty);
                    if rr.is_err() {
                        r_params = rr;
                    }
                }

                let r = self.unify(return_type, &callable_return_type);

                let ty = match &*ty.borrow() {
                    Type::FnPtr(_) => Ty::fn_ptr(callable_params_types, callable_return_type),
                    Type::Lambda(_) => Ty::lambda(callable_params_types, callable_return_type),
                    Type::Callable(_) => Ty::callable(callable_params_types, callable_return_type),
                    _ => unreachable!(),
                };

                self.substitutions.insert(ty_var_key, ty);

                return if r.is_err() {
                    r
                } else if r_params.is_err() {
                    r_params
                } else {
                    Ok(())
                };
            }

            _ => unreachable!(),
        }

        Ok(())
    }
}
