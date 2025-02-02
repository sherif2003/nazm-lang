use std::collections::HashMap;

use nazmc_ast::FileKey;
use nazmc_data_pool::typed_index_collections::TiVec;
use nazmc_diagnostics::span::Span;

use crate::typed_ast::{ArrayType, ConcreteType, FnPtrType, LambdaType, Ty, Type, TypeVarKey};

#[derive(Debug, Default, Clone, Copy)]
pub(crate) enum TyVarState {
    #[default]
    Unknown,
    Never,
    UnspecifiedNumber,
    UnspecifiedSignedNumber,
    UnspecifiedUnsignedInt,
    UnspecifiedSignedInt,
    UnspecifiedFloat,
}

impl TyVarState {
    fn get_super_type(t1: TyVarState, t2: TyVarState) -> Option<TyVarState> {
        if Self::cmp(t1, t2) {
            Some(t2)
        } else if Self::cmp(t2, t1) {
            Some(t1)
        } else {
            None
        }
    }

    fn get_sub_type(t1: TyVarState, t2: TyVarState) -> Option<TyVarState> {
        if Self::cmp(t1, t2) {
            Some(t1)
        } else if Self::cmp(t2, t1) {
            Some(t2)
        } else {
            None
        }
    }

    fn cmp(sub: TyVarState, sup: TyVarState) -> bool {
        match (sub, sup) {
            (_, TyVarState::Unknown)
            | (TyVarState::Never, _)
            | (
                TyVarState::UnspecifiedFloat,
                TyVarState::UnspecifiedFloat
                | TyVarState::UnspecifiedNumber
                | TyVarState::UnspecifiedSignedNumber,
            )
            | (
                TyVarState::UnspecifiedUnsignedInt,
                TyVarState::UnspecifiedUnsignedInt | TyVarState::UnspecifiedNumber,
            )
            | (
                TyVarState::UnspecifiedSignedInt,
                TyVarState::UnspecifiedUnsignedInt
                | TyVarState::UnspecifiedSignedInt
                | TyVarState::UnspecifiedNumber
                | TyVarState::UnspecifiedSignedNumber,
            ) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Default)]
pub(crate) struct Substitution {
    pub(crate) all_ty_vars: TiVec<TypeVarKey, (TyVarState, FileKey, Span)>,
    pub(crate) substitutions: HashMap<TypeVarKey, Ty>,
}

pub(crate) enum TypeUnificationErr {
    CycleDetected { var_key: TypeVarKey, ty: Ty },
    CannotUnify { t1: Ty, t2: Ty },
    CannotUnifyTyVar { ty_var_key: TypeVarKey, ty: Ty },
}

impl Substitution {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn new_unknown_ty_var(&mut self, file_key: FileKey, span: Span) -> Ty {
        let ty_var_key = self
            .all_ty_vars
            .push_and_get_key((TyVarState::Unknown, file_key, span));
        Ty::type_var(ty_var_key)
    }

    pub(crate) fn new_never_ty_var(&mut self, file_key: FileKey, span: Span) -> Ty {
        let ty_var_key = self
            .all_ty_vars
            .push_and_get_key((TyVarState::Never, file_key, span));
        Ty::type_var(ty_var_key)
    }

    pub(crate) fn new_unspecified_unsigned_int_ty_var(
        &mut self,
        file_key: FileKey,
        span: Span,
    ) -> Ty {
        let ty_var_key =
            self.all_ty_vars
                .push_and_get_key((TyVarState::UnspecifiedUnsignedInt, file_key, span));
        Ty::type_var(ty_var_key)
    }

    pub(crate) fn new_unspecified_float_ty_var(&mut self, file_key: FileKey, span: Span) -> Ty {
        let ty_var_key =
            self.all_ty_vars
                .push_and_get_key((TyVarState::UnspecifiedFloat, file_key, span));
        Ty::type_var(ty_var_key)
    }

    /// Apply substitutions to a type, recursively replacing type variables
    /// with their substituted types, if present in the substitution map.
    pub(crate) fn apply(&self, ty: &Ty) -> Ty {
        match &*ty.borrow() {
            // No substitution for concrete types
            Type::Concrete(_) => ty.clone(),

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

        let t1 = self.all_ty_vars[ty_var_key].0;

        match (self.all_ty_vars[ty_var_key].0, &*ty.borrow()) {
            (_, Type::TypeVar(ty_var_key2)) => {
                let t2 = self.all_ty_vars[*ty_var_key2].0;
                if let Some(sub_type) = TyVarState::get_sub_type(t1, t2) {
                    self.all_ty_vars[ty_var_key].0 = sub_type;
                    self.all_ty_vars[*ty_var_key2].0 = sub_type;
                } else {
                    return Err(TypeUnificationErr::CannotUnifyTyVar {
                        ty_var_key: ty_var_key,
                        ty: ty.clone(),
                    });
                }
            }
            (TyVarState::Unknown | TyVarState::Never, _)
            | (
                TyVarState::UnspecifiedNumber,
                Type::Concrete(
                    ConcreteType::I
                    | ConcreteType::I1
                    | ConcreteType::I2
                    | ConcreteType::I4
                    | ConcreteType::I8
                    | ConcreteType::U
                    | ConcreteType::U1
                    | ConcreteType::U2
                    | ConcreteType::U4
                    | ConcreteType::U8
                    | ConcreteType::F4
                    | ConcreteType::F8,
                ),
            )
            | (
                TyVarState::UnspecifiedUnsignedInt,
                Type::Concrete(
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
                ),
            )
            | (
                TyVarState::UnspecifiedSignedInt,
                Type::Concrete(
                    ConcreteType::I
                    | ConcreteType::I1
                    | ConcreteType::I2
                    | ConcreteType::I4
                    | ConcreteType::I8,
                ),
            )
            | (TyVarState::UnspecifiedFloat, Type::Concrete(ConcreteType::F4 | ConcreteType::F8)) =>
                {}
            _ => {
                return Err(TypeUnificationErr::CannotUnifyTyVar {
                    ty_var_key: ty_var_key,
                    ty: ty.clone(),
                })
            }
        };

        self.substitutions.insert(ty_var_key, ty.clone());
        Ok(())
    }

    #[inline]
    pub(crate) fn check_map_to_unspecified_number(&self, ty_var_key: TypeVarKey) -> bool {
        matches!(
            self.all_ty_vars[ty_var_key].0,
            TyVarState::UnspecifiedNumber
                | TyVarState::UnspecifiedSignedNumber
                | TyVarState::UnspecifiedUnsignedInt
                | TyVarState::UnspecifiedSignedInt
                | TyVarState::UnspecifiedFloat
        )
    }

    pub(crate) fn collect(&mut self) -> HashMap<TypeVarKey, ()> {
        let ty_vars_len = self.all_ty_vars.len();
        let mut unknown_vars = HashMap::new();

        for i in 0..ty_vars_len {
            let ty_var_key: TypeVarKey = i.into();

            let final_ty = self.final_apply(&mut unknown_vars, &Ty::type_var(ty_var_key));

            self.substitutions.insert(ty_var_key, final_ty);
        }

        unknown_vars
    }

    fn final_apply(&mut self, unknown_vars: &mut HashMap<TypeVarKey, ()>, ty: &Ty) -> Ty {
        match &*ty.borrow() {
            Type::Concrete(concrete_type) => ty.clone(),
            Type::TypeVar(type_var_key) => {
                if let Some(substituted_ty) = self.substitutions.get(type_var_key) {
                    self.final_apply(unknown_vars, &substituted_ty.clone()) // Apply recursively in case of nested substitutions
                } else {
                    match self.all_ty_vars[*type_var_key].0 {
                        TyVarState::Unknown
                        | TyVarState::UnspecifiedNumber
                        | TyVarState::UnspecifiedSignedNumber => {
                            self.substitutions.insert(*type_var_key, Ty::never());
                            unknown_vars.insert(*type_var_key, ());
                            Ty::never()
                        }
                        TyVarState::Never => {
                            self.substitutions.insert(*type_var_key, Ty::never());
                            Ty::never()
                        }
                        TyVarState::UnspecifiedUnsignedInt | TyVarState::UnspecifiedSignedInt => {
                            self.substitutions.insert(*type_var_key, Ty::i4());
                            Ty::i4()
                        }
                        TyVarState::UnspecifiedFloat => {
                            self.substitutions.insert(*type_var_key, Ty::f4());
                            Ty::f4()
                        }
                    }
                }
            }

            // Recursively apply substitutions to concrete types
            Type::Slice(inner) => Ty::slice(self.final_apply(unknown_vars, inner)),
            Type::Ptr(inner) => Ty::ptr(self.final_apply(unknown_vars, inner)),
            Type::Ref(inner) => Ty::reference(self.final_apply(unknown_vars, inner)),
            Type::PtrMut(inner) => Ty::ptr_mut(self.final_apply(unknown_vars, inner)),
            Type::RefMut(inner) => Ty::ref_mut(self.final_apply(unknown_vars, inner)),

            Type::Array(array_type) => Ty::array(
                self.final_apply(unknown_vars, &array_type.underlying_typ),
                array_type.size,
            ),

            Type::Tuple(tuple_type) => Ty::tuple(
                tuple_type
                    .types
                    .iter()
                    .map(|inner| self.final_apply(unknown_vars, inner)),
            ),

            Type::Lambda(lambda_type) => {
                let retutn_type = self.final_apply(unknown_vars, &lambda_type.return_type);
                Ty::lambda(
                    lambda_type
                        .params_types
                        .iter()
                        .map(|param| self.final_apply(unknown_vars, param)),
                    retutn_type,
                )
            }

            Type::FnPtr(fn_ptr_type) => {
                let retutn_type = self.final_apply(unknown_vars, &fn_ptr_type.return_type);
                Ty::fn_ptr(
                    fn_ptr_type
                        .params_types
                        .iter()
                        .map(|param| self.final_apply(unknown_vars, param)),
                    retutn_type,
                )
            }
        }
    }
}
