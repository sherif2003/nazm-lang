use crate::{
    type_infer::{NumberConstraints, TypeVarSubstitution},
    *,
};

impl<'a> SemanticsAnalyzer<'a> {
    pub(crate) fn ty_var_check(&mut self, base_ty: &Type, span: Span, is_expr: bool) -> Type {
        let applied_ty = self.type_inf_ctx.apply(base_ty);
        self.ty_check(
            base_ty,
            &applied_ty,
            span,
            self.unknown_type_errors.len(),
            is_expr,
        );
        applied_ty
    }

    pub(crate) fn ty_check(
        &mut self,
        base_ty: &Type,
        applied_ty: &Type,
        span: Span,
        possible_new_err_msg_idx: usize,
        is_expr: bool,
    ) {
        match applied_ty {
            Type::TypeVar(key) => match &self.type_inf_ctx.ty_vars[*key] {
                TypeVarSubstitution::Never => {}
                TypeVarSubstitution::Any
                | TypeVarSubstitution::Error
                | TypeVarSubstitution::ConstrainedNumber(
                    NumberConstraints::Any | NumberConstraints::Signed,
                ) => {
                    if let Some(&err_msg_idx) = self.unknown_ty_vars.get(key) {
                        if self.unknown_type_errors[err_msg_idx].2.is_none() && is_expr {
                            self.unknown_type_errors[err_msg_idx].2 = Some(span)
                        }
                    } else {
                        let mut err_msg_idx = self.unknown_type_errors.len();
                        if err_msg_idx == possible_new_err_msg_idx {
                            self.unknown_type_errors.push((base_ty.clone(), span, None));
                        } else {
                            // May be the same type have multiple unknown type variables
                            // So the first one will push the error message to unknown_type_errors
                            // But the others need to share the same error message
                            // So it will be provied by possible_new_err_msg_idx
                            // And here it will be greater than unknown_type_errors.len()
                            err_msg_idx = possible_new_err_msg_idx;
                            if self.unknown_type_errors[err_msg_idx].2.is_none() && is_expr {
                                self.unknown_type_errors[err_msg_idx].2 = Some(span)
                            }
                        }
                        self.unknown_ty_vars.insert(*key, err_msg_idx);
                    }
                }
                TypeVarSubstitution::ConstrainedNumber(
                    NumberConstraints::Int | NumberConstraints::SignedInt,
                ) => {
                    self.type_inf_ctx.ty_vars[*key] = TypeVarSubstitution::Determined(Type::i4());
                }
                TypeVarSubstitution::ConstrainedNumber(NumberConstraints::Float) => {
                    self.type_inf_ctx.ty_vars[*key] = TypeVarSubstitution::Determined(Type::f4());
                }
                TypeVarSubstitution::Determined(determined) => self.ty_check(
                    base_ty,
                    &determined.clone(),
                    span,
                    possible_new_err_msg_idx,
                    is_expr,
                ),
            },
            Type::Concrete(con_ty) => {
                self.concrete_ty_check(base_ty, con_ty, span, possible_new_err_msg_idx, is_expr)
            }
        }
    }

    pub(crate) fn concrete_ty_check(
        &mut self,
        base_ty: &Type,
        con_ty: &ConcreteType,
        span: Span,
        possible_new_err_msg_idx: usize,
        is_expr: bool,
    ) {
        match con_ty {
            ConcreteType::Composite(comp_ty) => match comp_ty {
                CompositeType::Slice(underlying_typ)
                | CompositeType::Ptr(underlying_typ)
                | CompositeType::Ref(underlying_typ)
                | CompositeType::PtrMut(underlying_typ)
                | CompositeType::RefMut(underlying_typ)
                | CompositeType::Array {
                    underlying_typ,
                    size: _,
                } => self.ty_check(
                    base_ty,
                    underlying_typ,
                    span,
                    possible_new_err_msg_idx,
                    is_expr,
                ),
                CompositeType::Tuple { types } => types.iter().for_each(|ty| {
                    self.ty_check(base_ty, ty, span, possible_new_err_msg_idx, is_expr)
                }),
                CompositeType::Lambda {
                    params_types,
                    return_type,
                }
                | CompositeType::FnPtr {
                    params_types,
                    return_type,
                } => {
                    params_types.iter().for_each(|ty| {
                        self.ty_check(base_ty, ty, span, possible_new_err_msg_idx, is_expr)
                    });

                    self.ty_check(
                        base_ty,
                        return_type,
                        span,
                        possible_new_err_msg_idx,
                        is_expr,
                    );
                }
            },
            _ => {}
        }
    }

    pub(crate) fn check_scope_ty_vars(&mut self, scope_key: ScopeKey) {
        let stms = std::mem::take(&mut self.ast.scopes[scope_key].stms);
        for stm in &stms {
            match stm {
                Stm::Let(let_stm_key) => {
                    if self.ast.lets[*let_stm_key].binding.typ.is_none() {
                        self.ty_var_check(
                            &self.typed_ast.lets[let_stm_key].ty.clone(),
                            self.ast.lets[*let_stm_key].binding.kind.get_span(),
                            false,
                        );
                    }
                    if let Some(expr_key) = self.ast.lets[*let_stm_key].assign {
                        self.check_expr_ty_vars(expr_key);
                    }
                }
                Stm::While(while_stm) => {
                    self.check_expr_ty_vars(while_stm.cond_expr_key);
                    self.check_scope_ty_vars(while_stm.scope_key);
                }
                Stm::Expr(expr_key) => {
                    self.check_expr_ty_vars(*expr_key);
                }
            }
        }

        self.ast.scopes[scope_key].stms = stms;
    }

    pub(crate) fn check_expr_ty_vars(&mut self, expr_key: ExprKey) {
        let kind = std::mem::take(&mut self.ast.exprs[expr_key].kind);

        let kind = match kind {
            ExprKind::Call(call_expr) => {
                self.check_expr_ty_vars(call_expr.on);
                call_expr
                    .args
                    .iter()
                    .for_each(|&expr_key| self.check_expr_ty_vars(expr_key));

                ExprKind::Call(call_expr)
            }
            ExprKind::Field(field_expr) => {
                self.check_expr_ty_vars(field_expr.on);
                ExprKind::Field(field_expr)
            }
            ExprKind::Idx(idx_expr) => {
                self.check_expr_ty_vars(idx_expr.on);
                self.check_expr_ty_vars(idx_expr.idx);
                ExprKind::Idx(idx_expr)
            }
            ExprKind::TupleIdx(tuple_idx_expr) => {
                self.check_expr_ty_vars(tuple_idx_expr.on);
                ExprKind::TupleIdx(tuple_idx_expr)
            }
            ExprKind::Tuple(exprs) => {
                exprs
                    .iter()
                    .for_each(|&expr_key| self.check_expr_ty_vars(expr_key));

                ExprKind::Tuple(exprs)
            }
            ExprKind::ArrayElemnts(elements) => {
                elements
                    .iter()
                    .for_each(|&expr_key| self.check_expr_ty_vars(expr_key));

                ExprKind::ArrayElemnts(elements)
            }
            ExprKind::If(if_expr) => {
                self.check_expr_ty_vars(if_expr.if_.1);
                self.check_scope_ty_vars(if_expr.if_.2);

                for else_if in &if_expr.else_ifs {
                    self.check_expr_ty_vars(else_if.1);
                    self.check_scope_ty_vars(else_if.2);
                }

                if let Some(else_) = if_expr.else_ {
                    self.check_scope_ty_vars(else_.1);
                }

                ExprKind::If(if_expr)
            }
            ExprKind::Lambda(lambda_expr) => {
                let Some(
                    ref ty @ Type::Concrete(ConcreteType::Composite(CompositeType::Lambda {
                        ref params_types,
                        ref return_type,
                    })),
                ) = self.typed_ast.exprs.remove(&expr_key)
                else {
                    unreachable!()
                };

                for (i, param_type) in params_types.iter().enumerate() {
                    let binding = &lambda_expr.params[i];
                    if binding.typ.is_none() {
                        self.ty_var_check(param_type, binding.kind.get_span(), false);
                    }
                }

                self.ty_var_check(&return_type, self.get_expr_span(expr_key), false);

                self.check_scope_ty_vars(lambda_expr.body);

                // Early return as it will recheck the lambda params types and will set is_expr to true
                // Which will make the second span of the unknown type error message
                // will make it larger than the first span

                let ty = self.type_inf_ctx.apply(&ty);
                self.typed_ast.exprs.insert(expr_key, ty);

                self.ast.exprs[expr_key].kind = ExprKind::Lambda(lambda_expr);

                return;
            }
            ExprKind::UnaryOp(unary_op_expr) => {
                self.check_expr_ty_vars(unary_op_expr.expr);
                ExprKind::UnaryOp(unary_op_expr)
            }
            ExprKind::BinaryOp(binary_op_expr) => {
                self.check_expr_ty_vars(binary_op_expr.left);
                self.check_expr_ty_vars(binary_op_expr.right);
                ExprKind::BinaryOp(binary_op_expr)
            }
            ExprKind::Return(return_expr) => {
                if let Some(expr_key) = return_expr.expr {
                    self.check_expr_ty_vars(expr_key);
                }

                let ty = &self.typed_ast.exprs[&expr_key];
                let ty = self.type_inf_ctx.apply(&ty);
                self.typed_ast.exprs.insert(expr_key, ty);

                self.ast.exprs[expr_key].kind = ExprKind::Return(return_expr);

                // Eearly return as this should has never type but it might be changed to error
                // So the error must be reported where it is and not here
                return;
            }
            kind @ (ExprKind::Break | ExprKind::Continue) => {
                let ty = &self.typed_ast.exprs[&expr_key];
                let ty = self.type_inf_ctx.apply(&ty);
                self.typed_ast.exprs.insert(expr_key, ty);

                self.ast.exprs[expr_key].kind = kind;

                // Eearly return as this should has never type but it might be changed to error
                // So the error must be reported where it is and not here
                return;
            }
            ExprKind::TupleStruct(_) => todo!(),
            ExprKind::ArrayElemntsSized(_) => todo!(),
            ExprKind::On => todo!(),
            kind @ _ => kind,
        };

        self.ast.exprs[expr_key].kind = kind;
        let span = self.get_expr_span(expr_key);

        let Some(ty) = self.typed_ast.exprs.remove(&expr_key) else {
            unreachable!();
        };

        let ty = self.ty_var_check(&ty, span, true);

        self.typed_ast.exprs.insert(expr_key, ty.clone());
    }
}
