use nazmc_diagnostics::span::SpanCursor;

use crate::{
    ty_infer::{CompositeType, ConcreteType, Type},
    typed_ast::{ArrayType, FieldInfo, LambdaParams},
    *,
};

use thin_vec::thin_vec;

impl<'a> SemanticsAnalyzer<'a> {
    pub(crate) fn infer_scope(&mut self, scope_key: ScopeKey) -> Ty {
        self.analyze_scope(scope_key);

        let return_ty = self.ast.scopes[scope_key]
            .return_expr
            .map_or_else(|| Ty::unit(), |expr_key| self.infer(expr_key));

        return_ty
    }

    pub(crate) fn infer(&mut self, expr_key: ExprKey) -> Ty {
        let ty = match &self.ast.exprs[expr_key].kind {
            ExprKind::Unit => Ty::unit(),
            ExprKind::Literal(lit_expr) => self.infer_lit_expr(*lit_expr, expr_key),
            ExprKind::PathNoPkg(path_no_pkg_key) => self.infer_path_no_pkg_expr(*path_no_pkg_key),
            ExprKind::PathInPkg(path_with_pkg_key) => {
                self.infer_path_with_pkg_expr(*path_with_pkg_key)
            }
            ExprKind::UnitStruct(unit_struct_path_key) => {
                let key = self.ast.state.unit_structs_paths_exprs[*unit_struct_path_key];
                Ty::unit_struct(key)
            }
            ExprKind::Tuple(thin_vec) => {
                let types = thin_vec
                    .clone() // TODO: Remove the clone
                    .into_iter()
                    .map(|expr_key| self.infer(expr_key));
                Ty::tuple(types)
            }
            ExprKind::Call(call_expr) => self.infer_call_expr(&call_expr.clone(), expr_key), // TODO: Remove the clone
            ExprKind::Idx(idx_expr) => self.infer_idx_expr(&idx_expr.clone(), expr_key), // TODO: Remove the clone
            ExprKind::ArrayElemnts(elements) => {
                self.infer_array_elements(&elements.clone(), expr_key) // TODO: Remove the clone
            }
            ExprKind::TupleIdx(tuple_idx_expr) => {
                self.infer_tuple_idx_expr(&tuple_idx_expr.clone(), expr_key) // TODO: Remove the clone
            }
            ExprKind::FieldsStruct(fields_struct_expr) => {
                // TODO: Remove the clone
                self.infer_fields_struct_expr(&fields_struct_expr.clone(), expr_key)
            }
            ExprKind::If(if_expr) => self.infer_if_expr(&if_expr.clone()), // TODO: Remove the clone
            ExprKind::Field(field_expr) => {
                self.infer_field_expr(&field_expr.clone(), expr_key) // TODO: Remove the clone
            }
            ExprKind::Lambda(lambda_expr) => self.infer_lambda_expr(&lambda_expr.clone()), // TODO: Remove the clone
            ExprKind::TupleStruct(tuple_struct_expr) => todo!(),
            ExprKind::ArrayElemntsSized(array_elements_sized_expr) => todo!(),
            ExprKind::UnaryOp(unary_op_expr) => {
                // TODO: Remove the clone
                self.infer_unary_op_expr(&unary_op_expr.clone(), expr_key)
            }
            ExprKind::BinaryOp(binary_op_expr) => self.infer_bin_op_expr(&binary_op_expr.clone()), // TODO: Remove the clone
            ExprKind::On => todo!(),
            ExprKind::Break | ExprKind::Continue => self.s.new_ty_var(),
            ExprKind::Return(return_expr) => self.infer_return_expr(&return_expr.clone()), // TODO: Remove the clone
        };

        self.typed_ast.exprs.insert(expr_key, ty.clone());

        ty
    }

    fn infer_lit_expr(&mut self, lit_expr: LiteralExpr, expr_key: ExprKey) -> Ty {
        match lit_expr {
            LiteralExpr::Str(_) => Ty::reference(Ty::string()),
            LiteralExpr::Char(_) => Ty::character(),
            LiteralExpr::Bool(_) => Ty::boolean(),
            LiteralExpr::Num(num_kind) => match num_kind {
                NumKind::F4(_) => Ty::f4(),
                NumKind::F8(_) => Ty::f8(),
                NumKind::I(_) => Ty::i(),
                NumKind::I1(_) => Ty::i1(),
                NumKind::I2(_) => Ty::i2(),
                NumKind::I4(_) => Ty::i4(),
                NumKind::I8(_) => Ty::i8(),
                NumKind::U(_) => Ty::u(),
                NumKind::U1(_) => Ty::u1(),
                NumKind::U2(_) => Ty::u2(),
                NumKind::U4(_) => Ty::u4(),
                NumKind::U8(_) => Ty::u8(),
                NumKind::UnspecifiedInt(_) => {
                    let ty_var = self.s.new_ty_var();
                    let _ = self
                        .s
                        .constrain_type_var(&ty_var, TyInfer::unspecified_int_constraints());
                    return ty_var;
                }
                NumKind::UnspecifiedFloat(_) => {
                    let ty_var = self.s.new_ty_var();
                    let _ = self
                        .s
                        .constrain_type_var(&ty_var, TyInfer::unspecified_float_constraints());
                    return ty_var;
                }
            },
        }
    }

    fn infer_path_no_pkg_expr(&mut self, path_no_pkg_key: PathNoPkgKey) -> Ty {
        let item = self.ast.state.paths_no_pkgs_exprs[path_no_pkg_key];

        let typ = match item {
            Item::Const { vis, key } => todo!(),
            Item::Static { vis, key } => todo!(),
            Item::Fn { vis, key } => &self.typed_ast.fns_signatures[&key],
            Item::LocalVar { id, key } => self
                .typed_ast
                .lets
                .get(&key)
                .unwrap()
                .bindings
                .get(&id)
                .unwrap(),
            Item::FnParam { idx, fn_key: _ } => {
                let Type::Concrete(ConcreteType::Composite(CompositeType::FnPtr {
                    params_types,
                    return_type: _,
                })) = &*self.typed_ast.fns_signatures[&self.current_fn_key].borrow()
                else {
                    unreachable!()
                };

                return params_types[idx as usize].clone();
            }
            Item::LambdaParam { id, scope_key } => self
                .typed_ast
                .lambdas_params
                .get(&scope_key)
                .unwrap()
                .bindings
                .get(&id)
                .unwrap(),
            _ => unreachable!(),
        };

        typ.clone()
    }

    fn infer_path_with_pkg_expr(&mut self, path_with_pkg_key: PathWithPkgKey) -> Ty {
        let item = self.ast.state.paths_with_pkgs_exprs[path_with_pkg_key];

        let typ: &Ty = match item {
            Item::Const { vis, key } => todo!(),
            Item::Static { vis, key } => todo!(),
            Item::Fn { vis, key } => &self.typed_ast.fns_signatures[&key],
            _ => unreachable!(),
        };

        typ.clone()
    }

    fn infer_call_expr(
        &mut self,
        CallExpr {
            on,
            args,
            parens_span,
        }: &CallExpr,
        expr_key: ExprKey,
    ) -> Ty {
        let on = *on;
        let parens_span = *parens_span;

        let on_expr_ty = self.infer(on);
        let on_expr_ty = self.s.apply(&on_expr_ty);

        let (params_types, return_type, is_fn) = match on_expr_ty.inner() {
            Type::Concrete(ConcreteType::Composite(CompositeType::FnPtr {
                params_types,
                return_type,
            })) => (params_types, return_type, true),
            Type::Concrete(ConcreteType::Composite(CompositeType::Lambda {
                params_types,
                return_type,
            })) => (params_types, return_type, false),
            _ => {
                let params_types = args
                    .iter()
                    .map(|expr_key| self.s.new_ty_var())
                    .collect::<ThinVec<_>>();
                let return_type = self.s.new_ty_var();

                if let Err(err) = self.s.constrain_type_var(
                    &on_expr_ty,
                    thin_vec![
                        ConcreteType::Composite(CompositeType::FnPtr {
                            params_types: params_types.clone(),
                            return_type: return_type.clone()
                        }),
                        ConcreteType::Composite(CompositeType::Lambda {
                            params_types: params_types.clone(),
                            return_type: return_type.clone()
                        }),
                    ],
                ) {
                    self.add_calling_non_callable_err(
                        &on_expr_ty,
                        self.get_expr_span(on),
                        parens_span,
                    );
                    return return_type;
                }

                (params_types, return_type, true)
            }
        };

        if params_types.len() == args.len() {
            for i in 0..args.len() {
                let arg_ty = self.infer(args[i]);
                if let Err(err) = self.s.unify(&params_types[i], &arg_ty) {
                    self.add_incorrect_fn_args_err(
                        &params_types[i],
                        &arg_ty,
                        self.get_expr_span(args[i]),
                        i,
                        on,
                        is_fn,
                    );
                }
            }
        } else {
            // Infer more types to collect more errors
            for i in 0..args.len() {
                let arg_ty = self.infer(args[i]);
                if i < params_types.len() {
                    if let Err(err) = self.s.unify(&params_types[i], &arg_ty) {
                        self.add_incorrect_fn_args_err(
                            &params_types[i],
                            &arg_ty,
                            self.get_expr_span(args[i]),
                            i,
                            on,
                            is_fn,
                        );
                    }
                }
            }

            self.add_incorrect_fn_args_len_err(parens_span, on, true);
        }

        return_type
    }

    fn infer_idx_expr(
        &mut self,
        IdxExpr {
            on,
            idx,
            brackets_span,
        }: &IdxExpr,
        expr_key: ExprKey,
    ) -> Ty {
        let on = *on;
        let brackets_span = *brackets_span;

        let on_expr_ty = self.infer(on);
        let on_expr_ty = self.s.apply(&on_expr_ty);

        let (underlying_ty, is_array) = match on_expr_ty.inner() {
            Type::Concrete(ConcreteType::Composite(CompositeType::Array {
                underlying_typ,
                size: _,
            })) => (underlying_typ, true),
            Type::Concrete(ConcreteType::Composite(CompositeType::Slice(underlying_ty))) => {
                (underlying_ty, false)
            }
            _ => {
                let underlying_ty = self.s.new_ty_var();

                if let Err(err) = self.s.constrain_type_var(
                    &on_expr_ty,
                    // Cannot constraint to array types
                    thin_vec![ConcreteType::Composite(CompositeType::Slice(
                        underlying_ty.clone()
                    ))],
                ) {
                    self.add_indexing_non_indexable_err(&on_expr_ty, on, brackets_span);
                }

                return underlying_ty;
            }
        };

        let idx_ty = self.infer(*idx);

        // TODO: Support ranges idexing
        if let Err(err) = self.s.unify(&Ty::u(), &idx_ty) {
            self.add_type_mismatch_err(&Ty::u(), &idx_ty, self.get_expr_span(*idx));
        }

        underlying_ty
    }

    fn infer_array_elements(&mut self, elements: &ThinVec<ExprKey>, expr_key: ExprKey) -> Ty {
        if elements.is_empty() {
            let unknown_ty = self.s.new_ty_var();
            return Ty::array(unknown_ty, 0);
        }

        let first_ty = self.infer(elements[0]);

        for &elem in &elements[1..] {
            let elem_ty = self.infer(elem);
            if let Err(_) = self.s.unify(&first_ty, &elem_ty) {
                self.add_array_element_type_mismatch_err(
                    &first_ty,
                    &elem_ty,
                    self.get_expr_span(elements[0]),
                    self.get_expr_span(elem),
                );
            }
        }

        Ty::array(first_ty, elements.len() as u32)
    }

    fn infer_tuple_idx_expr(
        &mut self,
        TupleIdxExpr { on, idx, idx_span }: &TupleIdxExpr,
        expr_key: ExprKey,
    ) -> Ty {
        let on = *on;
        let idx = *idx;

        let on_expr_ty = self.infer(on);
        let on_expr_ty = self.s.apply(&on_expr_ty);

        match on_expr_ty.inner() {
            Type::Concrete(ConcreteType::Composite(CompositeType::Tuple { types })) => {
                if idx < types.len() {
                    types[idx].clone()
                } else {
                    self.add_out_of_bounds_tuple_idx_err(idx, types.len(), *idx_span);
                    self.s.new_ty_var()
                }
            }
            _ => {
                self.add_indexing_non_tuple_err(&on_expr_ty, on, *idx_span);
                self.s.new_ty_var()
            }
        }
    }

    fn infer_fields_struct_expr(
        &mut self,
        FieldsStructExpr {
            path_key,
            fields: fields_exprs,
        }: &FieldsStructExpr,
        expr_key: ExprKey,
    ) -> Ty {
        let struct_key = self.ast.state.field_structs_paths_exprs[*path_key];

        let mut struct_fields = self.typed_ast.fields_structs[&struct_key].fields.clone();

        let mut used_fields = HashMap::with_capacity(struct_fields.len());

        for (field_id_expr, field_expr_key) in fields_exprs {
            let field_expr_ty = self.infer(*field_expr_key);

            if let Some(used_span) = used_fields.get(&field_id_expr.id) {
                self.add_field_is_used_more_than_once_err(
                    struct_key,
                    field_id_expr.id,
                    *used_span,
                    field_id_expr.span,
                );
            } else if let Some(FieldInfo {
                offset: _,
                typ: field_ty,
                idx,
            }) = struct_fields.get(&field_id_expr.id)
            {
                if let Err(err) = self.s.unify(&field_ty, &field_expr_ty) {
                    self.add_field_type_mismatch_err(
                        &field_ty,
                        &field_expr_ty,
                        struct_key,
                        *idx,
                        field_id_expr.span,
                        self.get_expr_span(*field_expr_key),
                    );
                }

                self.check_field_is_accessible_in_current_file(
                    struct_key,
                    *idx,
                    field_id_expr.span,
                );

                used_fields.insert(
                    field_id_expr.id,
                    field_id_expr
                        .span
                        .merged_with(&self.get_expr_span(*field_expr_key)),
                );

                struct_fields.remove(&field_id_expr.id);
            } else {
                self.add_unknown_field_in_struct_expr_err(
                    struct_key,
                    field_id_expr.id,
                    field_id_expr.span,
                );
            }
        }

        if !struct_fields.is_empty() {
            self.add_missing_fields_in_struct_expr_err(
                struct_key,
                struct_fields.iter().map(|(id, _)| *id).collect(),
                self.get_expr_span(expr_key),
            );
        }

        Ty::fields_struct(struct_key)
    }

    fn check_field_is_accessible_in_current_file(
        &mut self,
        struct_key: FieldsStructKey,
        field_idx: u32,
        field_id_expr_span: Span,
    ) {
        let struct_file_key = self.ast.fields_structs[struct_key].info.file_key;
        let struct_pkg_key = self.files_to_pkgs[struct_file_key];
        let current_file_pkg_key = self.files_to_pkgs[self.current_file_key];

        let vis = self.ast.fields_structs[struct_key].fields[field_idx as usize].vis;

        if matches!(vis, VisModifier::Private) && struct_file_key != self.current_file_key
            || matches!(vis, VisModifier::Default) && struct_pkg_key != current_file_pkg_key
        {
            self.add_filed_is_inaccessable_err(struct_key, field_idx, field_id_expr_span);
        }
    }

    pub(crate) fn infer_if_expr(
        &mut self,
        IfExpr {
            if_: (if_keyword_span, if_cond_expr_key, if_scope_key),
            else_ifs,
            else_,
        }: &IfExpr,
    ) -> Ty {
        let if_cond_ty = self.infer(*if_cond_expr_key);

        if let Err(err) = self.s.unify(&Ty::boolean(), &if_cond_ty) {
            self.add_branch_stm_condition_type_mismatch_err(
                &if_cond_ty,
                "لو",
                *if_keyword_span,
                *if_cond_expr_key,
            );
        }

        let if_ty = self.infer_scope(*if_scope_key);

        for (else_if_keyword_span, else_if_cond_expr_key, else_if_scope_key) in else_ifs {
            let else_if_cond_ty = self.infer(*if_cond_expr_key);

            if let Err(err) = self.s.unify(&Ty::boolean(), &else_if_cond_ty) {
                self.add_branch_stm_condition_type_mismatch_err(
                    &else_if_cond_ty,
                    "وإلا لو",
                    *else_if_keyword_span,
                    *else_if_cond_expr_key,
                );
            }

            let else_if_ty = self.infer_scope(*else_if_scope_key);

            if let Err(err) = self.s.unify(&if_ty, &else_if_ty) {
                self.add_type_mismatch_in_if_branches_err(
                    &if_ty,
                    &else_if_ty,
                    *if_scope_key,
                    *else_if_scope_key,
                    *if_keyword_span,
                    *else_if_keyword_span,
                );
            }
        }

        if let Some((else_keyword_span, else_scope_key)) = else_ {
            let else_ty = self.infer_scope(*else_scope_key);

            if let Err(err) = self.s.unify(&if_ty, &else_ty) {
                self.add_type_mismatch_in_if_branches_err(
                    &if_ty,
                    &else_ty,
                    *if_scope_key,
                    *else_scope_key,
                    *if_keyword_span,
                    *else_keyword_span,
                );
            }
        } else if let Err(err) = self.s.unify(&Ty::unit(), &if_ty) {
            self.add_missing_else_branch_err(&if_ty, *if_keyword_span, *if_scope_key);
        }

        if_ty
    }

    fn infer_field_expr(&mut self, FieldExpr { on, name }: &FieldExpr, expr_key: ExprKey) -> Ty {
        let on = *on;

        let on_expr_ty = self.infer(on);
        let on_expr_ty = self.s.apply(&on_expr_ty);

        // TODO: Support methods and the length method on slices
        match on_expr_ty.inner() {
            Type::Concrete(ConcreteType::FieldsStruct(struct_key)) => {
                let struct_fields = &self.typed_ast.fields_structs[&struct_key].fields;
                if let Some(FieldInfo {
                    offset: _,
                    typ,
                    idx,
                }) = struct_fields.get(&name.id)
                {
                    let ty = typ.clone();
                    self.check_field_is_accessible_in_current_file(struct_key, *idx, name.span);
                    ty
                } else {
                    self.add_unknown_field_in_struct_expr_err(struct_key, name.id, name.span);
                    self.s.new_ty_var()
                }
            }
            _ => {
                self.add_type_doesnt_have_fields_err(&on_expr_ty, on, *name);
                self.s.new_ty_var()
            }
        }
    }

    fn infer_return_expr(
        &mut self,
        ReturnExpr {
            return_keyword_span,
            expr,
        }: &ReturnExpr,
    ) -> Ty {
        let (found_return_ty, span) = expr.map_or_else(
            || (Ty::unit(), *return_keyword_span),
            |expr_key| {
                let span = self.get_expr_span(expr_key);
                if self.current_lambda_scope_key.is_some()
                    && self.current_lambda_first_implicit_return_ty_span.is_none()
                {
                    self.current_lambda_first_implicit_return_ty_span = Some(span);
                }
                (self.infer(expr_key), span)
            },
        );

        if let Err(err) = self
            .s
            .unify(&self.current_scope_expected_return_ty, &found_return_ty)
        {
            if let Some(lambda_scope_key) = self.current_lambda_scope_key {
                self.add_type_mismatch_in_lambda_return_ty_err(
                    lambda_scope_key,
                    &self.current_scope_expected_return_ty.clone(),
                    &found_return_ty,
                    span,
                );
            } else {
                let _fn = &self.ast.fns[self.current_fn_key];

                self.add_type_mismatch_in_fn_return_ty_err(
                    self.current_fn_key,
                    &self.current_scope_expected_return_ty.clone(),
                    &found_return_ty,
                    span,
                );
            }
        }

        self.s.new_ty_var()
    }

    fn infer_lambda_expr(&mut self, LambdaExpr { params, body }: &LambdaExpr) -> Ty {
        let lambda_scope_key = *body;
        let curly_braces_span = self.ast.scopes[lambda_scope_key].span;

        self.typed_ast.lambdas_params.insert(
            lambda_scope_key,
            LambdaParams {
                bindings: HashMap::new(),
            },
        );

        let mut params_tys = ThinVec::with_capacity(params.len());

        for Binding { kind, typ } in params {
            let binding_ty = if let Some(type_expr_key) = typ {
                self.analyze_type_expr(*type_expr_key).0
            } else {
                self.s.new_ty_var()
            };

            self.set_bindnig_ty_for_lambda(lambda_scope_key, &kind, &binding_ty);

            params_tys.push(binding_ty);
        }

        let outer_scope_expected_return_ty = self.current_scope_expected_return_ty.clone();
        let outer_lambda_first_return_ty_span = self.current_lambda_first_implicit_return_ty_span;
        let outer_scope_key = self.current_lambda_scope_key;

        self.current_scope_expected_return_ty = self.s.new_ty_var();
        self.current_lambda_first_implicit_return_ty_span = None;
        self.current_lambda_scope_key = Some(lambda_scope_key);

        let last_expr_return_ty = self.infer_scope(lambda_scope_key);

        let expected_lambda_return_ty = self.current_scope_expected_return_ty.clone();

        if let Err(err) = self
            .s
            .unify(&expected_lambda_return_ty, &last_expr_return_ty)
        {
            let span = if let Some(return_expr_key) = self.ast.scopes[lambda_scope_key].return_expr
            {
                self.get_expr_span(return_expr_key)
            } else {
                curly_braces_span
            };
            self.add_type_mismatch_in_lambda_return_ty_err(
                lambda_scope_key,
                &expected_lambda_return_ty,
                &last_expr_return_ty,
                span,
            );
        }

        self.current_scope_expected_return_ty = outer_scope_expected_return_ty;
        self.current_lambda_first_implicit_return_ty_span = outer_lambda_first_return_ty_span;
        self.current_lambda_scope_key = outer_scope_key;

        Ty::lambda(params_tys, expected_lambda_return_ty)
    }

    fn set_bindnig_ty_for_lambda(
        &mut self,
        lambda_scope_key: ScopeKey,
        kind: &BindingKind,
        ty: &Ty,
    ) {
        match kind {
            BindingKind::Id(id) => {
                self.typed_ast
                    .lambdas_params
                    .get_mut(&lambda_scope_key)
                    .unwrap()
                    .bindings
                    .insert(id.id, ty.clone());
            }
            BindingKind::MutId { id, .. } => {
                self.typed_ast
                    .lambdas_params
                    .get_mut(&lambda_scope_key)
                    .unwrap()
                    .bindings
                    .insert(id.id, ty.clone());
            }
            BindingKind::Tuple(kinds, span) => {
                if let Type::Concrete(ConcreteType::Composite(CompositeType::Tuple { types })) =
                    ty.inner()
                {
                    if kinds.len() == types.len() {
                        for i in 0..kinds.len() {
                            let kind = &kinds[i];
                            let ty = &types[i];
                            self.set_bindnig_ty_for_lambda(lambda_scope_key, kind, ty);
                        }
                    } else {
                        let found_ty = self.destructed_tuple_to_ty_with_unknown_for_lambda(
                            lambda_scope_key,
                            &kinds,
                        );
                        self.add_type_mismatch_err(ty, &found_ty, *span);
                    }
                } else {
                    let found_ty = self
                        .destructed_tuple_to_ty_with_unknown_for_lambda(lambda_scope_key, &kinds);
                    if let Err(err) = self.s.unify(ty, &found_ty) {
                        self.add_type_mismatch_err(&ty, &found_ty, *span);
                    }
                }
            }
        }
    }

    fn destructed_tuple_to_ty_with_unknown_for_lambda(
        &mut self,
        lambda_scope_key: ScopeKey,
        kinds: &[BindingKind],
    ) -> Ty {
        let mut tuple_types = ThinVec::with_capacity(kinds.len());
        for i in 0..kinds.len() {
            let kind = &kinds[i];
            let ty = self.s.new_ty_var();
            self.set_bindnig_ty_for_lambda(lambda_scope_key, kind, &ty);
            tuple_types.push(ty);
        }

        Ty::tuple(tuple_types)
    }

    fn infer_unary_op_expr(
        &mut self,
        UnaryOpExpr { op, op_span, expr }: &UnaryOpExpr,
        expr_key: ExprKey,
    ) -> Ty {
        let inner = self.infer(*expr);

        match op {
            UnaryOp::Minus => {
                if let Err(err) = self
                    .s
                    .constrain_type_var(&inner, TyInfer::unspecified_signed_number_constraints())
                {
                    self.add_type_mismatch_in_op_err(&Ty::i4(), &inner, *expr, *op_span);
                    Ty::i4()
                } else {
                    inner
                }
            }
            UnaryOp::LNot => {
                self.unify_with_int_num(&Ty::boolean(), *expr, &op_span);
                Ty::boolean()
            }
            UnaryOp::BNot => {
                if self.unify_with_int_num(&inner, *expr, &op_span) {
                    inner
                } else {
                    Ty::i4()
                }
            }
            UnaryOp::Deref => {
                let found_ty = self.s.apply(&inner);
                match found_ty.inner() {
                    Type::Concrete(ConcreteType::Composite(CompositeType::Ptr(underlying_ty)))
                    | Type::Concrete(ConcreteType::Composite(CompositeType::PtrMut(
                        underlying_ty,
                    ))) => underlying_ty,
                    _ => {
                        let underlying_ty = self.s.new_ty_var();

                        if let Err(err) = self.s.constrain_type_var(
                            &inner,
                            thin_vec![
                                ConcreteType::Composite(CompositeType::Ptr(underlying_ty.clone())),
                                ConcreteType::Composite(CompositeType::PtrMut(
                                    underlying_ty.clone()
                                ))
                            ],
                        ) {
                            self.add_cannot_deref_type(&found_ty, *expr, *op_span);
                        }
                        underlying_ty
                    }
                }
            }
            UnaryOp::Borrow => Ty::ptr(inner),
            UnaryOp::BorrowMut => Ty::ptr_mut(inner),
        }
    }

    fn infer_bin_op_expr(
        &mut self,
        BinaryOpExpr {
            op,
            op_span_cursor,
            left,
            right,
        }: &BinaryOpExpr,
    ) -> Ty {
        let left_ty = self.infer(*left);

        let op_len = match op {
            BinOp::OpenOpenRange => 4,
            BinOp::CloseOpenRange | BinOp::OpenCloseRange | BinOp::ShlAssign | BinOp::ShrAssign => {
                3
            }

            BinOp::LOr
            | BinOp::LAnd
            | BinOp::EqualEqual
            | BinOp::NotEqual
            | BinOp::GE
            | BinOp::LE
            | BinOp::Shr
            | BinOp::Shl
            | BinOp::PlusAssign
            | BinOp::MinusAssign
            | BinOp::TimesAssign
            | BinOp::DivAssign
            | BinOp::ModAssign
            | BinOp::BAndAssign
            | BinOp::BOrAssign
            | BinOp::XorAssign
            | BinOp::CloseCloseRange => 2,

            BinOp::GT
            | BinOp::LT
            | BinOp::BOr
            | BinOp::Xor
            | BinOp::BAnd
            | BinOp::Plus
            | BinOp::Minus
            | BinOp::Times
            | BinOp::Div
            | BinOp::Mod
            | BinOp::Assign => 1,
        };

        let op_span = Span {
            start: *op_span_cursor,
            end: SpanCursor {
                line: op_span_cursor.line,
                col: op_span_cursor.col + op_len,
            },
        };

        match op {
            BinOp::LOr | BinOp::LAnd => {
                self.unify_with_check(&Ty::boolean(), &left_ty, *left, &op_span);
                let right_ty = self.infer(*right);
                self.unify_with_check(&Ty::boolean(), &right_ty, *right, &op_span);
                Ty::boolean()
            }
            BinOp::GE | BinOp::GT | BinOp::LE | BinOp::LT => {
                if self.unify_with_num(&left_ty, *left, &op_span) {
                    let right_ty = self.infer(*right);
                    self.unify_with_check(&left_ty, &right_ty, *right, &op_span);
                } else {
                    let right_ty = self.infer(*right);
                    self.unify_with_num(&right_ty, *right, &op_span);
                }
                Ty::boolean()
            }
            BinOp::EqualEqual | BinOp::NotEqual => {
                let right_ty = self.infer(*right);
                self.unify_with_check(&left_ty, &right_ty, *right, &op_span);
                Ty::boolean()
            }
            BinOp::Assign => {
                let right_ty = self.infer(*right);
                self.unify_with_check(&left_ty, &right_ty, *right, &op_span);
                Ty::unit()
            }
            BinOp::OpenOpenRange
            | BinOp::CloseOpenRange
            | BinOp::OpenCloseRange
            | BinOp::CloseCloseRange => todo!(), // TODO
            BinOp::BOr | BinOp::Xor | BinOp::BAnd | BinOp::Shr | BinOp::Shl => {
                if self.unify_with_int_num(&left_ty, *left, &op_span) {
                    let right_ty = self.infer(*right);
                    self.unify_with_check(&left_ty, &right_ty, *right, &op_span);
                } else {
                    let right_ty = self.infer(*right);
                    self.unify_with_int_num(&right_ty, *right, &op_span);
                }
                left_ty
            }
            BinOp::Plus | BinOp::Minus | BinOp::Times | BinOp::Div | BinOp::Mod => {
                if self.unify_with_num(&left_ty, *left, &op_span) {
                    let right_ty = self.infer(*right);
                    self.unify_with_check(&left_ty, &right_ty, *right, &op_span);
                } else {
                    let right_ty = self.infer(*right);
                    self.unify_with_num(&right_ty, *right, &op_span);
                }
                left_ty
            }
            BinOp::BOrAssign
            | BinOp::XorAssign
            | BinOp::BAndAssign
            | BinOp::ShrAssign
            | BinOp::ShlAssign => {
                if self.unify_with_int_num(&left_ty, *left, &op_span) {
                    let right_ty = self.infer(*right);
                    self.unify_with_check(&left_ty, &right_ty, *right, &op_span);
                } else {
                    let right_ty = self.infer(*right);
                    self.unify_with_int_num(&right_ty, *right, &op_span);
                }
                Ty::unit()
            }
            BinOp::PlusAssign
            | BinOp::MinusAssign
            | BinOp::TimesAssign
            | BinOp::DivAssign
            | BinOp::ModAssign => {
                if self.unify_with_num(&left_ty, *left, &op_span) {
                    let right_ty = self.infer(*right);
                    self.unify_with_check(&left_ty, &right_ty, *right, &op_span);
                } else {
                    let right_ty = self.infer(*right);
                    self.unify_with_num(&right_ty, *right, &op_span);
                }
                Ty::unit()
            }
        }
    }

    fn unify_with_int_num(&mut self, found_ty: &Ty, expr_key: ExprKey, op_span: &Span) -> bool {
        if let Err(err) = self
            .s
            .constrain_type_var(&found_ty, TyInfer::unspecified_int_constraints())
        {
            self.add_type_mismatch_in_op_err(&Ty::i4(), &found_ty, expr_key, *op_span);
            false
        } else {
            true
        }
    }

    fn unify_with_num(&mut self, found_ty: &Ty, expr_key: ExprKey, op_span: &Span) -> bool {
        if let Err(err) = self
            .s
            .constrain_type_var(&found_ty, TyInfer::unspecified_number_constraints())
        {
            self.add_type_mismatch_in_op_err(&Ty::i4(), &found_ty, expr_key, *op_span);
            false
        } else {
            true
        }
    }

    fn unify_with_check(
        &mut self,
        expected_ty: &Ty,
        found_ty: &Ty,
        expr_key: ExprKey,
        op_span: &Span,
    ) -> bool {
        if let Err(err) = self.s.unify(expected_ty, found_ty) {
            self.add_type_mismatch_in_op_err(expected_ty, &found_ty, expr_key, *op_span);
            false
        } else {
            true
        }
    }
}
