use crate::{typed_ast::ArrayType, *};

impl<'a> SemanticsAnalyzer<'a> {
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
            ExprKind::TupleStruct(tuple_struct_expr) => todo!(),
            ExprKind::FieldsStruct(fields_struct_expr) => todo!(),
            ExprKind::Field(field_expr) => todo!(),
            ExprKind::ArrayElemntsSized(array_elements_sized_expr) => todo!(),
            ExprKind::If(if_expr) => todo!(),
            ExprKind::Lambda(lambda_expr) => todo!(),
            ExprKind::UnaryOp(unary_op_expr) => todo!(),
            ExprKind::BinaryOp(binary_op_expr) => todo!(),
            ExprKind::Return(expr_key) => todo!(),
            ExprKind::Break | ExprKind::Continue => self
                .s
                .new_never_ty_var(self.current_file_key, self.get_expr_span(expr_key)),
            ExprKind::On => todo!(),
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
                    return self.s.new_unspecified_unsigned_int_ty_var(
                        self.current_file_key,
                        self.get_expr_span(expr_key),
                    )
                }
                NumKind::UnspecifiedFloat(_) => {
                    return self.s.new_unspecified_float_ty_var(
                        self.current_file_key,
                        self.get_expr_span(expr_key),
                    )
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

        let (params_types, return_type, is_fn) = match on_expr_ty.inner() {
            Type::FnPtr(FnPtrType {
                params_types,
                return_type,
            }) => (params_types, return_type, true),
            Type::Lambda(LambdaType {
                params_types,
                return_type,
            }) => (params_types, return_type, false),
            Type::TypeVar(_) if self.s.check_map_to_unspecified_number(&on_expr_ty) => {
                self.add_calling_non_callable_err(&on_expr_ty, self.get_expr_span(on), parens_span);
                return self
                    .s
                    .new_unknown_ty_var(self.current_file_key, self.get_expr_span(expr_key));
            }
            Type::TypeVar(_) => {
                let params_types = args
                    .iter()
                    .map(|expr_key| {
                        self.s.new_unknown_ty_var(
                            self.current_file_key,
                            self.get_expr_span(*expr_key),
                        )
                    })
                    .collect::<ThinVec<_>>();
                let return_type = self
                    .s
                    .new_unknown_ty_var(self.current_file_key, self.get_expr_span(expr_key));
                (params_types, return_type, true)
            }
            _ => {
                self.add_calling_non_callable_err(&on_expr_ty, self.get_expr_span(on), parens_span);
                return self
                    .s
                    .new_unknown_ty_var(self.current_file_key, self.get_expr_span(expr_key));
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

        let (underlying_ty, is_array) = match on_expr_ty.inner() {
            Type::Array(ArrayType {
                underlying_typ,
                size: _,
            }) => (underlying_typ, true),
            Type::Slice(underlying_ty) => (underlying_ty, false),
            Type::TypeVar(_) if self.s.check_map_to_unspecified_number(&on_expr_ty) => {
                self.add_indexing_non_indexable_err(&on_expr_ty, brackets_span);
                return self
                    .s
                    .new_unknown_ty_var(self.current_file_key, self.get_expr_span(expr_key));
            }
            Type::TypeVar(_) => (
                self.s
                    .new_unknown_ty_var(self.current_file_key, self.get_expr_span(on)),
                true,
            ),
            _ => {
                self.add_indexing_non_indexable_err(&on_expr_ty, brackets_span);
                return self
                    .s
                    .new_unknown_ty_var(self.current_file_key, self.get_expr_span(expr_key));
            }
        };

        let idx_ty = self.infer(*idx);

        // TODO: Support ranges nidecies
        if let Err(err) = self.s.unify(&Ty::u(), &idx_ty) {
            self.add_type_mismatch_err(&Ty::u(), &idx_ty, self.get_expr_span(*idx));
        }

        underlying_ty
    }

    fn infer_array_elements(&mut self, elements: &ThinVec<ExprKey>, expr_key: ExprKey) -> Ty {
        if elements.is_empty() {
            let unknown_ty = self
                .s
                .new_unknown_ty_var(self.current_file_key, self.get_expr_span(expr_key));
            return Ty::array(unknown_ty, 0);
        }

        let first_ty = self.infer(elements[0]);

        for &elem in &elements[1..] {
            let elem_ty = self.infer(elem);
            if let Err(_) = self.s.unify(&first_ty, &elem_ty) {
                self.add_type_mismatch_err(&first_ty, &elem_ty, self.get_expr_span(elem));
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

        match on_expr_ty.inner() {
            Type::Tuple(TupleType { types }) => {
                if idx < types.len() {
                    types[idx].clone()
                } else {
                    self.add_out_of_bounds_tuple_idx_err(idx, types.len(), *idx_span);
                    self.s
                        .new_unknown_ty_var(self.current_file_key, self.get_expr_span(expr_key))
                }
            }
            Type::TypeVar(_) if self.s.check_map_to_unspecified_number(&on_expr_ty) => {
                self.add_indexing_non_tuple_err(&on_expr_ty, *idx_span);
                self.s
                    .new_unknown_ty_var(self.current_file_key, self.get_expr_span(expr_key))
            }
            Type::TypeVar(_) => {
                let unknown_ty = self
                    .s
                    .new_unknown_ty_var(self.current_file_key, self.get_expr_span(expr_key));
                unknown_ty
            }
            _ => {
                self.add_indexing_non_tuple_err(&on_expr_ty, *idx_span);
                self.s
                    .new_unknown_ty_var(self.current_file_key, self.get_expr_span(expr_key))
            }
        }
    }
}
