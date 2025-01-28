use crate::*;

impl<'a> SemanticsAnalyzer<'a> {
    pub(crate) fn infer(&mut self, expr_key: ExprKey) -> Ty {
        let ty = match &self.ast.exprs[expr_key].kind {
            ExprKind::Unit => Ty::unit(),
            ExprKind::Literal(lit_expr) => self.infer_lit_expr(*lit_expr),
            ExprKind::PathNoPkg(path_no_pkg_key) => self.infer_path_no_pkg_expr(*path_no_pkg_key),
            ExprKind::PathInPkg(path_with_pkg_key) => {
                self.infer_path_with_pkg_expr(*path_with_pkg_key)
            }
            ExprKind::UnitStruct(unit_struct_path_key) => {
                let key = self.ast.state.unit_structs_paths_exprs[*unit_struct_path_key];
                Ty::unit_struct(key)
            }
            ExprKind::Call(call_expr) => self.infer_call_expr(&call_expr.clone()), // TODO: Remove the clone
            ExprKind::TupleStruct(tuple_struct_expr) => todo!(),
            ExprKind::FieldsStruct(fields_struct_expr) => todo!(),
            ExprKind::Field(field_expr) => todo!(),
            ExprKind::Idx(idx_expr) => todo!(),
            ExprKind::TupleIdx(tuple_idx_expr) => todo!(),
            ExprKind::Tuple(thin_vec) => todo!(),
            ExprKind::ArrayElemnts(thin_vec) => todo!(),
            ExprKind::ArrayElemntsSized(array_elements_sized_expr) => todo!(),
            ExprKind::If(if_expr) => todo!(),
            ExprKind::Lambda(lambda_expr) => todo!(),
            ExprKind::UnaryOp(unary_op_expr) => todo!(),
            ExprKind::BinaryOp(binary_op_expr) => todo!(),
            ExprKind::Return(expr_key) => todo!(),
            ExprKind::Break(expr_key) => todo!(),
            ExprKind::Continue => todo!(),
            ExprKind::On => todo!(),
        };

        self.typed_ast.exprs.insert(expr_key, ty.clone());

        ty
    }

    fn infer_lit_expr(&mut self, lit_expr: LiteralExpr) -> Ty {
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
                NumKind::UnspecifiedInt(_) => return self.s.new_unspecified_unsigned_int_ty_var(),
                NumKind::UnspecifiedFloat(_) => return self.s.new_unspecified_float_ty_var(),
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
    ) -> Ty {
        let on = *on;
        let parens_span = *parens_span;

        let on_expr_ty = self.infer(on);

        let (params_types, return_type, is_fn) = if let Type::FnPtr(FnPtrType {
            params_types,
            return_type,
        }) = on_expr_ty.inner()
        {
            (params_types, return_type, true)
        } else if let Type::Lambda(LambdaType {
            params_types,
            return_type,
        }) = on_expr_ty.inner()
        {
            (params_types, return_type, false)
        } else {
            let non_callable_span = self.ast.exprs[on].span;
            self.add_calling_non_callable_err(&on_expr_ty, non_callable_span, parens_span);
            return self.s.new_unknown_ty_var();
        };

        if params_types.len() == args.len() {
            for i in 0..args.len() {
                let arg_ty = self.infer(args[i]);
                if let Err(err) = self.s.unify(&params_types[i], &arg_ty) {
                    self.add_incorrect_fn_args_err(
                        &params_types[i],
                        &arg_ty,
                        self.ast.exprs[args[i]].span,
                        i,
                        on,
                        is_fn,
                    );
                }
            }
        } else {
            // Infere more types to collect more errors
            for i in 0..args.len() {
                let arg_ty = self.infer(args[i]);
                if i < params_types.len() {
                    if let Err(err) = self.s.unify(&params_types[i], &arg_ty) {
                        self.add_incorrect_fn_args_err(
                            &params_types[i],
                            &arg_ty,
                            self.ast.exprs[args[i]].span,
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
}
