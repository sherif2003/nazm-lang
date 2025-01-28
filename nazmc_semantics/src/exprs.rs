use crate::{type_infer::Substitution, *};

impl<'a> SemanticsAnalyzer<'a> {
    pub(crate) fn infer(&mut self, expr_key: ExprKey) -> (Substitution, Ty) {
        let expr_span = self.ast.exprs[expr_key].span;

        let (substitution, ty) = match &self.ast.exprs[expr_key].kind {
            ExprKind::Unit => (Substitution::new(), Ty::unit()),
            ExprKind::Literal(lit_expr) => (Substitution::new(), self.infer_lit_expr(*lit_expr)),
            ExprKind::PathNoPkg(path_no_pkg_key) => (
                Substitution::new(),
                self.infer_path_no_pkg_expr(*path_no_pkg_key),
            ),
            ExprKind::PathInPkg(path_with_pkg_key) => (
                Substitution::new(),
                self.infer_path_with_pkg_expr(*path_with_pkg_key),
            ),
            ExprKind::UnitStruct(unit_struct_path_key) => {
                let key = self.ast.state.unit_structs_paths_exprs[*unit_struct_path_key];
                (Substitution::new(), Ty::unit_struct(key))
            }
            ExprKind::Call(call_expr) => {
                let CallExpr {
                    on,
                    args,
                    parens_span,
                } = call_expr.as_ref();

                let on = *on;
                let parens_span = *parens_span;
                let args_len = args.len();

                let (s, mut on_expr_ty) = self.infer(on);
                on_expr_ty = s.apply(&on_expr_ty);

                match on_expr_ty.inner() {
                    Type::Lambda(LambdaType {
                        params_types,
                        return_type,
                    })
                    | Type::FnPtr(FnPtrType {
                        params_types,
                        return_type,
                    }) => {
                        if params_types.len() == args_len {
                            for i in 0..args_len {}
                        } else {
                        }
                    }
                    Type::TypeVar(ty_var_key) => {}
                    _ => {
                        let non_callable_span = self.ast.exprs[on].span;
                        self.add_calling_non_callable_err(
                            &on_expr_ty,
                            non_callable_span,
                            parens_span,
                        );
                    }
                }

                todo!()
            }
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

        (substitution, ty)
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
                NumKind::UnspecifiedInt(_) => return self.new_unspecified_unsigned_int_ty_var(),
                NumKind::UnspecifiedFloat(_) => return self.new_unspecified_float_ty_var(),
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

    fn add_calling_non_callable_err(
        &mut self,
        non_callable_ty: &Ty,
        non_callable_span: Span,
        parens_span: Span,
    ) {
        let msg = format!("");
        let label1 = format!("يجب أن يكون دالة أو تعبير لامدا");
        let label2 = format!("ولكنه من النوع `{}`", self.fmt_ty(non_callable_ty));
        let mut code_window = CodeWindow::new(
            &self.files_infos[self.current_file_key],
            non_callable_span.start,
        );
        code_window.mark_secondary(non_callable_span, vec![label1, label2]);
        code_window.mark_error(parens_span, vec![]);
        let diagnostic = Diagnostic::error(msg, vec![code_window]);
        self.diagnostics.push(diagnostic);
    }
}
