use crate::*;

impl<'a> SemanticsAnalyzer<'a> {
    pub(crate) fn fmt_pkg_name(&self, pkg_key: PkgKey) -> String {
        self.pkgs_names[pkg_key]
            .iter()
            .map(|id| self.id_pool[*id].as_str())
            .collect::<Vec<_>>()
            .join("::")
    }

    pub(crate) fn fmt_item_name(&self, item_info: ItemInfo) -> String {
        let pkg = self.fmt_pkg_name(self.files_to_pkgs[item_info.file_key]);
        let name = &self.id_pool[item_info.id_key];
        if pkg.is_empty() {
            name.to_owned()
        } else {
            format!("{}::{}", pkg, name)
        }
    }

    pub(crate) fn fmt_ty(&self, ty: &Ty) -> String {
        match ty.inner() {
            Type::Unknown => format!("_"),
            Type::UnspecifiedUnsignedInt => format!("{{عدد}}"),
            Type::UnspecifiedSignedInt => format!("{{عدد صحيح}}"),
            Type::UnspecifiedFloat => format!("{{عدد عشري}}"),
            Type::TypeVar(ty_var_key) => {
                let new_ty = &self.s.apply(ty);
                if new_ty == ty {
                    // format!("_{}", usize::from(ty_var_key))
                    self.fmt_ty(&self.s.all_ty_vars[ty_var_key])
                } else {
                    self.fmt_ty(new_ty)
                }
            }
            Type::Slice(rc_cell) => format!("[{}]", self.fmt_ty(&rc_cell)),
            Type::Ptr(rc_cell) => format!("*{}", self.fmt_ty(&rc_cell)),
            Type::Ref(rc_cell) => format!("#{}", self.fmt_ty(&rc_cell)),
            Type::PtrMut(rc_cell) => format!("*متغير {}", self.fmt_ty(&rc_cell)),
            Type::RefMut(rc_cell) => format!("#متغير {}", self.fmt_ty(&rc_cell)),
            Type::Array(array_type) => format!(
                "[{}؛ {}]",
                self.fmt_ty(&array_type.underlying_typ),
                array_type.size
            ),
            Type::Tuple(tuple_type) => {
                format!(
                    "({})",
                    tuple_type
                        .types
                        .iter()
                        .map(|ty| self.fmt_ty(&ty))
                        .collect::<Vec<_>>()
                        .join("، ")
                )
            }
            Type::Lambda(lambda_type) => format!(
                "({}) -> {}",
                lambda_type
                    .params_types
                    .iter()
                    .map(|param_ty| self.fmt_ty(&param_ty))
                    .collect::<Vec<_>>()
                    .join("، "),
                self.fmt_ty(&lambda_type.return_type)
            ),
            Type::FnPtr(fn_ptr_type) => format!(
                "دالة({}) -> {}",
                fn_ptr_type
                    .params_types
                    .iter()
                    .map(|param_ty| self.fmt_ty(&param_ty))
                    .collect::<Vec<_>>()
                    .join("، "),
                self.fmt_ty(&fn_ptr_type.return_type)
            ),
            Type::Concrete(con_ty) => self.fmt_con_ty(&con_ty),
        }
    }

    pub(crate) fn fmt_con_ty(&self, con_ty: &ConcreteType) -> String {
        match con_ty {
            ConcreteType::Never => format!("!!"),
            ConcreteType::Unit => format!("()"),
            ConcreteType::I => format!("ص"),
            ConcreteType::I1 => format!("ص1"),
            ConcreteType::I2 => format!("ص2"),
            ConcreteType::I4 => format!("ص4"),
            ConcreteType::I8 => format!("ص8"),
            ConcreteType::U => format!("ط"),
            ConcreteType::U1 => format!("ط1"),
            ConcreteType::U2 => format!("ط2"),
            ConcreteType::U4 => format!("ط4"),
            ConcreteType::U8 => format!("ط8"),
            ConcreteType::F4 => format!("ع4"),
            ConcreteType::F8 => format!("ع8"),
            ConcreteType::Bool => format!("شرط"),
            ConcreteType::Char => format!("حرف"),
            ConcreteType::Str => format!("متن"),
            ConcreteType::UnitStruct(unit_struct_key) => {
                let item_info = self.ast.unit_structs[*unit_struct_key].info;
                self.fmt_item_name(item_info)
            }
            ConcreteType::TupleStruct(tuple_struct_key) => {
                let item_info = self.ast.tuple_structs[*tuple_struct_key].info;
                self.fmt_item_name(item_info)
            }
            ConcreteType::FieldsStruct(fields_struct_key) => {
                let item_info = self.ast.fields_structs[*fields_struct_key].info;
                self.fmt_item_name(item_info)
            }
        }
    }

    pub(crate) fn get_type_expr_span(&self, type_expr_key: TypeExprKey) -> Span {
        match &self.ast.types_exprs.all[type_expr_key] {
            TypeExpr::Path(path_type_expr_key) => self.ast.state.types_paths[*path_type_expr_key].1,
            TypeExpr::Paren(paren_type_expr_key) => {
                self.ast.types_exprs.parens[*paren_type_expr_key].span
            }
            TypeExpr::Slice(slice_type_expr_key) => {
                self.ast.types_exprs.slices[*slice_type_expr_key].span
            }
            TypeExpr::Ptr(ptr_type_expr_key) => self.ast.types_exprs.ptrs[*ptr_type_expr_key].span,
            TypeExpr::Ref(ref_type_expr_key) => self.ast.types_exprs.refs[*ref_type_expr_key].span,
            TypeExpr::PtrMut(ptr_mut_type_expr_key) => {
                self.ast.types_exprs.ptrs_mut[*ptr_mut_type_expr_key].span
            }
            TypeExpr::RefMut(ref_mut_type_expr_key) => {
                self.ast.types_exprs.refs_mut[*ref_mut_type_expr_key].span
            }
            TypeExpr::Tuple(tuple_type_expr_key) => {
                self.ast.types_exprs.tuples[*tuple_type_expr_key].span
            }
            TypeExpr::Array(array_type_expr_key) => {
                self.ast.types_exprs.arrays[*array_type_expr_key].span
            }
            TypeExpr::Lambda(lambda_type_expr_key) => {
                self.ast.types_exprs.lambdas[*lambda_type_expr_key].span
            }
        }
    }

    pub(crate) fn add_type_mismatch_err(&mut self, expected_ty: &Ty, found_ty: &Ty, span: Span) {
        let mut code_window = CodeWindow::new(&self.files_infos[self.current_file_key], span.start);
        code_window.mark_error(
            span,
            vec![format!(
                "يُتوقّع النوع `{}` ولكن تم العثور على النوع `{}`",
                self.fmt_ty(expected_ty),
                self.fmt_ty(found_ty)
            )],
        );
        let diagnostic = Diagnostic::error("أنواع غير متطابقة".into(), vec![code_window]);
        self.diagnostics.push(diagnostic);
    }

    pub(crate) fn add_incorrect_fn_args_err(
        &mut self,
        expected_ty: &Ty,
        found_ty: &Ty,
        arg_span: Span,
        arg_idx: usize,
        call_on_expr_key: ExprKey,
        // Is fn or lambda
        is_fn: bool,
    ) {
        let call_on_span = self.ast.exprs[call_on_expr_key].span;

        let mut code_window =
            CodeWindow::new(&self.files_infos[self.current_file_key], arg_span.start);

        code_window.mark_error(
            arg_span,
            vec![format!(
                "يُتوقّع النوع `{}` ولكن تم العثور على النوع `{}`",
                self.fmt_ty(expected_ty),
                self.fmt_ty(found_ty)
            )],
        );

        code_window.mark_secondary(
            call_on_span,
            vec![if is_fn {
                "مُعاملات هذه الدالة غير متطابقة"
            } else {
                "مُعاملات تعبير اللامدا غير متطابقة"
            }
            .into()],
        );

        let mut diagnostic = Diagnostic::error("أنواع غير متطابقة".into(), vec![code_window]);

        if let Some(Item::Fn { vis: _, key }) = self.check_expr_is_path_to_item(call_on_expr_key) {
            let info = self.ast.fns[key].info;
            let (param_ast_id, type_expr_key) = self.ast.fns[key].params[arg_idx];
            let param_span = param_ast_id
                .span
                .merged_with(&self.get_type_expr_span(type_expr_key));

            let mut code_window =
                CodeWindow::new(&self.files_infos[info.file_key], info.id_span.start);
            code_window.mark_note(info.id_span, vec![]);
            code_window.mark_secondary(param_span, vec![]);

            let note = Diagnostic::note("تم تعريف الدالة هنا".into(), vec![code_window]);
            diagnostic.chain(note);
        }

        self.diagnostics.push(diagnostic);
    }

    pub(crate) fn add_incorrect_fn_args_len_err(
        &mut self,
        parens_span: Span,
        call_on_expr_key: ExprKey,
        // Is fn or lambda
        is_fn: bool,
    ) {
        let call_on_span = self.ast.exprs[call_on_expr_key].span;

        let mut code_window =
            CodeWindow::new(&self.files_infos[self.current_file_key], parens_span.start);

        code_window.mark_error(parens_span, vec![]);
        code_window.mark_secondary(call_on_span, vec![]);

        let mut diagnostic = Diagnostic::error(
            if is_fn {
                "عدد مُعاملات الدالة غير متطابق"
            } else {
                "عدد مُعاملات تعبير اللامدا غير متطابق"
            }
            .into(),
            vec![code_window],
        );

        if let Some(Item::Fn { vis: _, key }) = self.check_expr_is_path_to_item(call_on_expr_key) {
            let info = self.ast.fns[key].info;

            let mut code_window =
                CodeWindow::new(&self.files_infos[info.file_key], info.id_span.start);
            code_window.mark_note(info.id_span, vec![]);

            let note = Diagnostic::note("تم تعريف الدالة هنا".into(), vec![code_window]);
            diagnostic.chain(note);
        }

        self.diagnostics.push(diagnostic);
    }

    fn check_expr_is_path_to_item(&self, expr_key: ExprKey) -> Option<Item> {
        match &self.ast.exprs[expr_key].kind {
            ExprKind::PathNoPkg(path_no_pkg_key) => {
                Some(self.ast.state.paths_no_pkgs_exprs[*path_no_pkg_key])
            }
            ExprKind::PathInPkg(path_with_pkg_key) => {
                Some(self.ast.state.paths_with_pkgs_exprs[*path_with_pkg_key])
            }
            _ => return None,
        }
    }

    pub(crate) fn add_calling_non_callable_err(
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
