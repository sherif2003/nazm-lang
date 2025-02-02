use std::vec;

use nazmc_diagnostics::span::{self, SpanCursor};

use crate::{type_infer::TyVarState, *};

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
            Type::TypeVar(ty_var_key) => {
                let new_ty = &self.s.apply(ty);
                if new_ty == ty {
                    // format!("_{}", usize::from(ty_var_key))
                    self.fmt_ty_var_state_ty(self.s.all_ty_vars[ty_var_key].0)
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

    pub(crate) fn fmt_ty_var_state_ty(&self, ty_var_state: TyVarState) -> String {
        match ty_var_state {
            TyVarState::Unknown => format!("_"),
            TyVarState::Never => format!("!!"),
            TyVarState::UnspecifiedNumber => format!("{{عدد}}"),
            TyVarState::UnspecifiedSignedNumber => format!("{{عدد}}"),
            TyVarState::UnspecifiedUnsignedInt => format!("{{عدد}}"),
            TyVarState::UnspecifiedSignedInt => format!("{{عدد صحيح}}"),
            TyVarState::UnspecifiedFloat => format!("{{عدد عشري}}"),
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
            TypeExpr::Ptr(ptr_type_expr_key) => {
                let expr = &self.ast.types_exprs.ptrs[*ptr_type_expr_key];
                expr.span
                    .merged_with(&self.get_type_expr_span(expr.underlying_typ))
            }
            TypeExpr::Ref(ref_type_expr_key) => {
                let expr = &self.ast.types_exprs.refs[*ref_type_expr_key];
                expr.span
                    .merged_with(&self.get_type_expr_span(expr.underlying_typ))
            }
            TypeExpr::PtrMut(ptr_mut_type_expr_key) => {
                let expr = &self.ast.types_exprs.ptrs_mut[*ptr_mut_type_expr_key];
                expr.span
                    .merged_with(&self.get_type_expr_span(expr.underlying_typ))
            }
            TypeExpr::RefMut(ref_mut_type_expr_key) => {
                let expr = &self.ast.types_exprs.refs_mut[*ref_mut_type_expr_key];
                expr.span
                    .merged_with(&self.get_type_expr_span(expr.underlying_typ))
            }
            TypeExpr::Tuple(tuple_type_expr_key) => {
                self.ast.types_exprs.tuples[*tuple_type_expr_key].span
            }
            TypeExpr::Array(array_type_expr_key) => {
                self.ast.types_exprs.arrays[*array_type_expr_key].span
            }
            TypeExpr::Lambda(lambda_type_expr_key) => {
                let expr = &self.ast.types_exprs.lambdas[*lambda_type_expr_key];
                expr.params_span
                    .merged_with(&self.get_type_expr_span(expr.return_type))
            }
        }
    }

    #[inline]
    pub(crate) fn get_expr_span(&self, expr_key: ExprKey) -> Span {
        self.ast.exprs[expr_key].span
    }

    pub(crate) fn add_while_stm_should_return_unit_err(
        &mut self,
        found_ty: &Ty,
        while_keyword_span: Span,
        while_scope_key: ScopeKey,
    ) {
        let return_expr_span =
            self.get_expr_span(self.ast.scopes[while_scope_key].return_expr.unwrap());

        let mut code_window = CodeWindow::new(
            &self.files_infos[self.current_file_key],
            return_expr_span.start,
        );

        code_window.mark_secondary(while_keyword_span, vec![]);
        code_window.mark_error(
            return_expr_span,
            vec![format!(
                "يُتوقّع النوع `()` ولكن تم العثور على النوع `{}`",
                self.fmt_ty(found_ty)
            )],
        );
        let diagnostic = Diagnostic::error(
            "جملة `طالما` يجب ألّا ترجع قيمة أو ترجع قيمة من نوع `()`".into(),
            vec![code_window],
        );
        self.diagnostics.push(diagnostic);
    }

    pub(crate) fn add_type_mismatch_in_let_stm_err(
        &mut self,
        expected_ty: &Ty,
        found_ty: &Ty,
        expected_type_expr_key: TypeExprKey,
        expr_key: ExprKey,
    ) {
        let expected_span = self.get_type_expr_span(expected_type_expr_key);
        let found_span = self.get_expr_span(expr_key);
        let expected_ty = self.fmt_ty(expected_ty);

        let mut code_window =
            CodeWindow::new(&self.files_infos[self.current_file_key], found_span.start);

        code_window.mark_secondary(
            expected_span,
            vec![format!("تم تعريف النوع `{}` هنا", expected_ty)],
        );
        code_window.mark_error(
            found_span,
            vec![format!(
                "يُتوقّع النوع `{}` ولكن تم العثور على النوع `{}`",
                expected_ty,
                self.fmt_ty(found_ty)
            )],
        );
        let diagnostic = Diagnostic::error("أنواع غير متطابقة".into(), vec![code_window]);
        self.diagnostics.push(diagnostic);
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
        let call_on_span = self.get_expr_span(call_on_expr_key);

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
        let call_on_span = self.get_expr_span(call_on_expr_key);

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
        let msg = format!("لا يمكن تنفيذ عملية الاستدعاء");
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

    pub(crate) fn add_indexing_non_indexable_err(
        &mut self,
        non_indexable_ty: &Ty,
        on_expr_key: ExprKey,
        brackets_span: Span,
    ) {
        let non_indexable_ty = self.fmt_ty(non_indexable_ty);
        let msg = format!("لا يمكن فهرسة قيمة من النوع `{}`", non_indexable_ty);
        let mut code_window = CodeWindow::new(
            &self.files_infos[self.current_file_key],
            brackets_span.start,
        );
        code_window.mark_secondary(
            self.get_expr_span(on_expr_key),
            vec![format!("التعبير من النوع `{}`", non_indexable_ty)],
        );
        code_window.mark_error(brackets_span, vec![]);
        let diagnostic = Diagnostic::error(msg, vec![code_window]);
        self.diagnostics.push(diagnostic);
    }

    pub(crate) fn add_array_element_type_mismatch_err(
        &mut self,
        expected_ty: &Ty,
        found_ty: &Ty,
        first_element_span: Span,
        element_span: Span,
    ) {
        let mut code_window =
            CodeWindow::new(&self.files_infos[self.current_file_key], element_span.start);
        let expected_ty = self.fmt_ty(expected_ty);

        code_window.mark_error(
            element_span,
            vec![format!(
                "يُتوقّع النوع `{}` ولكن تم العثور على النوع `{}`",
                expected_ty,
                self.fmt_ty(found_ty)
            )],
        );
        code_window.mark_secondary(
            first_element_span,
            vec![format!(
                "أول عنصر في المصفوفة هو من النوع `{}`",
                expected_ty
            )],
        );

        let diagnostic = Diagnostic::error("أنواع غير متطابقة".into(), vec![code_window]);
        self.diagnostics.push(diagnostic);
    }

    pub(crate) fn add_out_of_bounds_tuple_idx_err(
        &mut self,
        idx: usize,
        tuple_len: usize,
        idx_span: Span,
    ) {
        let msg = format!(
            "الرقم المرجعي {} خارج حدود الترتيب الذي يحتوي على {} عنصرًا",
            idx, tuple_len
        );
        let mut code_window =
            CodeWindow::new(&self.files_infos[self.current_file_key], idx_span.start);
        code_window.mark_error(idx_span, vec![msg]);
        let diagnostic = Diagnostic::error("خطأ في فهرسة الترتيب".into(), vec![code_window]);
        self.diagnostics.push(diagnostic);
    }

    pub(crate) fn add_indexing_non_tuple_err(
        &mut self,
        found_ty: &Ty,
        on_expr_key: ExprKey,
        idx_span: Span,
    ) {
        let found_ty = self.fmt_ty(found_ty);
        let msg = format!("لا يمكن فهرسة النوع `{}` كترتيب", found_ty);
        let mut code_window =
            CodeWindow::new(&self.files_infos[self.current_file_key], idx_span.start);
        code_window.mark_secondary(
            self.get_expr_span(on_expr_key),
            vec![format!("التعبير من النوع `{}`", found_ty)],
        );
        code_window.mark_error(idx_span, vec![]);
        let diagnostic = Diagnostic::error(msg, vec![code_window]);
        self.diagnostics.push(diagnostic);
    }

    pub(crate) fn add_field_is_used_more_than_once_err(
        &mut self,
        struct_key: FieldsStructKey,
        field_id: IdKey,
        first_use_span: Span,
        second_use_span: Span,
    ) {
        let struct_info = self.ast.fields_structs[struct_key].info;

        let msg = format!(
            "الحقل `{}` تم استخدامه أكثر من مرة في تعبير الهيكل",
            self.id_pool[field_id]
        );

        let mut code_window = CodeWindow::new(
            &self.files_infos[self.current_file_key],
            second_use_span.start,
        );

        code_window.mark_error(second_use_span, vec!["إعادة تعريف لهذا الحقل".into()]);
        code_window.mark_secondary(first_use_span, vec!["تم استخدامه هنا لأول مرة".into()]);
        let mut diagnostic = Diagnostic::error(msg, vec![code_window]);

        let note_msg = format!("تم تعريف الهيكل هنا");
        let mut note_code_window = CodeWindow::new(
            &self.files_infos[struct_info.file_key],
            struct_info.id_span.start,
        );
        note_code_window.mark_note(struct_info.id_span, vec![]);
        let note = Diagnostic::note(note_msg, vec![note_code_window]);

        diagnostic.chain(note);
        self.diagnostics.push(diagnostic);
    }

    pub(crate) fn add_missing_fields_in_struct_expr_err(
        &mut self,
        struct_key: FieldsStructKey,
        missing_fields: ThinVec<IdKey>,
        expr_span: Span,
    ) {
        let struct_info = self.ast.fields_structs[struct_key].info;
        let struct_name = self.fmt_item_name(struct_info);
        let missing_fields_list = missing_fields
            .iter()
            .map(|id| format!("`{}`", self.id_pool[*id]))
            .collect::<Vec<_>>()
            .join("، ");

        let msg = format!("بعض الحقول مفقودة في تعريف الهيكل `{}`", struct_name);

        let mut code_window =
            CodeWindow::new(&self.files_infos[self.current_file_key], expr_span.start);

        code_window.mark_error(expr_span, vec!["يجب تضمين جميع الحقول المطلوبة".into()]);

        let mut diagnostic = Diagnostic::error(msg, vec![code_window]);

        let note_msg = format!("تم تعريف الهيكل هنا");
        let mut note_code_window = CodeWindow::new(
            &self.files_infos[struct_info.file_key],
            struct_info.id_span.start,
        );
        note_code_window.mark_note(struct_info.id_span, vec![]);
        let note = Diagnostic::note(note_msg, vec![note_code_window]);

        diagnostic.chain(note);
        self.diagnostics.push(diagnostic);
    }

    pub(crate) fn add_unknown_field_in_struct_expr_err(
        &mut self,
        struct_key: FieldsStructKey,
        field_id_key: IdKey,
        field_id_expr_span: Span,
    ) {
        let struct_info = self.ast.fields_structs[struct_key].info;
        let struct_name = self.fmt_item_name(struct_info);
        let field_name = self.id_pool[field_id_key].clone();

        let msg = format!(
            "الحقل `{}` غير معروف في الهيكل `{}`",
            field_name, struct_name
        );

        let mut code_window = CodeWindow::new(
            &self.files_infos[self.current_file_key],
            field_id_expr_span.start,
        );

        code_window.mark_error(
            field_id_expr_span,
            vec!["هذا الحقل غير موجود في بنية الهيكل".into()],
        );

        let mut diagnostic = Diagnostic::error(msg, vec![code_window]);

        let note_msg = format!("تم تعريف الهيكل هنا");
        let mut note_code_window = CodeWindow::new(
            &self.files_infos[struct_info.file_key],
            struct_info.id_span.start,
        );
        note_code_window.mark_note(struct_info.id_span, vec![]);
        let note = Diagnostic::note(note_msg, vec![note_code_window]);

        diagnostic.chain(note);
        self.diagnostics.push(diagnostic);
    }

    pub(crate) fn add_field_type_mismatch_err(
        &mut self,
        expected_ty: &Ty,
        found_ty: &Ty,
        struct_key: FieldsStructKey,
        field_idx: u32,
        field_id_expr_span: Span,
        expr_span: Span,
    ) {
        let field_info = &self.ast.fields_structs[struct_key].fields[field_idx as usize];
        let field_ast_id = field_info.id;
        let field_ty_span = self.get_type_expr_span(field_info.typ);
        let field_name = &self.id_pool[field_ast_id.id];

        let struct_info = self.ast.fields_structs[struct_key].info;
        let struct_name = self.fmt_item_name(struct_info);

        let msg = format!(
            "القيمة المقدمة للحقل `{}` للهيكل `{}` غير متوافقة مع النوع المتوقع",
            field_name, struct_name
        );

        let mut code_window = CodeWindow::new(
            &self.files_infos[self.current_file_key],
            field_id_expr_span.start,
        );

        code_window.mark_error(
            expr_span,
            vec![format!(
                "يُتوقّع `{}` ولكن تم العثور على `{}`",
                self.fmt_ty(expected_ty),
                self.fmt_ty(found_ty)
            )],
        );

        let mut diagnostic = Diagnostic::error(msg, vec![code_window]);

        let note_msg = format!("تم تعريف الحقل هنا");
        let mut note_code_window = CodeWindow::new(
            &self.files_infos[struct_info.file_key],
            field_ast_id.span.start,
        );
        note_code_window.mark_secondary(struct_info.id_span, vec![]);
        note_code_window.mark_note(field_ast_id.span.merged_with(&field_ty_span), vec![]);
        let note = Diagnostic::note(note_msg, vec![note_code_window]);

        diagnostic.chain(note);
        self.diagnostics.push(diagnostic);
    }

    pub(crate) fn add_filed_is_inaccessable_err(
        &mut self,
        struct_key: FieldsStructKey,
        field_idx: u32,
        field_id_expr_span: Span,
    ) {
        let struct_info = self.ast.fields_structs[struct_key].info;
        let field_info = &self.ast.fields_structs[struct_key].fields[field_idx as usize];
        let field_ast_id = field_info.id;
        let field_name = &self.id_pool[field_ast_id.id];

        let msg = format!("لا يمكن الوصول للحقل `{}`", field_name);

        let mut code_window = CodeWindow::new(
            &self.files_infos[self.current_file_key],
            field_id_expr_span.start,
        );

        code_window.mark_error(field_id_expr_span, vec![]);

        let mut diagnostic = Diagnostic::error(msg, vec![code_window]);

        let note_msg = format!("تم تعريف الحقل هنا");
        let mut note_code_window = CodeWindow::new(
            &self.files_infos[struct_info.file_key],
            field_ast_id.span.start,
        );
        note_code_window.mark_secondary(struct_info.id_span, vec![]);
        note_code_window.mark_note(field_ast_id.span, vec![]);
        let note = Diagnostic::note(note_msg, vec![note_code_window]);

        diagnostic.chain(note);
        self.diagnostics.push(diagnostic);
    }

    pub(crate) fn add_branch_stm_condition_type_mismatch_err(
        &mut self,
        found_ty: &Ty,
        keyword: &'static str,
        keyword_span: Span,
        cond_expr_key: ExprKey,
    ) {
        let cond_expr_span = self.get_expr_span(cond_expr_key);
        let mut code_window = CodeWindow::new(
            &self.files_infos[self.current_file_key],
            cond_expr_span.start,
        );

        // TODO: Conditions may be of pointer types
        code_window.mark_secondary(keyword_span, vec![]);
        code_window.mark_error(
            cond_expr_span,
            vec![format!(
                "يُتوقّع النوع `شرط` ولكن تم العثور على النوع `{}`",
                self.fmt_ty(found_ty)
            )],
        );
        let diagnostic = Diagnostic::error(
            format!("نوع شرط `{}` يجب أن يكون من النوع `شرط`", keyword),
            vec![code_window],
        );
        self.diagnostics.push(diagnostic);
    }

    pub(crate) fn add_type_mismatch_in_if_branches_err(
        &mut self,
        if_ty: &Ty,
        second_branch_ty: &Ty,
        if_scope_key: ScopeKey,
        second_branch_scope_key: ScopeKey,
        if_keyword_span: Span,
        branch_keyword_span: Span,
    ) {
        let if_ty_name = self.fmt_ty(&if_ty);
        let branch_ty_name = self.fmt_ty(&second_branch_ty);

        let if_return_expr_key = self.ast.scopes[if_scope_key].return_expr;
        let branch_return_expr_key = self.ast.scopes[second_branch_scope_key].return_expr;
        let branch_span = branch_return_expr_key.map_or_else(
            || branch_keyword_span,
            |expr_key| self.get_expr_span(expr_key),
        );

        let mut code_window =
            CodeWindow::new(&self.files_infos[self.current_file_key], branch_span.start);

        if let Some(expr_key) = if_return_expr_key {
            code_window.mark_secondary(if_keyword_span, vec![]);
            code_window.mark_secondary(
                self.get_expr_span(expr_key),
                vec![format!("التفرع ينتهي بقيمة من النوع `{}`", if_ty_name)],
            );
        } else {
            code_window.mark_secondary(
                if_keyword_span,
                vec![format!("التفرع ينتهي بقيمة من النوع `{}`", if_ty_name)],
            );
        }

        if branch_return_expr_key.is_some() {
            code_window.mark_secondary(branch_keyword_span, vec![]);
        }

        code_window.mark_error(
            branch_span,
            vec![
                format!("التفرع ينتهي بقيمة من النوع `{}`", branch_ty_name),
                format!(
                    "التفرع يجب أن ينتهي بقيمة من النوع `{}` مثل التفرع الأول",
                    if_ty_name
                ),
            ],
        );

        let diagnostic = Diagnostic::error(
            format!("تعبيرات `لو` يجب أن تنتهي بقيم من نفس النوع"),
            vec![code_window],
        );

        self.diagnostics.push(diagnostic);
    }

    pub(crate) fn add_missing_else_branch_err(
        &mut self,
        if_ty: &Ty,
        if_keyword_span: Span,
        if_scope_key: ScopeKey,
    ) {
        let if_full_span = if_keyword_span.merged_with(&self.ast.scopes[if_scope_key].span);

        let if_return_expr_span =
            self.get_expr_span(self.ast.scopes[if_scope_key].return_expr.unwrap());

        let if_ty_name = self.fmt_ty(&if_ty);
        let mut code_window =
            CodeWindow::new(&self.files_infos[self.current_file_key], if_full_span.start);

        code_window.mark_error(if_full_span, vec![]);
        code_window.mark_secondary(
            if_return_expr_span,
            vec![format!("التفرع ينتهي بقيمة من النوع `{}`", if_ty_name)],
        );

        let mut diagnostic = Diagnostic::error(
            "يجب أن يكون هناك تفرع `وإلا` لتعبير `لو`".into(),
            vec![code_window],
        );
        let note = Diagnostic::note(
            "تعبير `وإلا` يمكن إهماله إذا كانت `لو` تنتهي بقيمة من النوع `()`".into(),
            vec![],
        );

        diagnostic.chain(note);
        self.diagnostics.push(diagnostic);
    }

    pub(crate) fn add_type_doesnt_have_fields_err(
        &mut self,
        found_ty: &Ty,
        on_expr_key: ExprKey,
        field_name_expr: ASTId,
    ) {
        let found_ty = self.fmt_ty(found_ty);
        let msg = format!("النوع `{}` لا يحتوي على حقول", found_ty);
        let mut code_window = CodeWindow::new(
            &self.files_infos[self.current_file_key],
            field_name_expr.span.start,
        );
        code_window.mark_secondary(
            self.get_expr_span(on_expr_key),
            vec![format!("التعبير من النوع `{}`", found_ty)],
        );
        code_window.mark_error(field_name_expr.span, vec![]);
        let diagnostic = Diagnostic::error(msg, vec![code_window]);
        self.diagnostics.push(diagnostic);
    }

    pub(crate) fn add_type_mismatch_in_fn_return_ty_err(
        &mut self,
        fn_key: FnKey,
        expected_fn_return_ty: &Ty,
        found_return_ty: &Ty,
        err_span: Span,
    ) {
        let _fn = &self.ast.fns[fn_key];

        let mut code_window =
            CodeWindow::new(&self.files_infos[self.current_file_key], err_span.start);

        code_window.mark_error(
            err_span,
            vec![
                format!(
                    "يُتوقّع أن تُرجِع الدالة قيمة من النوع `{}`",
                    self.fmt_ty(&expected_fn_return_ty),
                ),
                format!(
                    "ولكن تم العثور على النوع `{}`",
                    self.fmt_ty(&found_return_ty)
                ),
            ],
        );

        code_window.mark_secondary(_fn.info.id_span, vec!["في هذه الدالة".into()]);

        if let Some(type_expr_span) = _fn
            .return_type
            .map(|type_expr_key| self.get_type_expr_span(type_expr_key))
        {
            if err_span != type_expr_span {
                code_window.mark_secondary(
                    type_expr_span,
                    vec!["هنا تم تحديد نوع القيمة التي سترجعها الدالة".into()],
                );
            }
        }

        let diagnostic = Diagnostic::error("أنواع غير متطابقة".into(), vec![code_window]);
        self.diagnostics.push(diagnostic);
    }

    pub(crate) fn add_type_mismatch_in_lambda_return_ty_err(
        &mut self,
        lambda_scope_key: ScopeKey,
        expteced_lambda_return_ty: &Ty,
        found_return_ty: &Ty,
        span: Span,
    ) {
        let first_implicit_return_ty_span =
            self.current_lambda_first_implicit_return_ty_span.unwrap();

        let mut code_window = CodeWindow::new(&self.files_infos[self.current_file_key], span.start);

        code_window.mark_error(
            span,
            vec![format!(
                "يُتوقّع النوع `{}` ولكن تم العثور على النوع `{}`",
                self.fmt_ty(&expteced_lambda_return_ty),
                self.fmt_ty(&found_return_ty)
            )],
        );

        if self.ast.scopes[lambda_scope_key].return_expr.is_some() {
            let curly_braces_span = self.ast.scopes[lambda_scope_key].span;
            code_window.mark_secondary(curly_braces_span, vec!["في تعبير اللامدا هذا".into()]);
        } else {
            code_window
                .mark_secondary(span, vec!["تعبير اللامدا هذا يرجع ضمنياً النوع `()`".into()]);
        }

        code_window.mark_secondary(
            first_implicit_return_ty_span,
            vec!["تم تحديد نوع القيمة التي سترجع هنا".into()],
        );

        let diagnostic = Diagnostic::error("أنواع غير متطابقة".into(), vec![code_window]);
        self.diagnostics.push(diagnostic);
    }

    pub(crate) fn add_type_mismatch_in_op_err(
        &mut self,
        expected_ty: &Ty,
        found_ty: &Ty,
        expr_key: ExprKey,
        op_span: Span,
    ) {
        self.add_type_mismatch_err(expected_ty, found_ty, self.get_expr_span(expr_key));

        self.diagnostics
            .last_mut()
            .unwrap()
            .last_code_window()
            .mark_secondary(op_span, vec!["هذا المُؤثِّر".into()]);
    }

    pub(crate) fn add_cannot_deref_type(&mut self, ty: &Ty, expr_key: ExprKey, op_span: Span) {
        let expr_span = self.get_expr_span(expr_key);
        let ty = self.fmt_ty(ty);

        let mut code_window =
            CodeWindow::new(&self.files_infos[self.current_file_key], expr_span.start);
        code_window.mark_secondary(
            op_span,
            vec!["المُؤثِّر `*` يمكن تطبيقه فقط على المؤشِّرات".into()],
        );
        code_window.mark_error(
            expr_span,
            vec![format!("يُتوقّع مؤشِّر، ولكن تم العثور على `{}`", ty)],
        );

        let diagnostic = Diagnostic::error(
            format!("لا يمكن تطبيق المُؤثِّر `*` على النوع `{}`", ty),
            vec![code_window],
        );
        self.diagnostics.push(diagnostic);
    }
}
