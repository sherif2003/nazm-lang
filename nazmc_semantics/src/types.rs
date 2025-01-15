use std::{net::UdpSocket, path};

use typed_ast::{FieldsStruct, TupleStruct, TupleType, Type, TypeKey};

use crate::*;

type TypeLayout = (TypeKey, i32, u8);

impl<'a> SemanticsAnalyzer<'a> {
    const PTR_SIZE: i32 = usize::BITS as i32 / 8;

    fn get_type_expr_span(&self, type_expr_key: TypeExprKey) -> Span {
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

    fn analyze_type_expr_checked(
        &mut self,
        type_expr_key: TypeExprKey,
        at: FileKey,
        called_from: CycleDetected,
    ) -> (TypeKey, i32, u8) {
        let result = self.analyze_type_expr(type_expr_key);

        if self.semantics_stack.is_cycle_detected == CycleDetected::None {
            return result;
        }

        let type_expr_span = self.get_type_expr_span(type_expr_key);

        if self.semantics_stack.is_cycle_detected == called_from {
            match called_from {
                CycleDetected::None => {}
                CycleDetected::Const(const_key) => todo!(),
                CycleDetected::TupleStruct(tuple_struct_key) => {
                    let item_info = self.ast.tuple_structs[tuple_struct_key].info;
                    let msg = format!(
                        "توجد حلقة لا متناهية في تحديد حجم الهيكل `{}`",
                        self.fmt_item_name(item_info)
                    );
                    let (err_label, sec_label) = if self.cycle_stack.is_empty() {
                        ("", "")
                    } else {
                        ("", "")
                    };

                    let mut code_window =
                        CodeWindow::new(&self.files_infos[at], type_expr_span.start);

                    code_window.mark_error(type_expr_span, vec![err_label.into()]);
                    code_window.mark_secondary(item_info.id_span, vec![sec_label.into()]);

                    let mut diagnostic = Diagnostic::error(msg, vec![code_window]);

                    for cycle in std::mem::take(&mut self.cycle_stack).into_iter().rev() {
                        diagnostic.chain(cycle);
                    }

                    self.diagnostics.push(diagnostic);
                }
                CycleDetected::FieldsStruct(fields_struct_key) => {
                    let item_info = self.ast.fields_structs[fields_struct_key].info;
                    let msg = format!(
                        "توجد حلقة لا متناهية في تحديد حجم الهيكل `{}`",
                        self.fmt_item_name(item_info)
                    );
                    let label = if self.cycle_stack.is_empty() { "" } else { "" };

                    let mut code_window =
                        CodeWindow::new(&self.files_infos[at], type_expr_span.start);
                    code_window.mark_error(type_expr_span, vec![]);
                    code_window.mark_secondary(item_info.id_span, vec!["في هذا الهيكل".into()]);

                    let mut diagnostic = Diagnostic::error(msg, vec![code_window]);

                    for cycle in std::mem::take(&mut self.cycle_stack).into_iter().rev() {
                        diagnostic.chain(cycle);
                    }

                    self.diagnostics.push(diagnostic);
                }
            }

            self.semantics_stack.is_cycle_detected = CycleDetected::None;
        } else {
            match called_from {
                CycleDetected::None => {}
                CycleDetected::Const(const_key) => todo!(),
                CycleDetected::TupleStruct(tuple_struct_key) => {
                    let item_info = self.ast.tuple_structs[tuple_struct_key].info;

                    let msg = format!("عند تحديد حجم الهيكل `{}`", self.fmt_item_name(item_info));

                    let mut code_window =
                        CodeWindow::new(&self.files_infos[at], type_expr_span.start);
                    code_window.mark_note(type_expr_span, vec![]);
                    code_window.mark_secondary(item_info.id_span, vec!["في هذا الهيكل".into()]);

                    let diagnostic = Diagnostic::note(msg, vec![code_window]);

                    self.cycle_stack.push(diagnostic);
                }
                CycleDetected::FieldsStruct(fields_struct_key) => {
                    let item_info = self.ast.fields_structs[fields_struct_key].info;
                    let msg = format!("عند تحديد حجم الهيكل `{}`", self.fmt_item_name(item_info));

                    let mut code_window =
                        CodeWindow::new(&self.files_infos[at], type_expr_span.start);
                    code_window.mark_note(type_expr_span, vec![]);
                    code_window.mark_secondary(item_info.id_span, vec!["في هذا الهيكل".into()]);

                    let diagnostic = Diagnostic::note(msg, vec![code_window]);

                    self.cycle_stack.push(diagnostic);
                }
            }
        }
        result
    }

    pub(crate) fn analyze_type_expr(&mut self, type_expr_key: TypeExprKey) -> (TypeKey, i32, u8) {
        let type_expr = &self.ast.types_exprs.all[type_expr_key];
        let (typ, size, align) = match type_expr {
            TypeExpr::Path(path_type_expr_key) => self.analyze_path_type_expr(*path_type_expr_key),
            TypeExpr::Tuple(tuple_type_expr_key) => self.analyze_tuple(*tuple_type_expr_key),
            TypeExpr::Array(array_type_expr_key) => self.analyze_array(*array_type_expr_key),
            TypeExpr::Lambda(lambda_type_expr_key) => self.analyze_lambda(*lambda_type_expr_key),
            TypeExpr::Paren(paren_type_expr_key) => {
                return self.analyze_type_expr(
                    self.ast.types_exprs.parens[*paren_type_expr_key].underlying_typ,
                );
            }
            TypeExpr::Slice(slice_type_expr_key) => {
                let underlying_typ =
                    self.ast.types_exprs.slices[*slice_type_expr_key].underlying_typ;
                let (type_key, _size, _align) = self.analyze_type_expr(underlying_typ);
                (Type::Slice(type_key), -1, 0)
            }
            TypeExpr::Ptr(ptr_type_expr_key) => {
                let underlying_typ = self.ast.types_exprs.ptrs[*ptr_type_expr_key].underlying_typ;
                let (underlying_type_key, _size, _align) = self.analyze_type_expr(underlying_typ);
                (
                    Type::Ptr(underlying_type_key),
                    Self::PTR_SIZE,
                    Self::PTR_SIZE as u8,
                )
            }
            TypeExpr::Ref(ref_type_expr_key) => {
                let underlying_typ = self.ast.types_exprs.refs[*ref_type_expr_key].underlying_typ;
                let (underlying_type_key, _size, _align) = self.analyze_type_expr(underlying_typ);
                let underlying_typ_expr = self.ast.types_exprs.all[underlying_typ];
                let (size, align) = if let TypeExpr::Slice(_) = underlying_typ_expr {
                    (2 * Self::PTR_SIZE, 2 * Self::PTR_SIZE as u8)
                } else {
                    (Self::PTR_SIZE, Self::PTR_SIZE as u8)
                };
                (Type::Ref(underlying_type_key), size, align)
            }
            TypeExpr::PtrMut(ptr_mut_type_expr_key) => {
                let underlying_typ =
                    self.ast.types_exprs.ptrs_mut[*ptr_mut_type_expr_key].underlying_typ;
                let (underlying_type_key, _size, _align) = self.analyze_type_expr(underlying_typ);
                (
                    Type::Ptr(underlying_type_key),
                    Self::PTR_SIZE,
                    Self::PTR_SIZE as u8,
                )
            }
            TypeExpr::RefMut(ref_mut_type_expr_key) => {
                let underlying_typ =
                    self.ast.types_exprs.refs_mut[*ref_mut_type_expr_key].underlying_typ;
                let (underlying_type_key, _size, _align) = self.analyze_type_expr(underlying_typ);
                let underlying_typ_expr = self.ast.types_exprs.all[underlying_typ];
                let (size, align) = if let TypeExpr::Slice(_) = underlying_typ_expr {
                    (2 * Self::PTR_SIZE, 2 * Self::PTR_SIZE as u8)
                } else {
                    (Self::PTR_SIZE, Self::PTR_SIZE as u8)
                };
                (Type::RefMut(underlying_type_key), size, align)
            }
        };
        (self.types_pool.get_key(&typ), size, align)
    }

    #[inline]
    fn analyze_path_type_expr(&mut self, key: PathTypeExprKey) -> (Type, i32, u8) {
        let (path_type, _span) = &self.ast.state.types_paths[key];
        match path_type {
            Item::UnitStruct { vis: _, key } => self.analyze_unit_struct(*key),
            Item::TupleStruct { vis: _, key } => {
                let key = *key;
                self.analyze_tuple_struct(key);
                let s = self.typed_ast.tuple_structs.get(&key).unwrap();
                (Type::TupleStruct(key), s.size as i32, s.align)
            }
            Item::FieldsStruct { vis: _, key } => {
                let key = *key;
                self.analyze_fields_struct(key);
                let s = self.typed_ast.fields_structs.get(&key).unwrap();

                (Type::FieldsStruct(key), s.size as i32, s.align)
            }
            _ => unreachable!(),
        }
    }

    #[inline]
    fn analyze_unit_struct(&mut self, key: UnitStructKey) -> (Type, i32, u8) {
        let info = &self.ast.unit_structs[key].info;
        let file_path = &self.files_infos[info.file_key].path;
        if file_path != "أساسي.نظم" {
            return (Type::UnitStruct(key), 0, 0);
        }

        match info.id_key {
            IdKey::I_TYPE => (Type::I, Self::PTR_SIZE, Self::PTR_SIZE as u8),
            IdKey::I1_TYPE => (Type::I1, 1, 1),
            IdKey::I2_TYPE => (Type::I2, 2, 2),
            IdKey::I4_TYPE => (Type::I4, 4, 4),
            IdKey::I8_TYPE => (Type::I8, 8, 8),
            IdKey::U_TYPE => (Type::U, Self::PTR_SIZE, Self::PTR_SIZE as u8),
            IdKey::U1_TYPE => (Type::U1, 1, 1),
            IdKey::U2_TYPE => (Type::U2, 2, 2),
            IdKey::U4_TYPE => (Type::U4, 4, 4),
            IdKey::U8_TYPE => (Type::U8, 8, 8),
            IdKey::F4_TYPE => (Type::F4, 4, 4),
            IdKey::F8_TYPE => (Type::F8, 8, 8),
            IdKey::BOOL_TYPE => (Type::Bool, 1, 1),
            IdKey::CHAR_TYPE => (Type::Char, 4, 4),
            IdKey::STR_TYPE => (Type::Str, -1, 0),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn analyze_tuple_struct(&mut self, key: TupleStructKey) {
        if self.typed_ast.tuple_structs.contains_key(&key) {
            // It is already computed
            return;
        } else if self.semantics_stack.tuple_structs.contains_key(&key) {
            self.semantics_stack.is_cycle_detected = CycleDetected::TupleStruct(key);

            self.typed_ast.tuple_structs.insert(key, Default::default());

            self.semantics_stack.tuple_structs.remove(&key);

            return;
        }

        self.semantics_stack.tuple_structs.insert(key, ());

        let at = self.ast.tuple_structs[key].info.file_key;
        let called_from = CycleDetected::TupleStruct(key);
        let types_len = self.ast.tuple_structs[key].types.len();
        let mut types = ThinVec::with_capacity(types_len);
        let mut max_align = 0;
        let mut offset: u32 = 0;

        // FIXME: This is not tested

        for i in 0..types_len {
            let type_expr_key = self.ast.tuple_structs[key].types[i].1;
            let (typ, size, align) = self.analyze_type_expr_checked(type_expr_key, at, called_from);

            if size < 0 {
                // TODO: Unsized type
                panic!("Unsized type")
            }

            if align > max_align {
                max_align = align;
            }

            let aligned_offset = (i * align as usize) as u32;
            if aligned_offset > offset {
                offset = aligned_offset;
            }

            types.push(typed_ast::FieldInfo { offset, typ });
            offset += size as u32;
        }

        self.semantics_stack.tuple_structs.remove(&key);

        // TODO: Reorder types

        self.typed_ast.tuple_structs.insert(
            key,
            TupleStruct {
                types,
                size: offset,
                align: max_align,
            },
        );
    }

    #[inline]
    fn analyze_fields_struct(&mut self, key: FieldsStructKey) {
        if self.typed_ast.fields_structs.contains_key(&key) {
            // It is already computed
            return;
        } else if self.semantics_stack.fields_structs.contains_key(&key) {
            self.semantics_stack.is_cycle_detected = CycleDetected::FieldsStruct(key);

            self.typed_ast
                .fields_structs
                .insert(key, Default::default());

            self.semantics_stack.fields_structs.remove(&key);

            return;
        }

        self.semantics_stack.fields_structs.insert(key, ());

        let at = self.ast.fields_structs[key].info.file_key;
        let called_from = CycleDetected::FieldsStruct(key);
        let fields_len = self.ast.fields_structs[key].fields.len();
        let mut fields = HashMap::with_capacity(fields_len);
        let mut max_align = 0;
        let mut offset: u32 = 0;

        // FIXME: This is not tested
        for i in 0..fields_len {
            let FieldInfo {
                vis: _,
                id: ASTId { span: _, id },
                typ,
            } = self.ast.fields_structs[key].fields[i];

            let (typ, size, align) = self.analyze_type_expr_checked(typ, at, called_from);

            if size < 0 {
                // TODO: Unsized fields
            }

            if align > max_align {
                max_align = align;
            }

            let aligned_offset = (i * align as usize) as u32;
            if aligned_offset > offset {
                offset = aligned_offset;
            }

            fields.insert(id, typed_ast::FieldInfo { offset, typ });
            offset += size as u32;
        }

        self.semantics_stack.fields_structs.remove(&key);

        // TODO: Reorder fields

        self.typed_ast.fields_structs.insert(
            key,
            FieldsStruct {
                fields,
                size: offset,
                align: max_align,
            },
        );
    }

    #[inline]
    fn analyze_tuple(&mut self, key: TupleTypeExprKey) -> (Type, i32, u8) {
        let types_len = self.ast.types_exprs.tuples[key].types.len();
        let mut types = ThinVec::with_capacity(types_len);
        let mut max_align = 0;
        let mut offset: u32 = 0;

        // FIXME: This is not tested

        for i in 0..types_len {
            let type_expr_key = self.ast.types_exprs.tuples[key].types[i];
            let (typ, size, align) = self.analyze_type_expr(type_expr_key);

            if size < 0 {
                // TODO: Unsized type
            }

            if align > max_align {
                max_align = align;
            }

            let aligned_offset = (i * align as usize) as u32;
            if aligned_offset > offset {
                offset = aligned_offset;
            }

            types.push(typed_ast::FieldInfo { offset, typ });
            offset += size as u32;
        }

        // TODO: Reorder types

        let key = self.tuple_types_pool.get_key(&TupleType { types });

        (Type::Tuple(key), offset as i32, max_align)
    }

    #[inline]
    fn analyze_array(&mut self, key: ArrayTypeExprKey) -> (Type, i32, u8) {
        let underlying_typ = self.ast.types_exprs.arrays[key].underlying_typ;

        let (underlying_typ, underlying_typ_size, underlying_typ_align) =
            self.analyze_type_expr(underlying_typ);

        let size_expr_scope_key = self.ast.types_exprs.arrays[key].size_expr_scope_key;
        todo!()
    }

    #[inline]
    fn analyze_lambda(&mut self, key: LambdaTypeExprKey) -> (Type, i32, u8) {
        let params_types_len = self.ast.types_exprs.lambdas[key].params_types.len();
        let mut params_types = ThinVec::with_capacity(params_types_len);

        for i in 0..params_types_len {
            let type_expr_key = self.ast.types_exprs.lambdas[key].params_types[i];
            params_types.push(self.analyze_type_expr(type_expr_key).0);
        }

        let return_type = self
            .analyze_type_expr(self.ast.types_exprs.lambdas[key].return_type)
            .0;

        let key = self.lambda_types_pool.get_key(&LambdaType {
            params_types,
            return_type,
        });

        (Type::Lambda(key), Self::PTR_SIZE, Self::PTR_SIZE as u8)
    }
}
