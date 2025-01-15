use std::path;

use typed_ast::{FieldsStruct, TupleStruct, Type, TypeKey};

use crate::*;

impl<'a> SemanticsAnalyzer<'a> {
    pub(crate) fn analyze_type_expr(&mut self, type_expr_key: TypeExprKey) -> (TypeKey, i32, u8) {
        let type_expr = &self.ast.types_exprs.all[type_expr_key];
        let (typ, size, align) = match type_expr {
            TypeExpr::Path(path_type_expr_key) => self.analyze_path_type_expr(*path_type_expr_key),
            TypeExpr::Paren(paren_type_expr_key) => todo!(),
            TypeExpr::Slice(slice_type_expr_key) => todo!(),
            TypeExpr::Ptr(ptr_type_expr_key) => todo!(),
            TypeExpr::Ref(ref_type_expr_key) => todo!(),
            TypeExpr::PtrMut(ptr_mut_type_expr_key) => todo!(),
            TypeExpr::RefMut(ref_mut_type_expr_key) => todo!(),
            TypeExpr::Tuple(tuple_type_expr_key) => todo!(),
            TypeExpr::Array(array_type_expr_key) => todo!(),
            TypeExpr::Lambda(lambda_type_expr_key) => todo!(),
        };
        (self.types_pool.get_key(&typ), size, align)
    }

    fn analyze_path_type_expr(&mut self, key: PathTypeExprKey) -> (Type, i32, u8) {
        let path_type = &self.ast.state.types_paths[key];
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

    fn analyze_unit_struct(&mut self, key: UnitStructKey) -> (Type, i32, u8) {
        let info = &self.ast.unit_structs[key].info;
        let file_path = &self.files_infos[info.file_key].path;
        if file_path != "أساسي.نظم" {
            return (Type::UnitStruct(key), 0, 0);
        }

        match info.id_key {
            IdKey::I_TYPE => (Type::I, isize::BITS as i32 / 8, isize::BITS as u8 / 8),
            IdKey::I1_TYPE => (Type::I1, 1, 1),
            IdKey::I2_TYPE => (Type::I2, 2, 2),
            IdKey::I4_TYPE => (Type::I4, 4, 4),
            IdKey::I8_TYPE => (Type::I8, 8, 8),
            IdKey::U_TYPE => (Type::U, usize::BITS as i32 / 8, usize::BITS as u8 / 8),
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

    fn analyze_tuple_struct(&mut self, key: TupleStructKey) {
        if self.semantics_stack.tuple_structs.contains_key(&key) {
            // TODO: Cycle detected
            return;
        } else if self.typed_ast.tuple_structs.contains_key(&key) {
            // It is already computed
            return;
        }

        self.semantics_stack.tuple_structs.insert(key, ());

        let types_len = self.ast.tuple_structs[key].types.len();
        let mut types = ThinVec::with_capacity(types_len);
        let mut max_align = 0;
        let mut offset: u32 = 0;

        // FIXME: This is not tested

        for i in 0..types_len {
            let type_expr_key = self.ast.tuple_structs[key].types[i].1;
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

    fn analyze_fields_struct(&mut self, key: FieldsStructKey) {
        if self.semantics_stack.fields_structs.contains_key(&key) {
            // TODO: Cycle detected
            return;
        } else if self.typed_ast.fields_structs.contains_key(&key) {
            // It is already computed
            return;
        }

        self.semantics_stack.fields_structs.insert(key, ());

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

            let (typ, size, align) = self.analyze_type_expr(typ);

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
}
