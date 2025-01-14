use std::path;

use typed_ast::{Type, TypeKey, TypeKind};

use crate::*;

impl<'a> SemanticsAnalyzer<'a> {
    pub(crate) fn analyze_type_expr(&mut self, type_expr_key: TypeExprKey) -> TypeKey {
        let type_expr = &self.ast.types_exprs.all[type_expr_key];
        let typ = match type_expr {
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
        self.typed_ast.types_pool.get_key(&typ)
    }

    fn analyze_path_type_expr(&mut self, key: PathTypeExprKey) -> Type {
        let path_type = &self.ast.state.types_paths[key];
        match path_type {
            Item::UnitStruct { vis: _, key } => self.analyze_unit_struct(*key),
            Item::TupleStruct { vis: _, key } => self.analyze_tuple_struct(*key),
            Item::FieldsStruct { vis: _, key } => self.analyze_fields_struct(*key),
            _ => unreachable!(),
        }
    }

    fn analyze_unit_struct(&mut self, key: UnitStructKey) -> Type {
        let info = &self.ast.unit_structs[key].info;
        let file_path = &self.files_infos[info.file_key].path;
        if file_path != "أساسي.نظم" {
            return Type {
                kind: TypeKind::UnitStruct(key),
                size: 0,
            };
        }

        match info.id_key {
            IdKey::I_TYPE => Type::I_TYPE,
            IdKey::I1_TYPE => Type::I1_TYPE,
            IdKey::I2_TYPE => Type::I2_TYPE,
            IdKey::I4_TYPE => Type::I4_TYPE,
            IdKey::I8_TYPE => Type::I8_TYPE,
            IdKey::U_TYPE => Type::U_TYPE,
            IdKey::U1_TYPE => Type::U1_TYPE,
            IdKey::U2_TYPE => Type::U2_TYPE,
            IdKey::U4_TYPE => Type::U4_TYPE,
            IdKey::U8_TYPE => Type::U8_TYPE,
            IdKey::F4_TYPE => Type::F4_TYPE,
            IdKey::F8_TYPE => Type::F8_TYPE,
            IdKey::BOOL_TYPE => Type::BOOL_TYPE,
            IdKey::CHAR_TYPE => Type::CHAR_TYPE,
            IdKey::STR_TYPE => Type::STR_TYPE,
            _ => unreachable!(),
        }
    }

    fn analyze_tuple_struct(&mut self, key: TupleStructKey) -> Type {
        todo!()
    }

    fn analyze_fields_struct(&mut self, key: FieldsStructKey) -> Type {
        todo!()
    }
}
