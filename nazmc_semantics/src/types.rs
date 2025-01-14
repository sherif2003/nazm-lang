use crate::*;

impl<'a> SemanticsAnalyzer<'a> {
    fn analyze_type(&mut self, type_expr: TypeExpr) {
        match type_expr {
            TypeExpr::Path(path_type_expr_key) => todo!(),
            TypeExpr::Paren(paren_type_expr_key) => todo!(),
            TypeExpr::Slice(slice_type_expr_key) => todo!(),
            TypeExpr::Ptr(ptr_type_expr_key) => todo!(),
            TypeExpr::Ref(ref_type_expr_key) => todo!(),
            TypeExpr::PtrMut(ptr_mut_type_expr_key) => todo!(),
            TypeExpr::RefMut(ref_mut_type_expr_key) => todo!(),
            TypeExpr::Tuple(tuple_type_expr_key) => todo!(),
            TypeExpr::Array(array_type_expr_key) => todo!(),
            TypeExpr::Lambda(lambda_type_expr_key) => todo!(),
        }
    }
}
