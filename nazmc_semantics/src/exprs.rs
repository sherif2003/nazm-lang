use std::vec;

use nazmc_ast::{Expr, ExprKey, LiteralExpr};
use nazmc_diagnostics::{CodeWindow, Diagnostic};

use crate::{
    typed_ast::{ArrayType, Type},
    SemanticsAnalyzer,
};

impl<'a> SemanticsAnalyzer<'a> {
    pub(crate) fn analyze_expr(&mut self, expr_key: ExprKey) {
        // Take the ownership (good for vectors as in ExprKind::ArrayElemnts) instead of cloning then restoring it
        let kind = std::mem::take(&mut self.ast.exprs[expr_key].kind);

        let (kind, typ) = match kind {
            kind @ nazmc_ast::ExprKind::Return(expr_key)
            | kind @ nazmc_ast::ExprKind::Break(expr_key) => {
                if let Some(expr_key) = expr_key {
                    self.analyze_expr(expr_key);
                }
                (kind, Type::Never)
            }
            kind @ nazmc_ast::ExprKind::Continue => (kind, Type::Never),
            kind @ nazmc_ast::ExprKind::On => todo!(),
            kind @ nazmc_ast::ExprKind::Literal(LiteralExpr::Str(_)) => {
                (kind, Type::Ref(self.types_pool.get_key(&Type::Str)))
            }
            kind @ nazmc_ast::ExprKind::Literal(LiteralExpr::Num(
                nazmc_ast::NumKind::UnspecifiedInt(_),
            )) => (kind, Type::UnspecifiedInt),
            kind @ nazmc_ast::ExprKind::Literal(LiteralExpr::Num(
                nazmc_ast::NumKind::UnspecifiedFloat(_),
            )) => (kind, Type::UnspecifiedFloat),
            kind @ nazmc_ast::ExprKind::Literal(LiteralExpr::Num(nazmc_ast::NumKind::I(_))) => {
                (kind, Type::I)
            }
            kind @ nazmc_ast::ExprKind::Literal(LiteralExpr::Num(nazmc_ast::NumKind::I1(_))) => {
                (kind, Type::I1)
            }
            kind @ nazmc_ast::ExprKind::Literal(LiteralExpr::Num(nazmc_ast::NumKind::I2(_))) => {
                (kind, Type::I2)
            }
            kind @ nazmc_ast::ExprKind::Literal(LiteralExpr::Num(nazmc_ast::NumKind::I4(_))) => {
                (kind, Type::I4)
            }
            kind @ nazmc_ast::ExprKind::Literal(LiteralExpr::Num(nazmc_ast::NumKind::I8(_))) => {
                (kind, Type::I8)
            }
            kind @ nazmc_ast::ExprKind::Literal(LiteralExpr::Num(nazmc_ast::NumKind::U(_))) => {
                (kind, Type::U)
            }
            kind @ nazmc_ast::ExprKind::Literal(LiteralExpr::Num(nazmc_ast::NumKind::U1(_))) => {
                (kind, Type::U1)
            }
            kind @ nazmc_ast::ExprKind::Literal(LiteralExpr::Num(nazmc_ast::NumKind::U2(_))) => {
                (kind, Type::U2)
            }
            kind @ nazmc_ast::ExprKind::Literal(LiteralExpr::Num(nazmc_ast::NumKind::U4(_))) => {
                (kind, Type::U4)
            }
            kind @ nazmc_ast::ExprKind::Literal(LiteralExpr::Num(nazmc_ast::NumKind::U8(_))) => {
                (kind, Type::U8)
            }
            kind @ nazmc_ast::ExprKind::Literal(LiteralExpr::Num(nazmc_ast::NumKind::F4(_))) => {
                (kind, Type::F4)
            }
            kind @ nazmc_ast::ExprKind::Literal(LiteralExpr::Num(nazmc_ast::NumKind::F8(_))) => {
                (kind, Type::F8)
            }
            kind @ nazmc_ast::ExprKind::Literal(LiteralExpr::Bool(_)) => (kind, Type::Bool),
            kind @ nazmc_ast::ExprKind::Literal(LiteralExpr::Char(_)) => (kind, Type::Char),
            kind @ nazmc_ast::ExprKind::Unit => (kind, Type::Unit),
            nazmc_ast::ExprKind::ArrayElemnts(array_elements) => {
                let size = array_elements.len() as u32;

                let mut underlying_typ = None;

                for expr_key in &array_elements {
                    self.analyze_expr(*expr_key);
                    if let Some(underlying_typ) = underlying_typ {
                        let expr_type_key = *self.typed_ast.exprs.get(&expr_key).unwrap();
                        if expr_type_key == underlying_typ {
                            continue;
                        }
                        let diagnostic = Diagnostic::error("النوع غير متساوي".into(), vec![]);
                        self.diagnostics.push(diagnostic);
                        break;
                    } else {
                        underlying_typ = Some(*self.typed_ast.exprs.get(&expr_key).unwrap())
                    }
                }

                let underlying_typ = underlying_typ.unwrap_or(self.types_pool.get_key(&Type::Unit));

                let array_type_key = self.array_types_pool.get_key(&ArrayType {
                    underlying_typ,
                    size,
                });

                (
                    nazmc_ast::ExprKind::ArrayElemnts(array_elements),
                    Type::Array(array_type_key),
                )
            }
            nazmc_ast::ExprKind::ArrayElemntsSized(array_elements_sized_expr) => todo!(),

            nazmc_ast::ExprKind::PathNoPkg(path_no_pkg_key) => todo!(),
            nazmc_ast::ExprKind::PathInPkg(path_with_pkg_key) => todo!(),
            nazmc_ast::ExprKind::Call(call_expr) => todo!(),
            nazmc_ast::ExprKind::UnitStruct(unit_struct_path_key) => todo!(),
            nazmc_ast::ExprKind::TupleStruct(tuple_struct_expr) => todo!(),
            nazmc_ast::ExprKind::FieldsStruct(fields_struct_expr) => todo!(),
            nazmc_ast::ExprKind::Field(field_expr) => todo!(),
            nazmc_ast::ExprKind::Idx(idx_expr) => todo!(),
            nazmc_ast::ExprKind::TupleIdx(tuple_idx_expr) => todo!(),
            nazmc_ast::ExprKind::Tuple(thin_vec) => todo!(),
            nazmc_ast::ExprKind::If(if_expr) => todo!(),
            nazmc_ast::ExprKind::Lambda(lambda_expr) => todo!(),
            nazmc_ast::ExprKind::UnaryOp(unary_op_expr) => todo!(),
            nazmc_ast::ExprKind::BinaryOp(binary_op_expr) => todo!(),
        };

        // Restore the ownership
        self.ast.exprs[expr_key].kind = kind;

        self.typed_ast
            .exprs
            .insert(expr_key, self.types_pool.get_key(&typ));
    }

    fn analyze_array_elements(&mut self, array_elements: &[ExprKey]) -> Type {
        todo!()
    }
}
