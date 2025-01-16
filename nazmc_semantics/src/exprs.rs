use nazmc_ast::{Expr, ExprKey, LiteralExpr};

use crate::{typed_ast::Type, SemanticsAnalyzer};

impl<'a> SemanticsAnalyzer<'a> {
    pub(crate) fn analyze_expr(&mut self, expr_key: ExprKey) {
        let Expr { span, kind } = &self.ast.exprs[expr_key];

        let typ = match kind {
            nazmc_ast::ExprKind::Literal(LiteralExpr::Num(nazmc_ast::NumKind::UnspecifiedInt(
                _,
            ))) => todo!(),
            nazmc_ast::ExprKind::Literal(LiteralExpr::Num(
                nazmc_ast::NumKind::UnspecifiedFloat(_),
            )) => todo!(),
            nazmc_ast::ExprKind::Literal(LiteralExpr::Str(_)) => {
                Type::Ref(self.types_pool.get_key(&Type::Str))
            }
            nazmc_ast::ExprKind::Literal(LiteralExpr::Num(nazmc_ast::NumKind::I(_))) => Type::I,
            nazmc_ast::ExprKind::Literal(LiteralExpr::Num(nazmc_ast::NumKind::I1(_))) => Type::I1,
            nazmc_ast::ExprKind::Literal(LiteralExpr::Num(nazmc_ast::NumKind::I2(_))) => Type::I2,
            nazmc_ast::ExprKind::Literal(LiteralExpr::Num(nazmc_ast::NumKind::I4(_))) => Type::I4,
            nazmc_ast::ExprKind::Literal(LiteralExpr::Num(nazmc_ast::NumKind::I8(_))) => Type::I8,
            nazmc_ast::ExprKind::Literal(LiteralExpr::Num(nazmc_ast::NumKind::U(_))) => Type::U,
            nazmc_ast::ExprKind::Literal(LiteralExpr::Num(nazmc_ast::NumKind::U1(_))) => Type::U1,
            nazmc_ast::ExprKind::Literal(LiteralExpr::Num(nazmc_ast::NumKind::U2(_))) => Type::U2,
            nazmc_ast::ExprKind::Literal(LiteralExpr::Num(nazmc_ast::NumKind::U4(_))) => Type::U4,
            nazmc_ast::ExprKind::Literal(LiteralExpr::Num(nazmc_ast::NumKind::U8(_))) => Type::U8,
            nazmc_ast::ExprKind::Literal(LiteralExpr::Num(nazmc_ast::NumKind::F4(_))) => Type::F4,
            nazmc_ast::ExprKind::Literal(LiteralExpr::Num(nazmc_ast::NumKind::F8(_))) => Type::F8,
            nazmc_ast::ExprKind::Literal(LiteralExpr::Bool(_)) => Type::Bool,
            nazmc_ast::ExprKind::Literal(LiteralExpr::Char(_)) => Type::Char,
            nazmc_ast::ExprKind::Unit => Type::Unit,

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
            nazmc_ast::ExprKind::ArrayElemnts(thin_vec) => todo!(),
            nazmc_ast::ExprKind::ArrayElemntsSized(array_elements_sized_expr) => todo!(),
            nazmc_ast::ExprKind::If(if_expr) => todo!(),
            nazmc_ast::ExprKind::Lambda(lambda_expr) => todo!(),
            nazmc_ast::ExprKind::UnaryOp(unary_op_expr) => todo!(),
            nazmc_ast::ExprKind::BinaryOp(binary_op_expr) => todo!(),
            nazmc_ast::ExprKind::Return(expr_key) | nazmc_ast::ExprKind::Break(expr_key) => {
                if let Some(expr_key) = expr_key {
                    self.analyze_expr(*expr_key);
                }
                Type::Never
            }
            nazmc_ast::ExprKind::Continue => Type::Never,
            nazmc_ast::ExprKind::On => todo!(),
        };

        self.typed_ast
            .exprs
            .insert(expr_key, self.types_pool.get_key(&typ));
    }
}
