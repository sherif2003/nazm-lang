use std::vec;

use nazmc_ast::{ExprKey, LiteralExpr};
use nazmc_diagnostics::{span::Span, Diagnostic};
use thin_vec::ThinVec;

use crate::{
    typed_ast::{ArrayType, TupleType, Ty, Type},
    SemanticsAnalyzer,
};

impl<'a> SemanticsAnalyzer<'a> {
    pub(crate) fn infer_expr(&mut self, expected_ty: &Ty, expr_key: ExprKey) {
        // Take the ownership (good for vectors as in ExprKind::ArrayElemnts) instead of cloning, then restoring it
        let kind = std::mem::take(&mut self.ast.exprs[expr_key].kind);
        let expr_span = self.ast.exprs[expr_key].span;

        let kind = match kind {
            nazmc_ast::ExprKind::Unit => {
                self.unify(expected_ty, &[Type::Unit], &[Type::Unknown], expr_span);
                nazmc_ast::ExprKind::Unit
            }
            nazmc_ast::ExprKind::Literal(lit_expr) => {
                self.infer_lit_expr(expected_ty, &lit_expr, expr_span);
                nazmc_ast::ExprKind::Literal(lit_expr)
            }
            nazmc_ast::ExprKind::PathNoPkg(path_no_pkg_key) => todo!(),
            nazmc_ast::ExprKind::PathInPkg(path_with_pkg_key) => todo!(),
            nazmc_ast::ExprKind::Call(call_expr) => todo!(),
            nazmc_ast::ExprKind::UnitStruct(unit_struct_path_key) => todo!(),
            nazmc_ast::ExprKind::TupleStruct(tuple_struct_expr) => todo!(),
            nazmc_ast::ExprKind::FieldsStruct(fields_struct_expr) => todo!(),
            nazmc_ast::ExprKind::Field(field_expr) => todo!(),
            nazmc_ast::ExprKind::Idx(idx_expr) => todo!(),
            nazmc_ast::ExprKind::TupleIdx(tuple_idx_expr) => todo!(),
            nazmc_ast::ExprKind::Tuple(exprs) => {
                self.infer_tuple_expr(expected_ty, &exprs, expr_span);
                nazmc_ast::ExprKind::Tuple(exprs)
            }
            nazmc_ast::ExprKind::ArrayElemnts(thin_vec) => todo!(),
            nazmc_ast::ExprKind::ArrayElemntsSized(array_elements_sized_expr) => todo!(),
            nazmc_ast::ExprKind::If(if_expr) => todo!(),
            nazmc_ast::ExprKind::Lambda(lambda_expr) => todo!(),
            nazmc_ast::ExprKind::UnaryOp(unary_op_expr) => todo!(),
            nazmc_ast::ExprKind::BinaryOp(binary_op_expr) => todo!(),
            nazmc_ast::ExprKind::Return(expr_key) => todo!(),
            nazmc_ast::ExprKind::Break(expr_key) => todo!(),
            nazmc_ast::ExprKind::Continue => todo!(),
            nazmc_ast::ExprKind::On => todo!(),
        };

        // Restore the ownership
        self.ast.exprs[expr_key].kind = kind;

        self.typed_ast.exprs.insert(expr_key, expected_ty.clone());
    }

    fn infer_lit_expr(&mut self, expected_ty: &Ty, lit_expr: &LiteralExpr, expr_span: Span) {
        match lit_expr {
            LiteralExpr::Str(_) => self.unify(
                expected_ty,
                &[Type::Ref(Ty::new(Type::Str))],
                &[Type::Unknown],
                expr_span,
            ),
            LiteralExpr::Char(_) => {
                self.unify(expected_ty, &[Type::Char], &[Type::Unknown], expr_span)
            }
            LiteralExpr::Bool(_) => {
                self.unify(expected_ty, &[Type::Bool], &[Type::Unknown], expr_span)
            }
            LiteralExpr::Num(num_kind) => match num_kind {
                nazmc_ast::NumKind::F4(_) => self.unify(
                    expected_ty,
                    &[Type::F4],
                    &[Type::Unknown, Type::UnspecifiedFloat],
                    expr_span,
                ),
                nazmc_ast::NumKind::F8(_) => self.unify(
                    expected_ty,
                    &[Type::F8],
                    &[Type::Unknown, Type::UnspecifiedFloat],
                    expr_span,
                ),
                nazmc_ast::NumKind::I(_) => self.unify(
                    expected_ty,
                    &[Type::I],
                    &[
                        Type::Unknown,
                        Type::UnspecifiedUnsignedInt,
                        Type::UnspecifiedSignedInt,
                    ],
                    expr_span,
                ),
                nazmc_ast::NumKind::I1(_) => self.unify(
                    expected_ty,
                    &[Type::I1],
                    &[
                        Type::Unknown,
                        Type::UnspecifiedUnsignedInt,
                        Type::UnspecifiedSignedInt,
                    ],
                    expr_span,
                ),
                nazmc_ast::NumKind::I2(_) => self.unify(
                    expected_ty,
                    &[Type::I2],
                    &[
                        Type::Unknown,
                        Type::UnspecifiedUnsignedInt,
                        Type::UnspecifiedSignedInt,
                    ],
                    expr_span,
                ),
                nazmc_ast::NumKind::I4(_) => self.unify(
                    expected_ty,
                    &[Type::I4],
                    &[
                        Type::Unknown,
                        Type::UnspecifiedUnsignedInt,
                        Type::UnspecifiedSignedInt,
                    ],
                    expr_span,
                ),
                nazmc_ast::NumKind::I8(_) => self.unify(
                    expected_ty,
                    &[Type::I8],
                    &[
                        Type::Unknown,
                        Type::UnspecifiedUnsignedInt,
                        Type::UnspecifiedSignedInt,
                    ],
                    expr_span,
                ),
                nazmc_ast::NumKind::U(_) => self.unify(
                    expected_ty,
                    &[Type::U],
                    &[Type::Unknown, Type::UnspecifiedUnsignedInt],
                    expr_span,
                ),
                nazmc_ast::NumKind::U1(_) => self.unify(
                    expected_ty,
                    &[Type::U1],
                    &[Type::Unknown, Type::UnspecifiedUnsignedInt],
                    expr_span,
                ),
                nazmc_ast::NumKind::U2(_) => self.unify(
                    expected_ty,
                    &[Type::U2],
                    &[Type::Unknown, Type::UnspecifiedUnsignedInt],
                    expr_span,
                ),
                nazmc_ast::NumKind::U4(_) => self.unify(
                    expected_ty,
                    &[Type::U4],
                    &[Type::Unknown, Type::UnspecifiedUnsignedInt],
                    expr_span,
                ),
                nazmc_ast::NumKind::U8(_) => self.unify(
                    expected_ty,
                    &[Type::U8],
                    &[Type::Unknown, Type::UnspecifiedUnsignedInt],
                    expr_span,
                ),
                nazmc_ast::NumKind::UnspecifiedInt(_) => self.unify(
                    expected_ty,
                    &[
                        Type::U8,
                        Type::U4,
                        Type::U2,
                        Type::U1,
                        Type::U,
                        Type::I8,
                        Type::I4,
                        Type::I2,
                        Type::I1,
                        Type::I,
                        Type::UnspecifiedSignedInt,
                        Type::UnspecifiedUnsignedInt,
                    ],
                    &[Type::Unknown],
                    expr_span,
                ),
                nazmc_ast::NumKind::UnspecifiedFloat(_) => self.unify(
                    expected_ty,
                    &[Type::F8, Type::F4, Type::UnspecifiedFloat],
                    &[Type::Unknown],
                    expr_span,
                ),
            },
        }
    }

    fn infer_tuple_expr(&mut self, expected_ty: &Ty, exprs: &[ExprKey], expr_span: Span) {
        let inner = expected_ty.inner();
        if let Type::Tuple(TupleType { types }) = inner {
            if types.len() == exprs.len() {
                for i in 0..exprs.len() {
                    let expr_key = exprs[i];
                    self.infer_expr(&types[i], expr_key);
                }
            } else {
                let found_ty = self.infer_tuple_expr_with_unknown(exprs);
                self.add_type_mismatch_err(expected_ty, &found_ty, expr_span);
            }
        } else {
            let tuple_ty = self.infer_tuple_expr_with_unknown(exprs);
            self.unify(
                expected_ty,
                &[tuple_ty.inner()],
                &[Type::Unknown],
                expr_span,
            );
        }
    }

    fn infer_tuple_expr_with_unknown(&mut self, exprs: &[ExprKey]) -> Ty {
        let mut tuple_types = ThinVec::with_capacity(exprs.len());

        for i in 0..exprs.len() {
            let expr_key = exprs[i];
            tuple_types.push(Ty::new(Type::Unknown));
            self.infer_expr(&tuple_types[i], expr_key);
        }
        Ty::new(Type::Tuple(TupleType { types: tuple_types }))
    }

    pub(crate) fn analyze_expr(&mut self, expr_key: ExprKey) -> Ty {
        // Take the ownership (good for vectors as in ExprKind::ArrayElemnts) instead of cloning, then restoring it
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
                (kind, Type::Ref(Ty::new(Type::Str)))
            }
            kind @ nazmc_ast::ExprKind::Literal(LiteralExpr::Num(
                nazmc_ast::NumKind::UnspecifiedInt(_),
            )) => (kind, Type::UnspecifiedUnsignedInt),
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
                let array_type_key = self.analyze_array_elements(&array_elements);
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

        Ty::new(typ)
    }

    fn analyze_array_elements(&mut self, array_elements: &[ExprKey]) -> ArrayType {
        let size = array_elements.len() as u32;

        let first_expr_span = array_elements
            .first()
            .map(|expr_key| self.ast.exprs[*expr_key].span);

        let underlying_typ = Ty::new(Type::Unknown);

        for expr_key in array_elements {
            let expr_typ = self.analyze_expr(*expr_key);

            if self.is_subtype_of(&underlying_typ.borrow(), &expr_typ.borrow()) {
                continue;
            }

            let first_expr_span = first_expr_span.unwrap();
            let expr_span = self.ast.exprs[*expr_key].span;

            let diagnostic = Diagnostic::error("أنواع غير متطابقة".into(), vec![]);
            self.diagnostics.push(diagnostic);
            break;
        }

        ArrayType {
            underlying_typ,
            size,
        }
    }
}
