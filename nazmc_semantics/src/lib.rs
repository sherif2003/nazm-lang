mod consts;
mod errors;
mod exprs;
mod type_infer;
mod typed_ast;
mod types;

use nazmc_data_pool::{typed_index_collections::TiSlice, IdKey};

pub(crate) use nazmc_ast::*;
use nazmc_diagnostics::{
    eprint_diagnostics, file_info::FileInfo, span::Span, CodeWindow, Diagnostic,
};
use std::{collections::HashMap, process::exit};
use thin_vec::ThinVec;
use type_infer::Substitution;
use typed_ast::{
    ConcreteType, FnPtrType, LambdaType, LetStm, TupleType, Ty, Type, TypeVarKey, TypedAST,
};

#[derive(Clone, Copy, Default, PartialEq, Eq)]
enum CycleDetected {
    #[default]
    None,
    Const(ConstKey),
    TupleStruct(TupleStructKey),
    FieldsStruct(FieldsStructKey),
}

#[derive(Default)]
struct SemanticsStack {
    consts: HashMap<ConstKey, ()>,
    tuple_structs: HashMap<TupleStructKey, ()>,
    fields_structs: HashMap<FieldsStructKey, ()>,
    is_cycle_detected: CycleDetected,
}

#[derive(Default)]
pub struct SemanticsAnalyzer<'a> {
    files_infos: &'a TiSlice<FileKey, FileInfo>,
    files_to_pkgs: &'a TiSlice<FileKey, PkgKey>,
    id_pool: &'a TiSlice<IdKey, String>,
    pkgs_names: &'a TiSlice<PkgKey, &'a ThinVec<IdKey>>,
    ast: AST<Resolved>,
    typed_ast: TypedAST,
    semantics_stack: SemanticsStack,
    diagnostics: Vec<Diagnostic<'a>>,
    cycle_stack: Vec<Diagnostic<'a>>,
    current_file_key: FileKey,
    current_fn_key: FnKey,
    s: Substitution,
    /// For fns and lambdas only
    current_scope_expected_return_ty: Ty,
    is_current_fn_scope: bool,
}

impl<'a> SemanticsAnalyzer<'a> {
    pub fn new(
        files_infos: &'a TiSlice<FileKey, FileInfo>,
        files_to_pkgs: &'a TiSlice<FileKey, PkgKey>,
        id_pool: &'a TiSlice<IdKey, String>,
        pkgs_names: &'a TiSlice<PkgKey, &'a ThinVec<IdKey>>,
        ast: nazmc_ast::AST<Resolved>,
    ) -> Self {
        Self {
            files_infos,
            files_to_pkgs,
            id_pool,
            pkgs_names,
            typed_ast: TypedAST {
                consts: HashMap::with_capacity(ast.consts.len()),
                statics: HashMap::with_capacity(ast.statics.len()),
                tuple_structs: HashMap::with_capacity(ast.tuple_structs.len()),
                fields_structs: HashMap::with_capacity(ast.fields_structs.len()),
                fns_signatures: HashMap::with_capacity(ast.fns.len()),
                lets: HashMap::with_capacity(ast.lets.len()),
                exprs: HashMap::with_capacity(ast.exprs.len()),
            },
            ast,
            ..Default::default()
        }
    }

    pub fn analyze(mut self) {
        // for type_expr_key in self.ast.types_exprs.all.keys() {
        //     self.analyze_type_expr(type_expr_key);
        // }

        for struct_key in self.ast.fields_structs.keys() {
            self.analyze_fields_struct(struct_key);
        }

        for fn_key in self.ast.fns.keys() {
            self.analyze_fn_signature(fn_key);
        }

        // Will be restored to true by inner lambda scopes
        self.is_current_fn_scope = true;

        // TODO: Remove the clone
        // Don't take the ownership as the errors module requires the access to them
        for (fn_key, _fn) in self.ast.fns.clone().iter_enumerated() {
            self.current_file_key = _fn.info.file_key;
            self.current_fn_key = fn_key;

            self.current_scope_expected_return_ty =
                if let Type::FnPtr(fn_ptr) = &*self.typed_ast.fns_signatures[&fn_key].borrow() {
                    fn_ptr.return_type.clone()
                } else {
                    unreachable!()
                };

            let found_return_ty = self.infer_scope(_fn.scope_key);

            if let Err(err) = self
                .s
                .unify(&self.current_scope_expected_return_ty, &found_return_ty)
            {
                let type_expr_span = self.get_type_expr_span(_fn.return_type);

                let span = self.ast.scopes[_fn.scope_key]
                    .return_expr
                    .map_or(type_expr_span, |expr_key| self.get_expr_span(expr_key));

                self.add_type_mismatch_in_fn_return_ty_err(
                    fn_key,
                    &self.current_scope_expected_return_ty.clone(),
                    &found_return_ty,
                    type_expr_span,
                    span,
                );
            }
        }

        let unknown_vars = self.s.collect();

        for (ty_var_key, _) in unknown_vars {
            let (ty_state, file_key, span) = self.s.all_ty_vars[ty_var_key];

            let mut code_window = CodeWindow::new(&self.files_infos[file_key], span.start);
            code_window.mark_error(span, vec!["لا يمكن تحديد النوع هنا ضمنياً".into()]);
            let diagnostic =
                Diagnostic::error("يجب تحديد النوع بشكل خارجي".into(), vec![code_window]);
            self.diagnostics.push(diagnostic);
        }

        println!("Exprs types:");
        println!("Len: {}", self.typed_ast.exprs.values().len());
        for (key, ty) in &self.typed_ast.exprs {
            let ty = self.s.apply(ty);
            println!(
                "{:?}, Ty: {}, Kind: {:?}, Type: {:?}",
                *key,
                self.fmt_ty(&ty),
                self.ast.exprs[*key].kind,
                ty.inner()
            )
        }

        println!("{:#?}\n!!!!!!!!!!!!!!!!!!!!\n", self.s);

        if !self.diagnostics.is_empty() {
            eprint_diagnostics(self.diagnostics);
            exit(1);
        }

        // TODO
    }

    fn analyze_fn_signature(&mut self, fn_key: FnKey) {
        let params = self.ast.fns[fn_key]
            .params
            .clone() // TODO: Remove the clone
            .iter()
            .map(|(_, type_expr_key)| self.analyze_type_expr(*type_expr_key).0)
            .collect::<ThinVec<_>>();

        let return_type = self.analyze_type_expr(self.ast.fns[fn_key].return_type).0;

        self.typed_ast
            .fns_signatures
            .insert(fn_key, Ty::fn_ptr(params, return_type));
    }

    fn analyze_scope(&mut self, scope_key: ScopeKey) {
        let stms = std::mem::take(&mut self.ast.scopes[scope_key].stms);

        for stm in &stms {
            match stm {
                Stm::Let(let_stm_key) => {
                    let let_stm_type =
                        if let Some(type_expr_key) = self.ast.lets[*let_stm_key].binding.typ {
                            self.analyze_type_expr(type_expr_key).0
                        } else {
                            self.s.new_unknown_ty_var(
                                self.current_file_key,
                                self.ast.lets[*let_stm_key].binding.kind.get_span(),
                            )
                        };

                    if let Some(expr_key) = self.ast.lets[*let_stm_key].assign {
                        let expr_ty = self.infer(expr_key);

                        if let Err(err) = self.s.unify(&let_stm_type, &expr_ty) {
                            let expected_type_expr_key =
                                self.ast.lets[*let_stm_key].binding.typ.unwrap();
                            self.add_type_mismatch_in_let_stm_err(
                                &let_stm_type,
                                &expr_ty,
                                expected_type_expr_key,
                                expr_key,
                            );
                        }
                    }

                    self.typed_ast.lets.insert(
                        *let_stm_key,
                        LetStm {
                            bindings: HashMap::new(),
                        },
                    );

                    self.set_bindnig_ty(
                        *let_stm_key,
                        self.ast.lets[*let_stm_key].binding.kind.clone(),
                        &let_stm_type,
                    );
                }
                Stm::While(while_stm) => {
                    let WhileStm {
                        while_keyword_span,
                        cond_expr_key: while_cond_expr_key,
                        scope_key: while_scope_key,
                    } = **while_stm;

                    let while_cond_ty = self.infer(while_cond_expr_key);

                    if let Err(err) = self.s.unify(&Ty::boolean(), &while_cond_ty) {
                        self.add_branch_stm_condition_type_mismatch_err(
                            &while_cond_ty,
                            "طالما",
                            while_keyword_span,
                            while_cond_expr_key,
                        );
                    }

                    let while_scope_ty = self.infer_scope(while_scope_key);

                    if let Err(err) = self.s.unify(&Ty::unit(), &while_scope_ty) {
                        self.add_while_stm_should_return_unit_err(
                            &while_scope_ty,
                            while_keyword_span,
                            while_scope_key,
                        );
                    }
                }
                Stm::If(if_expr) => {
                    self.infer_if_expr(&if_expr);
                }
                Stm::Expr(expr_key) => {
                    self.infer(*expr_key);
                }
            }
        }

        self.ast.scopes[scope_key].stms = stms;
    }

    fn set_bindnig_ty(&mut self, let_stm_key: LetStmKey, kind: BindingKind, ty: &Ty) {
        match kind {
            BindingKind::Id(id) => {
                self.typed_ast
                    .lets
                    .get_mut(&let_stm_key)
                    .unwrap()
                    .bindings
                    .insert(id.id, ty.clone());
            }
            BindingKind::MutId { id, .. } => {
                self.typed_ast
                    .lets
                    .get_mut(&let_stm_key)
                    .unwrap()
                    .bindings
                    .insert(id.id, ty.clone());
            }
            BindingKind::Tuple(kinds, span) => {
                if let Type::Tuple(TupleType { types }) = ty.inner() {
                    if kinds.len() == types.len() {
                        for i in 0..kinds.len() {
                            let kind = &kinds[i];
                            let ty = &types[i];
                            self.set_bindnig_ty(let_stm_key, kind.clone(), ty);
                        }
                    } else {
                        let found_ty =
                            self.destructed_tuple_to_ty_with_unknown(let_stm_key, &kinds);
                        self.add_type_mismatch_err(ty, &found_ty, span);
                    }
                } else {
                    let found_ty = self.destructed_tuple_to_ty_with_unknown(let_stm_key, &kinds);
                    if let Err(err) = self.s.unify(ty, &found_ty) {
                        self.add_type_mismatch_err(&ty, &found_ty, span);
                    }
                }
            }
        }
    }

    fn destructed_tuple_to_ty_with_unknown(
        &mut self,
        let_stm_key: LetStmKey,
        kinds: &[BindingKind],
    ) -> Ty {
        let mut tuple_types = ThinVec::with_capacity(kinds.len());
        for i in 0..kinds.len() {
            let kind = &kinds[i];
            let ty = self
                .s
                .new_unknown_ty_var(self.current_file_key, kind.get_span());
            self.set_bindnig_ty(let_stm_key, kind.clone(), &ty);
            tuple_types.push(ty);
        }

        Ty::tuple(tuple_types)
    }
}
