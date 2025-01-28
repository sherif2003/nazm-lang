mod consts;
mod errors;
mod exprs;
mod type_infer;
mod typed_ast;
mod types;

use nazmc_data_pool::{
    typed_index_collections::{ti_vec, TiSlice, TiVec},
    DataPoolBuilder, IdKey,
};

pub(crate) use nazmc_ast::*;
use nazmc_diagnostics::{
    eprint_diagnostics, file_info::FileInfo, span::Span, CodeWindow, Diagnostic,
};
use std::{collections::HashMap, process::exit};
use thin_vec::ThinVec;
use type_infer::Substitution;
use typed_ast::{
    ArrayType, ConTy, ConcreteType, FnPtrType, LambdaType, LetStm, TupleType, Ty, Type, TypeVarKey,
    TypedAST,
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
    s: Substitution,
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

        // for expr_key in self.ast.exprs.keys() {
        //     self.analyze_expr(expr_key);
        // }

        for fn_key in self.ast.fns.keys() {
            self.analyze_fn_signature(fn_key);
        }

        // TODO: Remove the clone
        // Don't take the ownership as the errors module requires the access to them
        for _fn in &self.ast.fns.clone() {
            self.current_file_key = _fn.info.file_key;
            self.analyze_scope(_fn.scope_key);
        }

        println!("Exprs types:");
        println!("Len: {}", self.typed_ast.exprs.values().len());
        for ty in self.typed_ast.exprs.values() {
            println!("{}", self.fmt_ty(&ty))
        }

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
                    let let_stm_type = if let Some(expr_key) = self.ast.lets[*let_stm_key].assign {
                        let mut expr_ty = self.infer(expr_key);

                        let mut let_stm_type =
                            if let Some(type_expr_key) = self.ast.lets[*let_stm_key].binding.typ {
                                self.analyze_type_expr(type_expr_key).0
                            } else {
                                self.s.new_unknown_ty_var()
                            };

                        println!("Let type: {:#?}", let_stm_type.inner());

                        let expr_span = self.ast.exprs[expr_key].span;

                        println!("Expr type: {:#?}", expr_ty.inner());

                        if let Err(err) = self.s.unify(&let_stm_type, &expr_ty) {
                            self.add_type_mismatch_err(&let_stm_type, &expr_ty, expr_span);
                        }

                        expr_ty = self.s.apply(&expr_ty);
                        let_stm_type = self.s.apply(&let_stm_type);

                        println!("Expr inferred type: {:#?}", expr_ty.inner());

                        let_stm_type
                    } else {
                        self.s.new_unknown_ty_var()
                    };

                    println!("Let inferred type: {:#?}", let_stm_type.inner());
                    println!("===============");

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
                Stm::While(expr_key, scope_key) => todo!(),
                Stm::If(if_expr) => todo!(),
                Stm::Expr(expr_key) => {
                    self.infer(*expr_key);
                }
            }
        }

        self.ast.scopes[scope_key].stms = stms;

        // self.typed_ast.substitutions = self
        //     .typed_ast
        //     .substitutions
        //     .keys()
        //     .map(|ty_var_key| self.s.apply(&Ty::type_var(ty_var_key)))
        //     .collect();

        // println!("{:#?}\n!!!!!!!!!!!!!!!!!!!!\n", self.s)
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
                    // self.unify(ty, &found_ty, span);
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
            let ty = self.s.new_unknown_ty_var();
            self.set_bindnig_ty(let_stm_key, kind.clone(), &ty);
            tuple_types.push(ty);
        }

        Ty::tuple(tuple_types)
    }
}
