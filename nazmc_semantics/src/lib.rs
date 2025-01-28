mod consts;
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
                substitutions: TiVec::new(),
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

        let fns = std::mem::take(&mut self.ast.fns);

        for _fn in fns {
            self.current_file_key = _fn.info.file_key;
            self.analyze_scope(_fn.scope_key);
        }

        for ty_var in self.typed_ast.substitutions {
            println!("{:#?}", ty_var.inner());
            println!("-------------------------")
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
        let mut s = Substitution::new();

        for stm in &stms {
            match stm {
                Stm::Let(let_stm_key) => {
                    let let_stm_type = if let Some(expr_key) = self.ast.lets[*let_stm_key].assign {
                        let (substitution, mut expr_ty) = self.infer(expr_key);

                        let mut let_stm_type =
                            if let Some(type_expr_key) = self.ast.lets[*let_stm_key].binding.typ {
                                self.analyze_type_expr(type_expr_key).0
                            } else {
                                self.new_unknown_ty_var()
                            };

                        println!("Let type: {:#?}", let_stm_type.inner());

                        s += substitution;

                        let expr_span = self.ast.exprs[expr_key].span;

                        expr_ty = s.apply(&expr_ty);
                        let_stm_type = s.apply(&let_stm_type);

                        println!("Expr type: {:#?}", expr_ty.inner());

                        match Substitution::unify(&let_stm_type, &expr_ty) {
                            Ok(substitution) => {
                                s += substitution;
                            }
                            Err(err) => {
                                self.add_type_mismatch_err(&let_stm_type, &expr_ty, expr_span);
                            }
                        }

                        expr_ty = s.apply(&expr_ty);
                        let_stm_type = s.apply(&let_stm_type);

                        println!("Expr inferred type: {:#?}", expr_ty.inner());

                        let_stm_type
                    } else {
                        self.new_unknown_ty_var()
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

        self.typed_ast.substitutions = self
            .typed_ast
            .substitutions
            .keys()
            .map(|ty_var_key| s.apply(&Ty::type_var(ty_var_key)))
            .collect();

        println!("{:#?}\n!!!!!!!!!!!!!!!!!!!!\n", s)
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
            let ty = self.new_unknown_ty_var();
            self.set_bindnig_ty(let_stm_key, kind.clone(), &ty);
            tuple_types.push(ty);
        }

        Ty::tuple(tuple_types)
    }

    fn new_unknown_ty_var(&mut self) -> Ty {
        let ty_var_key = self
            .typed_ast
            .substitutions
            .push_and_get_key(Ty::new(Type::Unknown));

        Ty::type_var(ty_var_key)
    }

    fn new_unspecified_unsigned_int_ty_var(&mut self) -> Ty {
        let ty_var_key = self
            .typed_ast
            .substitutions
            .push_and_get_key(Ty::new(Type::UnspecifiedUnsignedInt));

        Ty::type_var(ty_var_key)
    }

    fn new_unspecified_float_ty_var(&mut self) -> Ty {
        let ty_var_key = self
            .typed_ast
            .substitutions
            .push_and_get_key(Ty::new(Type::UnspecifiedFloat));

        Ty::type_var(ty_var_key)
    }

    fn add_type_mismatch_err(&mut self, expected_ty: &Ty, found_ty: &Ty, span: Span) {
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

    fn fmt_pkg_name(&self, pkg_key: PkgKey) -> String {
        self.pkgs_names[pkg_key]
            .iter()
            .map(|id| self.id_pool[*id].as_str())
            .collect::<Vec<_>>()
            .join("::")
    }

    fn fmt_item_name(&self, item_info: ItemInfo) -> String {
        let pkg = self.fmt_pkg_name(self.files_to_pkgs[item_info.file_key]);
        let name = &self.id_pool[item_info.id_key];
        if pkg.is_empty() {
            name.to_owned()
        } else {
            format!("{}::{}", pkg, name)
        }
    }

    fn fmt_ty(&self, ty: &Ty) -> String {
        match ty.inner() {
            Type::Unknown => format!("_"),
            Type::UnspecifiedUnsignedInt => format!("{{عدد}}"),
            Type::UnspecifiedSignedInt => format!("{{عدد صحيح}}"),
            Type::UnspecifiedFloat => format!("{{عدد عشري}}"),
            Type::TypeVar(_) => todo!(),
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

    fn fmt_con_ty(&self, con_ty: &ConcreteType) -> String {
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
}
