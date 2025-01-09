use nazmc_ast::{
    ASTId, ArrayType, ArrayTypeKey, FieldsStructKey, FieldsStructPathKey, FileKey, FnKey, Item,
    ItemInfo, ItemPath, LambdaType, LambdaTypeKey, LiteralExpr, PathNoPkgKey, PathTypeExprKey,
    PathWithPkgKey, PkgKey, PkgPath, ScopeKey, StarImportStm, TupleStructKey, TupleStructPathKey,
    TupleType, TupleTypeKey, Type, TypeExpr, TypeExprKey, TypeKey, Types, UnitStructKey,
    UnitStructPathKey, VisModifier,
};
use nazmc_data_pool::{
    typed_index_collections::{ti_vec, TiSlice, TiVec},
    DataPoolBuilder, IdKey,
};
use nazmc_diagnostics::{
    eprint_diagnostics, file_info::FileInfo, span::Span, CodeWindow, Diagnostic,
};
use std::{collections::HashMap, process::exit};
use thin_vec::ThinVec;

pub struct NameResolver<'a> {
    files_infos: &'a TiSlice<FileKey, FileInfo>,
    id_pool: &'a TiSlice<IdKey, String>,
    pkgs: &'a HashMap<ThinVec<IdKey>, usize>,
    pkgs_names: &'a TiSlice<PkgKey, &'a ThinVec<IdKey>>,
    ast: nazmc_ast::AST<nazmc_ast::Unresolved>,
    all_types_pool: DataPoolBuilder<TypeKey, Type>,
    tuple_types_pool: DataPoolBuilder<TupleTypeKey, TupleType>,
    array_types_pool: DataPoolBuilder<ArrayTypeKey, ArrayType>,
    lambda_types_pool: DataPoolBuilder<LambdaTypeKey, LambdaType>,
    resolved_types_paths: TiVec<PathTypeExprKey, Type>,
    diagnostics: Vec<Diagnostic<'a>>,
}

struct ResolvedImport {
    resolved_item: Item,
}

struct ResolvedStarImport {
    pkg_path_span: Span,
    resolved_pkg_key: PkgKey,
}

impl<'a> NameResolver<'a> {
    pub fn new(
        files_infos: &'a TiSlice<FileKey, FileInfo>,
        id_pool: &'a TiSlice<IdKey, String>,
        pkgs: &'a HashMap<ThinVec<IdKey>, usize>,
        pkgs_names: &'a TiSlice<PkgKey, &'a ThinVec<IdKey>>,
        ast: nazmc_ast::AST<nazmc_ast::Unresolved>,
    ) -> Self {
        Self {
            all_types_pool: DataPoolBuilder::new(),
            tuple_types_pool: DataPoolBuilder::new(),
            array_types_pool: DataPoolBuilder::new(),
            lambda_types_pool: DataPoolBuilder::new(),
            resolved_types_paths: TiVec::with_capacity(ast.state.types_exprs.paths.len()),
            files_infos,
            id_pool,
            pkgs,
            pkgs_names,
            ast,
            diagnostics: vec![],
        }
    }

    pub fn resolve(mut self) -> nazmc_ast::AST<nazmc_ast::Resolved> {
        let paths = std::mem::take(&mut self.ast.state.paths);

        let resolved_imports = paths
            .imports
            .into_iter_enumerated()
            .map(|(file_key, file_imports)| {
                file_imports
                    .into_iter()
                    .map(|import| {
                        let resolved_item = self
                            .resolve_non_pkg_item_path(|_| true, "", file_key, import.item_path)
                            .unwrap_or_default();
                        (
                            import.alias.id,
                            ResolvedImport {
                                // alias_span: import.alias.span,
                                resolved_item,
                            },
                        )
                    })
                    .collect()
            })
            .collect::<TiVec<FileKey, HashMap<_, _>>>();

        let resolved_star_imports = paths
            .star_imports
            .into_iter()
            .map(|star_imports_stms| {
                star_imports_stms
                    .into_iter()
                    .map(
                        |StarImportStm {
                             top_pkg_span,
                             pkg_path,
                         }| {
                            let start_span =
                                top_pkg_span.unwrap_or_else(|| *pkg_path.spans.first().unwrap());

                            let pkg_path_span = if let Some(span) = pkg_path.spans.last() {
                                start_span.merged_with(&span)
                            } else {
                                start_span
                            };

                            let resolved_pkg_key =
                                self.resolve_pkg_path(pkg_path).unwrap_or_default();

                            ResolvedStarImport {
                                pkg_path_span,
                                resolved_pkg_key,
                            }
                        },
                    )
                    .collect()
            })
            .collect::<TiVec<FileKey, ThinVec<_>>>();

        self.resolved_types_paths = std::mem::take(&mut self.ast.state.types_exprs.paths)
            .into_iter()
            .map(|item_path| {
                let file_key = item_path.pkg_path.file_key;

                let item = self
                    .resolve_item_path_from_local_file(
                        file_key,
                        item_path,
                        &resolved_imports,
                        &resolved_star_imports,
                        |item| {
                            matches!(
                                item,
                                Item::UnitStruct { .. }
                                    | Item::TupleStruct { .. }
                                    | Item::FieldsStruct { .. }
                            )
                        },
                        "هيكل",
                    )
                    .unwrap_or_default();

                match item {
                    Item::UnitStruct { vis: _, key } => nazmc_ast::Type::UnitStruct(key),
                    Item::TupleStruct { vis: _, key } => nazmc_ast::Type::TupleStruct(key),
                    Item::FieldsStruct { vis: _, key } => nazmc_ast::Type::FieldsStruct(key),
                    _ => nazmc_ast::Type::UnitStruct(UnitStructKey::default()),
                }
            })
            .collect::<TiVec<PathTypeExprKey, Type>>();

        let resolved_unit_structs_exprs = paths
            .unit_structs_paths_exprs
            .into_iter()
            .map(|item_path| {
                let file_key = item_path.pkg_path.file_key;

                self.resolve_item_path_from_local_file(
                    file_key,
                    item_path,
                    &resolved_imports,
                    &resolved_star_imports,
                    |item| matches!(item, Item::UnitStruct { .. }),
                    explicit_item_kind_to_str(Item::UnitStruct {
                        vis: VisModifier::Default,
                        key: UnitStructKey::default(),
                    }),
                )
                .map(|item| {
                    if let Item::UnitStruct { key, .. } = item {
                        key
                    } else {
                        Default::default()
                    }
                })
                .unwrap_or_default()
            })
            .collect::<TiVec<UnitStructPathKey, UnitStructKey>>();

        let resolved_tuple_structs_exprs = paths
            .tuple_structs_paths_exprs
            .into_iter()
            .map(|item_path| {
                let file_key = item_path.pkg_path.file_key;

                self.resolve_item_path_from_local_file(
                    file_key,
                    item_path,
                    &resolved_imports,
                    &resolved_star_imports,
                    |item| matches!(item, Item::TupleStruct { .. }),
                    explicit_item_kind_to_str(Item::TupleStruct {
                        vis: VisModifier::Default,
                        key: TupleStructKey::default(),
                    }),
                )
                .map(|item| {
                    if let Item::TupleStruct { key, .. } = item {
                        key
                    } else {
                        Default::default()
                    }
                })
                .unwrap_or_default()
            })
            .collect::<TiVec<TupleStructPathKey, TupleStructKey>>();

        let resolved_field_structs_exprs = paths
            .field_structs_paths_exprs
            .into_iter()
            .map(|item_path| {
                let file_key = item_path.pkg_path.file_key;

                self.resolve_item_path_from_local_file(
                    file_key,
                    item_path,
                    &resolved_imports,
                    &resolved_star_imports,
                    |item| matches!(item, Item::FieldsStruct { .. }),
                    explicit_item_kind_to_str(Item::FieldsStruct {
                        vis: VisModifier::Default,
                        key: FieldsStructKey::default(),
                    }),
                )
                .map(|item| {
                    if let Item::FieldsStruct { key, .. } = item {
                        key
                    } else {
                        Default::default()
                    }
                })
                .unwrap_or_default()
            })
            .collect::<TiVec<FieldsStructPathKey, FieldsStructKey>>();

        let resolved_paths_with_pkgs_exprs = paths
            .paths_with_pkgs_exprs
            .into_iter()
            .map(|item_path| {
                let file_key = item_path.pkg_path.file_key;

                self.resolve_item_path_from_local_file(
                    file_key,
                    item_path,
                    &resolved_imports,
                    &resolved_star_imports,
                    |item| {
                        matches!(
                            item,
                            Item::Const { .. } | Item::Static { .. } | Item::Fn { .. }
                        )
                    },
                    "عنصر",
                )
                .unwrap_or_default()
            })
            .collect::<TiVec<PathWithPkgKey, Item>>();

        // Resolve paths that has no leading pkg path
        let mut resolved_paths_no_pkgs_exprs =
            ti_vec![Default::default(); paths.paths_no_pkgs_exprs.len()];

        let mut names_stack = vec![];

        let fns_len = self.ast.fns.len();

        for i in 0..fns_len {
            names_stack.clear();
            let fn_key = FnKey::from(i);
            let at = self.ast.fns[fn_key].info.file_key;
            let scope_key = self.ast.fns[fn_key].scope_key;
            self.resolve_paths_in_scope(
                at,
                &mut names_stack,
                scope_key,
                &paths.paths_no_pkgs_exprs,
                &mut resolved_paths_no_pkgs_exprs,
                &resolved_imports,
                &resolved_star_imports,
            );
        }

        if !self.diagnostics.is_empty() {
            eprint_diagnostics(self.diagnostics);
            exit(1);
        }

        for type_expr_key in self.ast.state.types_exprs.all.keys() {
            self.get_unique_type(type_expr_key);
        }

        if !self.diagnostics.is_empty() {
            eprint_diagnostics(self.diagnostics);
            exit(1);
        }

        let types = Types {
            all: self.all_types_pool.build(),
            tuples: self.tuple_types_pool.build(),
            arrays: self.array_types_pool.build(),
            lambdas: self.lambda_types_pool.build(),
        };

        let state = nazmc_ast::Resolved {
            types,
            unit_structs_paths_exprs: resolved_unit_structs_exprs,
            tuple_structs_paths_exprs: resolved_tuple_structs_exprs,
            field_structs_paths_exprs: resolved_field_structs_exprs,
            paths_no_pkgs_exprs: resolved_paths_no_pkgs_exprs,
            paths_with_pkgs_exprs: resolved_paths_with_pkgs_exprs,
        };

        nazmc_ast::AST {
            state,
            consts: self.ast.consts,
            statics: self.ast.statics,
            unit_structs: self.ast.unit_structs,
            tuple_structs: self.ast.tuple_structs,
            fields_structs: self.ast.fields_structs,
            fns: self.ast.fns,
            scopes: self.ast.scopes,
            lets: self.ast.lets,
        }
    }

    fn get_unique_type(&mut self, type_expr_key: TypeExprKey) -> TypeKey {
        let type_expr = self.ast.state.types_exprs.all[type_expr_key];

        let typ = match type_expr {
            TypeExpr::Path(path_type_expr_key) => self.resolved_types_paths[path_type_expr_key],
            TypeExpr::Paren(paren_type_expr_key) => {
                let type_expr_key =
                    self.ast.state.types_exprs.parens[paren_type_expr_key].underlying_typ;
                let type_key = self.get_unique_type(type_expr_key);
                return type_key;
            }
            TypeExpr::Slice(slice_type_expr_key) => {
                let type_expr_key =
                    self.ast.state.types_exprs.slices[slice_type_expr_key].underlying_typ;
                let type_key = self.get_unique_type(type_expr_key);
                Type::Slice(type_key)
            }
            TypeExpr::Ptr(ptr_type_expr_key) => {
                let type_expr_key =
                    self.ast.state.types_exprs.ptrs[ptr_type_expr_key].underlying_typ;
                let type_key = self.get_unique_type(type_expr_key);
                Type::Ptr(type_key)
            }
            TypeExpr::Ref(ref_type_expr_key) => {
                let type_expr_key =
                    self.ast.state.types_exprs.refs[ref_type_expr_key].underlying_typ;
                let type_key = self.get_unique_type(type_expr_key);
                Type::Ref(type_key)
            }
            TypeExpr::PtrMut(ptr_mut_type_expr_key) => {
                let type_expr_key =
                    self.ast.state.types_exprs.ptrs_mut[ptr_mut_type_expr_key].underlying_typ;
                let type_key = self.get_unique_type(type_expr_key);
                Type::PtrMut(type_key)
            }
            TypeExpr::RefMut(ref_mut_type_expr_key) => {
                let type_expr_key =
                    self.ast.state.types_exprs.refs_mut[ref_mut_type_expr_key].underlying_typ;
                let type_key = self.get_unique_type(type_expr_key);
                Type::RefMut(type_key)
            }
            TypeExpr::Tuple(tuple_type_expr_key) => {
                let types = std::mem::take(
                    &mut self.ast.state.types_exprs.tuples[tuple_type_expr_key].types,
                )
                .into_iter()
                .map(|type_expr_key| self.get_unique_type(type_expr_key))
                .collect();

                let key = self.tuple_types_pool.get_key(&TupleType { types });

                Type::Tuple(key)
            }
            TypeExpr::Array(array_type_expr_key) => {
                // TODO: Support const expressions
                let array_expr = &self.ast.state.types_exprs.arrays[array_type_expr_key];

                let size = if let nazmc_ast::ExprKind::Literal(LiteralExpr::Num(
                    num @ nazmc_ast::NumKind::U(_) | num @ nazmc_ast::NumKind::UnspecifiedInt(_),
                )) = &array_expr.size_expr.kind
                {
                    match num {
                        nazmc_ast::NumKind::U(size) => *size as u32,
                        nazmc_ast::NumKind::UnspecifiedInt(size) => *size as u32,
                        _ => unreachable!(),
                    }
                } else {
                    let mut code_window = CodeWindow::new(
                        &self.files_infos[array_expr.file_key],
                        array_expr.span.start,
                    );
                    code_window.mark_error(
                        array_expr.span,
                        vec!["يجب أن يكون ثابت من النوع ص".to_string()],
                    );
                    let mut diagnostic = Diagnostic::error(
                        "تعبير حجم المصفوفة يجب أن يكون ثابت من النوع ص".to_string(),
                        vec![code_window],
                    );
                    let note = Diagnostic::note(
                        "سيتم دعم التعبيرات الثابتة في المستقبل إن شاء الله".to_string(),
                        vec![],
                    );
                    diagnostic.chain(note);
                    self.diagnostics.push(diagnostic);
                    0
                };

                let underlying_typ = self.get_unique_type(array_expr.underlying_typ);

                let key = self.array_types_pool.get_key(&ArrayType {
                    underlying_typ,
                    size,
                });

                Type::Array(key)
            }
            TypeExpr::Lambda(lambda_type_expr_key) => {
                let params_types = std::mem::take(
                    &mut self.ast.state.types_exprs.lambdas[lambda_type_expr_key].params_types,
                )
                .into_iter()
                .map(|type_expr_key| self.get_unique_type(type_expr_key))
                .collect();

                let return_type = self.get_unique_type(
                    self.ast.state.types_exprs.lambdas[lambda_type_expr_key].return_type,
                );

                let key = self.lambda_types_pool.get_key(&LambdaType {
                    params_types,
                    return_type,
                });

                Type::Lambda(key)
            }
        };

        self.all_types_pool.get_key(&typ)
    }

    fn resolve_paths_in_scope(
        &mut self,
        at: FileKey,
        names_stack: &mut Vec<IdKey>,
        scope_key: ScopeKey,
        paths: &TiSlice<PathNoPkgKey, (ASTId, PkgKey)>,
        resolved_paths: &mut TiSlice<PathNoPkgKey, Item>,
        resolved_imports: &TiSlice<FileKey, HashMap<IdKey, ResolvedImport>>,
        resolved_star_imports: &TiSlice<FileKey, ThinVec<ResolvedStarImport>>,
    ) {
        let scope = &self.ast.scopes[scope_key];

        for name in &scope.extra_params {
            names_stack.push(*name);
        }

        'label: for event in std::mem::take(&mut self.ast.state.scope_events[scope_key]) {
            match event {
                nazmc_ast::ScopeEvent::Let(let_stm_key) => {
                    let let_stm = &self.ast.lets[let_stm_key];
                    nazmc_ast::expand_names_binding_owned(&let_stm.binding.kind, names_stack);
                }
                nazmc_ast::ScopeEvent::Path(path_no_pkg_key) => {
                    let path_expr = &paths[path_no_pkg_key];
                    for name in names_stack.iter().rev() {
                        if *name == path_expr.0.id {
                            resolved_paths[path_no_pkg_key] = Item::LocalVar;
                            continue 'label;
                        }
                    }
                    resolved_paths[path_no_pkg_key] = self
                        .resolve_item_path_with_no_pkg_path(
                            at,
                            path_expr.0,
                            path_expr.1,
                            &resolved_imports,
                            &resolved_star_imports,
                            |item| {
                                matches!(
                                    item,
                                    Item::Const { .. } | Item::Static { .. } | Item::Fn { .. }
                                )
                            },
                            "عنصر",
                        )
                        .unwrap_or_default();
                }
                nazmc_ast::ScopeEvent::Scope(scope_key) => {
                    let len = names_stack.len();

                    self.resolve_paths_in_scope(
                        at,
                        names_stack,
                        scope_key,
                        paths,
                        resolved_paths,
                        resolved_imports,
                        resolved_star_imports,
                    );

                    names_stack.truncate(len);
                }
            }
        }
    }

    fn resolve_non_pkg_item_path(
        &mut self,
        predicate_kind: impl Fn(Item) -> bool,
        expected_kind: &str,
        at: FileKey,
        item_path: ItemPath,
    ) -> Option<Item> {
        let ASTId {
            span: item_path_span,
            id: item_path_id,
        } = item_path.item;

        let at_span = item_path
            .pkg_path
            .spans
            .first()
            .unwrap_or(&item_path_span)
            .merged_with(&item_path_span);

        let at_pkg_key = item_path.pkg_path.pkg_key;

        let (item_pkg_key, resolved_item) = self.resolve_item_path(item_path)?;

        if !predicate_kind(resolved_item) {
            self.add_unexpected_item_kind_err(
                at,
                item_path_span,
                expected_kind,
                explicit_item_kind_to_str(resolved_item),
                resolved_item,
            );
            None
        } else if self.check_resolved_item(
            at,
            at_span,
            at_pkg_key,
            item_pkg_key,
            resolved_item,
            item_path_id,
        ) {
            Some(resolved_item)
        } else {
            None
        }
    }

    fn resolve_item_path_from_local_file(
        &mut self,
        at: FileKey,
        item_path: ItemPath,
        resolved_imports: &TiSlice<FileKey, HashMap<IdKey, ResolvedImport>>,
        resolved_star_imports: &TiSlice<FileKey, ThinVec<ResolvedStarImport>>,
        predicate_kind: impl Fn(Item) -> bool,
        expected_kind: &str,
    ) -> Option<Item> {
        if item_path.pkg_path.ids.is_empty() && item_path.top_pkg_span.is_none() {
            self.resolve_item_path_with_no_pkg_path(
                at,
                item_path.item,
                item_path.pkg_path.pkg_key,
                &resolved_imports,
                &resolved_star_imports,
                predicate_kind,
                expected_kind,
            )
        } else {
            self.resolve_non_pkg_item_path(predicate_kind, expected_kind, at, item_path)
        }
    }

    fn resolve_item_path_with_no_pkg_path(
        &mut self,
        at: FileKey,
        item_ast_id: ASTId,
        item_path_pkg_key: PkgKey,
        resolved_imports: &TiSlice<FileKey, HashMap<IdKey, ResolvedImport>>,
        resolved_star_imports: &TiSlice<FileKey, ThinVec<ResolvedStarImport>>,
        predicate_kind: impl Fn(Item) -> bool,
        expected_kind: &str,
    ) -> Option<Item> {
        if let Some(item) = resolved_imports[at].get(&item_ast_id.id) {
            return Some(item.resolved_item);
        }

        if let Some(item) = self.ast.state.pkgs_to_items[item_path_pkg_key].get(&item_ast_id.id) {
            return if predicate_kind(*item) {
                Some(*item)
            } else {
                self.add_unexpected_item_kind_err(
                    at,
                    item_ast_id.span,
                    expected_kind,
                    explicit_item_kind_to_str(*item),
                    *item,
                );
                None
            };
        }

        let resolved_items_with_same_name = resolved_star_imports[at]
            .iter()
            .filter_map(|star_import| {
                self.ast.state.pkgs_to_items[star_import.resolved_pkg_key]
                    .get(&item_ast_id.id)
                    .filter(|item| predicate_kind(**item))
                    .map(|item| (star_import, item))
            })
            .collect::<Vec<_>>();

        let (accessible_items_with_same_name, inaccessible_items_with_same_name): (
            Vec<(&ResolvedStarImport, &Item)>,
            Vec<(&ResolvedStarImport, &Item)>,
        ) = resolved_items_with_same_name.into_iter().partition(
            |(resolved_star_import, resolved_item)| match resolved_item {
                Item::UnitStruct { vis, .. }
                | Item::TupleStruct { vis, .. }
                | Item::FieldsStruct { vis, .. }
                | Item::Const { vis, .. }
                | Item::Static { vis, .. }
                | Item::Fn { vis, .. }
                    if !matches!(vis, nazmc_ast::VisModifier::Default)
                        || resolved_star_import.resolved_pkg_key == item_path_pkg_key =>
                {
                    true
                }
                _ => false,
            },
        );

        if accessible_items_with_same_name.len() == 1 {
            let (resolved_star_import, resolved_item) =
                *accessible_items_with_same_name.first().unwrap();

            let resolved_item = *resolved_item;

            return if !predicate_kind(resolved_item) {
                self.add_unexpected_item_kind_err(
                    at,
                    item_ast_id.span,
                    expected_kind,
                    explicit_item_kind_to_str(resolved_item),
                    resolved_item,
                );
                None
            } else if self.check_resolved_item(
                at,
                item_ast_id.span,
                item_path_pkg_key,
                resolved_star_import.resolved_pkg_key,
                resolved_item,
                item_ast_id.id,
            ) {
                Some(resolved_item)
            } else {
                None
            };
        } else if accessible_items_with_same_name.is_empty() {
            if inaccessible_items_with_same_name.is_empty() {
                self.add_unresolved_name_err_with_possible_paths(
                    at,
                    item_ast_id.span,
                    item_ast_id.id,
                    |name| {
                        (
                            format!("لم يتم العثور على {} بالاسم `{}`", expected_kind, name),
                            format!(""),
                        )
                    },
                );
            } else if inaccessible_items_with_same_name.len() == 1 {
                let (star_import, item) = inaccessible_items_with_same_name.first().unwrap();

                let item = **item;

                let mut note_code_window =
                    CodeWindow::new(&self.files_infos[at], star_import.pkg_path_span.start);

                note_code_window.mark_note(star_import.pkg_path_span, vec![]);

                let item_kind_str = explicit_item_kind_to_str(item);

                let note = Diagnostic::note(
                    format!(
                        "تم استيراد {} `{}` هنا ضمنياً",
                        item_kind_str, self.id_pool[item_ast_id.id]
                    ),
                    vec![note_code_window],
                );

                let reason = match item {
                    Item::Const { .. } | Item::Static { .. } => "لأنه خاص بالحزمة التابع لها",
                    Item::Fn { .. } => "لأنها خاصة بالحزمة التابعة لها",
                    _ => unreachable!(),
                };

                let mut diagnostic =
                    self.unresolved_name_err(at, item_ast_id.span, item_ast_id.id, |name| {
                        (
                            format!("لا يُمكن الوصول إلى {} `{}` {}", item_kind_str, name, reason),
                            format!(""),
                        )
                    });

                diagnostic.chain(note);

                self.diagnostics.push(diagnostic);
            } else {
                let mut help_code_window = CodeWindow::new(
                    &self.files_infos[at],
                    inaccessible_items_with_same_name
                        .first()
                        .unwrap()
                        .0
                        .pkg_path_span
                        .start,
                );

                for (star_import, _item) in inaccessible_items_with_same_name {
                    help_code_window.mark_help(star_import.pkg_path_span, vec![]);
                }

                let help = Diagnostic::help(
                    format!("تم استيراد ضمنياً عناصر مشابهة من المسارات الآتية ولكنها خاصة بالحزم التابعة لها"),
                    vec![help_code_window],
                );

                let mut diagnostic =
                    self.unresolved_name_err(at, item_ast_id.span, item_ast_id.id, |name| {
                        (
                            format!("لم يتم العثور على {} بالاسم `{}`", expected_kind, name),
                            format!(""),
                        )
                    });

                diagnostic.chain(help);

                self.diagnostics.push(diagnostic);
            }

            return None;
        }

        let name = &self.id_pool[item_ast_id.id];

        let mut code_window = CodeWindow::new(&self.files_infos[at], item_ast_id.span.start);

        code_window.mark_error(
            item_ast_id.span,
            vec!["يوجد أكثر من عنصر بنفس الاسم تم استيرادهم ضمنياً".to_string()],
        );

        let note_msg = match accessible_items_with_same_name.len() {
            2 => {
                format!(
                    "تم استيراد عنصرين بنفس الاسم `{}` من المسارات التالية",
                    name
                )
            }
            n @ 3..=10 => {
                format!(
                    "تم استيراد {} عناصر بنفس الاسم `{}` من المسارات التالية",
                    n, name
                )
            }
            n => {
                format!(
                    "تم استيراد {} عنصر بنفس الاسم `{}` من المسارات التالية",
                    n, name
                )
            }
        };

        let mut note = Diagnostic::note(note_msg, vec![]);

        let mut help_code_window = CodeWindow::new(
            &self.files_infos[at],
            accessible_items_with_same_name
                .first()
                .unwrap()
                .0
                .pkg_path_span
                .start,
        );

        for (resolved_star_import, _resolved_item) in accessible_items_with_same_name {
            help_code_window.mark_note(resolved_star_import.pkg_path_span, vec!["".to_string()]);
        }

        note.push_code_window(help_code_window);

        let msg = format!(
            "الاسم `{}` قد تكرر في الملف ضمنياً، فلا يمكن تحديد أي عنصر يجب استخدامه",
            name
        );

        let mut diagnostic = Diagnostic::error(msg, vec![code_window]);

        diagnostic.chain(note);

        self.diagnostics.push(diagnostic);

        None
    }

    fn resolve_item_path(
        &mut self,
        ItemPath {
            pkg_path,
            item: item_ast_id,
            top_pkg_span: _,
        }: ItemPath,
    ) -> Option<(PkgKey, Item)> {
        let empty_path = pkg_path.ids.is_empty();

        let file_key = pkg_path.file_key;

        let item_pkg_key = self.resolve_pkg_path(pkg_path)?;

        if let Some(item) = self.ast.state.pkgs_to_items[item_pkg_key].get(&item_ast_id.id) {
            Some((item_pkg_key, *item))
        } else {
            self.add_unresolved_name_err_with_possible_paths(
                file_key,
                item_ast_id.span,
                item_ast_id.id,
                |name| {
                    if empty_path {
                        (format!("لم يتم العثور على الاسم `{name}`"), format!(""))
                    } else {
                        (
                            format!("لم يتم العثور على الاسم `{name}` في المسار"),
                            format!("هذا الاسم غير موجود داخل المسار المحدد"),
                        )
                    }
                },
            );
            None
        }
    }

    fn resolve_pkg_path(&mut self, pkg_path: PkgPath) -> Option<PkgKey> {
        match self.pkgs.get(&pkg_path.ids) {
            Some(item_pkg_key) => Some((*item_pkg_key).into()),
            None => {
                self.add_unresolved_name_pkg_path_err(pkg_path);
                None
            }
        }
    }

    fn check_resolved_item(
        &mut self,
        at: FileKey,
        at_span: Span,
        at_pkg_key: PkgKey,
        item_pkg_key: PkgKey,
        resolved_item: Item,
        item_id: IdKey,
    ) -> bool {
        match resolved_item {
            Item::Pkg => {
                self.add_pkgs_cannot_be_imported_err(at, at_span);
                false
            }
            Item::UnitStruct { vis, .. }
            | Item::TupleStruct { vis, .. }
            | Item::FieldsStruct { vis, .. }
            | Item::Const { vis, .. }
            | Item::Static { vis, .. }
            | Item::Fn { vis, .. }
                if matches!(vis, VisModifier::Default) && item_pkg_key != at_pkg_key =>
            {
                self.add_encapsulation_err(at, at_span, resolved_item, item_id);
                false
            }
            _ => true,
        }
    }

    fn add_unexpected_item_kind_err(
        &mut self,
        at: FileKey,
        at_span: Span,
        expected_kind: &str,
        found_kind: &str,
        item: Item,
    ) {
        let msg = format!("يُتوقع {} ولكن تم العثور على {}", expected_kind, found_kind);

        let file_info = &self.files_infos[at];
        let mut code_window = CodeWindow::new(file_info, at_span.start);
        code_window.mark_error(at_span, vec![]);

        let help = self.help_diagnostic_to_find_item(item);

        let mut diagnostic = Diagnostic::error(msg, vec![code_window]);
        diagnostic.chain(help);
        self.diagnostics.push(diagnostic);
    }

    fn unresolved_name_err(
        &mut self,
        at: FileKey,
        at_span: Span,
        id: IdKey,
        fmt_msg_and_label: impl Fn(&String) -> (String, String),
    ) -> Diagnostic<'a> {
        let file_info = &self.files_infos[at];
        let name = &self.id_pool[id];
        let (msg, label) = fmt_msg_and_label(name);

        let mut code_window = CodeWindow::new(file_info, at_span.start);

        code_window.mark_error(at_span, vec![label]);

        let diagnostic = Diagnostic::error(msg, vec![code_window]);

        diagnostic
    }

    fn add_unresolved_name_err_with_possible_paths(
        &mut self,
        at: FileKey,
        at_span: Span,
        id: IdKey,
        fmt_msg_and_label: impl Fn(&String) -> (String, String),
    ) {
        let mut diagnostic = self.unresolved_name_err(at, at_span, id, fmt_msg_and_label);

        let mut possible_paths = vec![];

        for (pkg_key, pkg_to_items) in self.ast.state.pkgs_to_items.iter_enumerated() {
            if let Some(found_item) = pkg_to_items.get(&id) {
                if matches!(found_item, Item::Pkg) {
                    continue;
                }
                let found_item = *found_item;
                let item_info = self.get_item_info(found_item);
                let item_file = &self.files_infos[item_info.file_key];
                let item_span_cursor = item_info.id_span.start;
                let item_kind_str = item_kind_to_str(found_item);

                let pkg_name = self.fmt_pkg_name(pkg_key);
                let name = &self.id_pool[id];
                let item_path = format!(
                    "{}:{}:{}",
                    &item_file.path,
                    item_span_cursor.line + 1,
                    item_span_cursor.col + 1
                );
                let path = format!(
                    "\t- {} {}::{} في {}",
                    item_kind_str, pkg_name, name, item_path
                );

                possible_paths.push(path);
            }
        }

        if !possible_paths.is_empty() {
            let mut help = Diagnostic::help(
                format!("تم العثور على عناصر مشابهة بنفس الاسم في المسارات التالية:"),
                vec![],
            );

            for t in possible_paths {
                help.chain_free_text(t);
            }

            diagnostic.chain(help);
        }

        self.diagnostics.push(diagnostic);
    }

    fn add_unresolved_name_pkg_path_err(
        &mut self,
        PkgPath {
            pkg_key: _,
            file_key,
            mut ids,
            mut spans,
        }: PkgPath,
    ) {
        while let Some(first_invalid_seg_id) = ids.pop() {
            let first_invalid_seg_span = spans.pop().unwrap();

            if self.pkgs.contains_key(&ids) {
                self.add_unresolved_name_err_with_possible_paths(
                    file_key,
                    first_invalid_seg_span,
                    first_invalid_seg_id,
                    |name| {
                        (
                            format!("لم يتم العثور على الاسم `{name}` في المسار"),
                            format!("هذا الاسم غير موجود داخل المسار المحدد"),
                        )
                    },
                );
                return;
            }
        }
    }

    fn add_encapsulation_err(
        &mut self,
        at: FileKey,
        at_span: Span,
        resolved_item: Item,
        name: IdKey,
    ) {
        let file_info = &self.files_infos[at];
        let name = &self.id_pool[name];
        let msg = match resolved_item {
            Item::UnitStruct { .. } | Item::TupleStruct { .. } | Item::FieldsStruct { .. } => {
                format!(
                    "لا يمكن الوصول إلى الهيكل `{}` لأنه خاص بالحزمة التابع لها",
                    name
                )
            }
            Item::Const { .. } => {
                format!(
                    "لا يمكن الوصول إلى الثابت `{}` لأنه خاص بالحزمة التابع لها",
                    name
                )
            }
            Item::Static { .. } => {
                format!(
                    "لا يمكن الوصول إلى المشترك `{}` لأنه خاص بالحزمة التابع لها",
                    name
                )
            }
            Item::Fn { .. } => format!(
                "لا يمكن الوصول إلى الدالة `{}` لأنها خاصة بالحزمة التابعة لها",
                name
            ),
            Item::Pkg | Item::LocalVar => {
                unreachable!()
            }
        };

        let mut code_window = CodeWindow::new(file_info, at_span.start);
        code_window.mark_error(at_span, vec![]);
        let help = self.help_diagnostic_to_find_item(resolved_item);

        let mut diagnostic = Diagnostic::error(msg, vec![code_window]);
        diagnostic.chain(help);
        self.diagnostics.push(diagnostic);
    }

    fn add_pkgs_cannot_be_imported_err(&mut self, at: FileKey, at_span: Span) {
        let file_info = &self.files_infos[at];
        let msg = format!("الحزم لا يمكن إستيرادها");
        let mut code_window = CodeWindow::new(file_info, at_span.start);
        code_window.mark_error(
            at_span,
            vec!["يجب أن يؤول المسار إلى عنصر (دالة أو هيكل) وليس حزمة".to_string()],
        );
        let diagnostic = Diagnostic::error(msg, vec![code_window]);
        self.diagnostics.push(diagnostic);
    }

    fn help_diagnostic_to_find_item(&self, item: Item) -> Diagnostic<'a> {
        let item_kind_str = item_kind_to_str(item);
        let help_msg = format!("تم العثور على ال{} هنا", item_kind_str);
        let resolved_item_info = self.get_item_info(item);
        let file_of_resolved_item = &self.files_infos[resolved_item_info.file_key];
        let mut help_code_window =
            CodeWindow::new(file_of_resolved_item, resolved_item_info.id_span.start);
        help_code_window.mark_help(resolved_item_info.id_span, vec![]);
        Diagnostic::help(help_msg, vec![help_code_window])
    }

    fn get_item_info(&self, item: Item) -> ItemInfo {
        match item {
            nazmc_ast::Item::UnitStruct { key, .. } => self.ast.unit_structs[key].info,
            nazmc_ast::Item::TupleStruct { key, .. } => self.ast.tuple_structs[key].info,
            nazmc_ast::Item::FieldsStruct { key, .. } => self.ast.fields_structs[key].info,
            nazmc_ast::Item::Const { key, .. } => self.ast.consts[key].info,
            nazmc_ast::Item::Static { key, .. } => self.ast.statics[key].info,
            nazmc_ast::Item::Fn { key, .. } => self.ast.fns[key].info,
            nazmc_ast::Item::Pkg | nazmc_ast::Item::LocalVar => {
                unreachable!()
            }
        }
    }

    fn fmt_pkg_name(&self, pkg_key: PkgKey) -> String {
        self.pkgs_names[pkg_key]
            .iter()
            .map(|id| self.id_pool[*id].as_str())
            .collect::<Vec<_>>()
            .join("::")
    }
}

#[inline]
fn item_kind_to_str(item: Item) -> &'static str {
    match item {
        Item::Pkg => "حزمة",
        Item::UnitStruct { .. } | Item::TupleStruct { .. } | Item::FieldsStruct { .. } => "هيكل",
        Item::Const { .. } => "ثابت",
        Item::Static { .. } => "مشترك",
        Item::Fn { .. } => "دالة",
        Item::LocalVar => unreachable!(),
    }
}

fn explicit_item_kind_to_str(item: Item) -> &'static str {
    match item {
        Item::Pkg => "حزمة",
        Item::UnitStruct { .. } => "هيكل وحدة",
        Item::TupleStruct { .. } => "هيكل تراتيب",
        Item::FieldsStruct { .. } => "هيكل حقول",
        Item::Const { .. } => "ثابت",
        Item::Static { .. } => "مشترك",
        Item::Fn { .. } => "دالة",
        Item::LocalVar => unreachable!(),
    }
}
