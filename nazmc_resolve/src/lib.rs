use nazmc_ast::{
    ASTId, FieldsStructPathKey, FileKey, Item, ItemInfo, ItemPath, PathNoPkgKey, PathWithPkgKey,
    PkgKey, PkgPath, ScopeKey, TupleStructPathKey, TypePathKey, UnitStructPathKey,
};
use nazmc_data_pool::{
    typed_index_collections::{ti_vec, TiSlice, TiVec},
    IdKey,
};
use nazmc_diagnostics::{
    eprint_diagnostics, file_info::FileInfo, span::Span, CodeWindow, Diagnostic,
};
use std::{collections::HashMap, process::exit};
use thin_vec::ThinVec;

// TODO: improve err msgs readability

pub struct NameResolver<'a> {
    files_infos: &'a TiSlice<FileKey, FileInfo>,
    id_pool: &'a TiSlice<IdKey, String>,
    pkgs: &'a HashMap<ThinVec<IdKey>, usize>,
    pkgs_names: &'a TiSlice<PkgKey, &'a ThinVec<IdKey>>,
    ast: nazmc_ast::AST<nazmc_ast::Unresolved>,
    diagnostics: Vec<Diagnostic<'a>>,
    // unresolved_pkgs_paths: Vec<(FileKey, ASTId)>,
    // unresolved_items_paths: Vec<(FileKey, ASTId)>,
    // wrong_items_found: Vec<WrongItemFoundErr>,
    // pkgs_cannot_be_imported: Vec<(FileKey, Span)>,
    // unaccessable_item_errs: Vec<UnaccessableItemErr>,
}

struct UnaccessableItemErr {
    imported_at: FileKey,
    imported_span: Span,
    found_item_info: ItemInfo,
}

struct WrongItemFoundErr {
    file_key: FileKey,
    span: Span,
    item_kind: u64,
    expected_kind: u64,
}

struct ResolvedImport {
    alias_span: Span,
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
                            .resolve_non_pkg_item_path(file_key, import.item_path)
                            .unwrap_or_default();
                        (
                            import.alias.id,
                            ResolvedImport {
                                alias_span: import.alias.span,
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
            .map(|pkgs_paths| {
                pkgs_paths
                    .into_iter()
                    .map(|pkg_path| {
                        let span = pkg_path
                            .spans
                            .first()
                            .unwrap()
                            .merged_with(&pkg_path.spans.last().unwrap());

                        let resolved_pkg_key = self.resolve_pkg_path(pkg_path).unwrap_or_default();

                        ResolvedStarImport {
                            pkg_path_span: span,
                            resolved_pkg_key,
                        }
                    })
                    .collect()
            })
            .collect::<TiVec<FileKey, ThinVec<_>>>();

        let resolved_types_paths = paths
            .types_paths
            .into_iter()
            .map(|item_path| {
                let file_key = item_path.pkg_path.file_key;

                // FIXME: The comapring of items kinds err msg is somehow bad
                self.resolve_item_path_from_local_file(
                    file_key,
                    item_path,
                    &resolved_imports,
                    &resolved_star_imports,
                    |item_kind| {
                        item_kind == Item::UNIT_STRUCT
                            || item_kind == Item::TUPLE_STRUCT
                            || item_kind == Item::FIELDS_STRUCT
                    },
                    "هيكل",
                )
                .unwrap_or_default()
            })
            .collect::<TiVec<TypePathKey, Item>>();

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
                    |item_kind| item_kind == Item::UNIT_STRUCT,
                    explicit_item_kind_to_str(Item::UNIT_STRUCT),
                )
                .unwrap_or_default()
            })
            .collect::<TiVec<UnitStructPathKey, Item>>();

        let resolved_tuple_structs_exprs = paths
            .tuple_structs_paths_exprs
            .into_iter()
            .map(|item_path| {
                println!("ccc");
                let file_key = item_path.pkg_path.file_key;

                let x = self
                    .resolve_item_path_from_local_file(
                        file_key,
                        item_path,
                        &resolved_imports,
                        &resolved_star_imports,
                        |item_kind| item_kind == Item::TUPLE_STRUCT,
                        explicit_item_kind_to_str(Item::TUPLE_STRUCT),
                    )
                    .unwrap_or_default();

                println!("ccc");
                x
            })
            .collect::<TiVec<TupleStructPathKey, Item>>();

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
                    |item_kind| item_kind == Item::FIELDS_STRUCT,
                    explicit_item_kind_to_str(Item::FIELDS_STRUCT),
                )
                .unwrap_or_default()
            })
            .collect::<TiVec<FieldsStructPathKey, Item>>();

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
                    // TODO: Support statics and consts
                    |item_kind| item_kind == Item::FN,
                    explicit_item_kind_to_str(Item::FN),
                )
                .unwrap_or_default()
            })
            .collect::<TiVec<PathWithPkgKey, Item>>();

        // Resolve paths that has no leading pkg path
        let mut resolved_paths = ti_vec![Default::default(); paths.paths_no_pkgs_exprs.len()];

        let mut names_stack = vec![];
        let fns_scopes_len = self.ast.fns_scopes.len();

        for i in 0..fns_scopes_len {
            names_stack.clear();
            let at = self.ast.fns[i].info.file_key;
            self.resolve_paths_in_scope(
                at,
                &mut names_stack,
                i.into(),
                &paths.paths_no_pkgs_exprs,
                &mut resolved_paths,
                &resolved_imports,
                &resolved_star_imports,
            );
        }

        if !self.diagnostics.is_empty() {
            eprint_diagnostics(self.diagnostics);
            exit(1);
        }

        // let resolved_unit_structs_paths_exprs = paths
        //     .unit_structs_paths_exprs
        //     .into_iter()
        //     .map(|item_path| {
        //         let item = self.resolve_item_path(item_path);
        //         self.check_resolved_item_kind(Item::UNIT_STRUCT, item);
        //         item
        //     })
        //     .collect::<TiVec<UnitStructPathKey, Item>>();

        todo!()
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
        let scope = std::mem::take(&mut self.ast.scopes[scope_key]);

        for name in scope.extra_params {
            names_stack.push(name);
        }

        'label: for event in scope.events {
            match event {
                nazmc_ast::ScopeEvent::Let(let_stm_key) => {
                    let let_stm = &self.ast.lets[let_stm_key];
                    nazmc_ast::expand_names_binding_owned(&let_stm.binding.kind, names_stack);
                }
                nazmc_ast::ScopeEvent::Path(path_no_pkg_key) => {
                    let path_expr = &paths[path_no_pkg_key];
                    for name in names_stack.iter().rev() {
                        if *name == path_expr.0.id {
                            resolved_paths[path_no_pkg_key] = Item::new(Item::LOCAL_VAR, 0, 0);
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
                            // TODO: Support statics and consts
                            |item_kind| item_kind == Item::FN,
                            explicit_item_kind_to_str(Item::FN),
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

    fn resolve_non_pkg_item_path(&mut self, at: FileKey, item_path: ItemPath) -> Option<Item> {
        let item_id = item_path.item.id;

        let at_span = item_path
            .pkg_path
            .spans
            .first()
            .unwrap_or(&item_path.item.span)
            .merged_with(&item_path.item.span);

        let at_pkg_key = item_path.pkg_path.pkg_key;

        let (item_pkg_key, resolved_item) = self.resolve_item_path(item_path)?;

        if self.check_resolved_item(
            at,
            at_span,
            at_pkg_key,
            item_pkg_key,
            resolved_item,
            item_id,
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
        predicate_kind: impl Fn(u64) -> bool,
        expected_kind: &str,
    ) -> Option<Item> {
        if item_path.pkg_path.ids.is_empty() {
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
            self.resolve_non_pkg_item_path(at, item_path)
        }
    }

    fn resolve_item_path_with_no_pkg_path(
        &mut self,
        at: FileKey,
        item_ast_id: ASTId,
        item_path_pkg_key: PkgKey,
        resolved_imports: &TiSlice<FileKey, HashMap<IdKey, ResolvedImport>>,
        resolved_star_imports: &TiSlice<FileKey, ThinVec<ResolvedStarImport>>,
        predicate_kind: impl Fn(u64) -> bool,
        expected_kind: &str,
    ) -> Option<Item> {
        if let Some(item) = resolved_imports[at].get(&item_ast_id.id) {
            return Some(item.resolved_item);
        }

        if let Some(item) =
            self.ast.state.pkgs_to_items[nazmc_ast::TOP_PKG_KEY].get(&item_ast_id.id)
        {
            return if predicate_kind(item.kind()) {
                Some(*item)
            } else {
                self.add_unexpected_item_kind(
                    at,
                    item_ast_id.span,
                    expected_kind,
                    explicit_item_kind_to_str(item.kind()),
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
                    .filter(|item| predicate_kind(item.kind()))
                    .map(|item| (star_import, item))
            })
            .collect::<Vec<_>>();

        if resolved_items_with_same_name.len() == 1 {
            let (resolved_star_import, resolved_item) =
                *resolved_items_with_same_name.first().unwrap();

            let resolved_item = *resolved_item;

            return if !predicate_kind(resolved_item.kind()) {
                self.add_unexpected_item_kind(
                    at,
                    item_ast_id.span,
                    expected_kind,
                    explicit_item_kind_to_str(resolved_item.kind()),
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
        } else if resolved_items_with_same_name.is_empty() {
            self.add_unresolved_name_err(
                at,
                item_ast_id.span,
                item_ast_id.id,
                |name| format!("لم يتم العثور على {} بالاسم `{}`", expected_kind, name),
                |_name| format!(""),
            );
            return None;
        }

        let name = &self.id_pool[item_ast_id.id];

        let mut code_window = CodeWindow::new(&self.files_infos[at], item_ast_id.span.start);

        code_window.mark_error(
            item_ast_id.span,
            vec!["يوجد أكثر من عنصر بنفس الاسم تم استيرادهم ضمنياً".to_string()],
        );

        let mut note = Diagnostic::note(
            format!(
                "تم استيراد {} عنصر بنفس الاسم `{}` من المسارات التالية",
                resolved_items_with_same_name.len(),
                name
            ),
            vec![],
        );

        let mut help_code_window = CodeWindow::new(
            &self.files_infos[at],
            resolved_items_with_same_name
                .first()
                .unwrap()
                .0
                .pkg_path_span
                .start,
        );

        for (resolved_star_import, _resolved_item) in resolved_items_with_same_name {
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
        }: ItemPath,
    ) -> Option<(PkgKey, Item)> {
        let empty_path = pkg_path.ids.is_empty();

        let file_key = pkg_path.file_key;

        let item_pkg_key = self.resolve_pkg_path(pkg_path)?;

        if let Some(item) = self.ast.state.pkgs_to_items[item_pkg_key].get(&item_ast_id.id) {
            Some((item_pkg_key, *item))
        } else {
            self.add_unresolved_name_err(
                file_key,
                item_ast_id.span,
                item_ast_id.id,
                |name| {
                    if empty_path {
                        format!("لم يتم العثور على الاسم `{name}`")
                    } else {
                        format!("لم يتم العثور على الاسم `{name}` في المسار")
                    }
                },
                |_name| {
                    if empty_path {
                        format!("")
                    } else {
                        format!("هذا الاسم غير موجود داخل المسار المحدد")
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
        if resolved_item.kind() == Item::PKG {
            self.add_pkgs_cannot_be_imported_err(at, at_span);
            false
        } else if resolved_item.visibility() == Item::DEFAULT && item_pkg_key != at_pkg_key {
            self.add_encapsulation_err(at, at_span, resolved_item, item_id);
            false
        } else {
            true
        }
    }

    // FIXME: Change mechanism of checking the items' kinds
    fn add_unexpected_item_kind(
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

    fn add_unresolved_name_err(
        &mut self,
        at: FileKey,
        at_span: Span,
        id: IdKey,
        fmt_msg: impl Fn(&String) -> String,
        fmt_label: impl Fn(&String) -> String,
    ) {
        let file_info = &self.files_infos[at];
        let name = &self.id_pool[id];

        let mut code_window = CodeWindow::new(file_info, at_span.start);

        code_window.mark_error(at_span, vec![fmt_label(name)]);

        let msg = fmt_msg(name);

        let mut diagnostic = Diagnostic::error(msg, vec![code_window]);

        let mut possible_paths = vec![];

        for (pkg_key, pkg_to_items) in self.ast.state.pkgs_to_items.iter_enumerated() {
            if let Some(found_item) = pkg_to_items.get(&id) {
                if found_item.kind() == Item::PKG {
                    continue;
                }
                let item_info = self.get_item_info(*found_item);
                let item_file = &self.files_infos[item_info.file_key];
                let item_span_cursor = item_info.id_span.start;
                let item_kind_str = item_kind_to_str(found_item.kind());

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
                self.add_unresolved_name_err(
                    file_key,
                    first_invalid_seg_span,
                    first_invalid_seg_id,
                    |name| format!("لم يتم العثور على الاسم `{name}` في المسار"),
                    |_name| format!("هذا الاسم غير موجود داخل المسار المحدد"),
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
        let item_kind = resolved_item.kind();
        let msg = match item_kind {
            Item::UNIT_STRUCT | Item::TUPLE_STRUCT | Item::FIELDS_STRUCT => {
                format!(
                    "لا يمكن الوصول إلى هيكل `{}` لأنه خاص بالحزمة التابع لها",
                    name
                )
            }
            Item::FN => format!(
                "لا يمكن الوصول إلى دالة `{}` لأنها خاصة بالحزمة التابعة لها",
                name
            ),
            _ => {
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
        let item_kind = item.kind();
        let item_kind_str = item_kind_to_str(item_kind);
        let help_msg = format!("تم العثور على ال{} هنا", item_kind_str);
        let resolved_item_info = self.get_item_info(item);
        let file_of_resolved_item = &self.files_infos[resolved_item_info.file_key];
        let mut help_code_window =
            CodeWindow::new(file_of_resolved_item, resolved_item_info.id_span.start);
        help_code_window.mark_help(resolved_item_info.id_span, vec![]);
        Diagnostic::help(help_msg, vec![help_code_window])
    }

    fn get_item_info(&self, item: Item) -> ItemInfo {
        let idx = item.index();
        match item.kind() {
            Item::UNIT_STRUCT => self.ast.unit_structs[idx].info,
            Item::TUPLE_STRUCT => self.ast.tuple_structs[idx].info,
            Item::FIELDS_STRUCT => self.ast.fields_structs[idx].info,
            Item::FN => self.ast.fns[idx].info,
            _ => {
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
fn item_kind_to_str(kind: u64) -> &'static str {
    match kind {
        Item::PKG => "حزمة",
        Item::UNIT_STRUCT | Item::TUPLE_STRUCT | Item::FIELDS_STRUCT => "هيكل",
        Item::FN => "دالة",
        _ => {
            unreachable!()
        }
    }
}

fn explicit_item_kind_to_str(kind: u64) -> &'static str {
    match kind {
        Item::PKG => "حزمة",
        Item::UNIT_STRUCT => "هيكل وحدة",
        Item::TUPLE_STRUCT => "هيكل تراتيب",
        Item::FIELDS_STRUCT => "هيكل حقول",
        Item::FN => "دالة",
        _ => {
            unreachable!()
        }
    }
}
