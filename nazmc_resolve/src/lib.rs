use std::{collections::HashMap, process::exit};

use nazmc_ast::{
    ASTId, ASTPaths, FileKey, Item, ItemInfo, ItemPath, PkgKey, PkgPath, UnitStructKey,
    UnitStructPathKey, Unresolved, AST,
};
use nazmc_data_pool::{
    typed_index_collections::{ti_vec, TiSlice, TiVec},
    IdKey,
};
use nazmc_diagnostics::{
    eprint_diagnostics, file_info::FileInfo, span::Span, CodeWindow, Diagnostic,
};
use thin_vec::ThinVec;

pub fn resolve(ast: nazmc_ast::AST<nazmc_ast::Unresolved>) -> nazmc_ast::AST<nazmc_ast::Resolved> {
    todo!()
}

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

    fn resolve_non_pkg_item_path(&mut self, at: FileKey, item_path: ItemPath) -> Option<Item> {
        let item_id = item_path.item.id;

        let at_span = item_path
            .pkg_path
            .spans
            .first()
            .unwrap_or(&item_path.item.span)
            .merged_with(&item_path.item.span);

        let resolved_item = self.resolve_item_path(item_path)?;

        if resolved_item.kind() == Item::PKG {
            self.add_pkgs_cannot_be_imported_err(at, at_span);
            None
        } else if resolved_item.visibility() == Item::DEFAULT {
            self.add_encapsulation_err(at, at_span, resolved_item, item_id);
            None
        } else {
            Some(resolved_item)
        }
    }

    fn resolve_item_path(
        &mut self,
        ItemPath {
            pkg_path,
            item: item_ast_id,
        }: ItemPath,
    ) -> Option<Item> {
        let file_key = pkg_path.file_key;

        let item_pkg_key = self.resolve_pkg_path(pkg_path)?;

        if let Some(item) = self.ast.state.pkgs_to_items[item_pkg_key].get(&item_ast_id.id) {
            Some(*item)
        } else {
            self.add_unresolved_name_err(file_key, item_ast_id.span, item_ast_id.id);
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

    fn check_resolved_item_kind(
        &mut self,
        at: FileKey,
        at_span: Span,
        expected_kind: u64,
        item: Item,
    ) {
        let item_kind = item.kind();

        if item_kind == expected_kind {
            return;
        }

        let expected_kind_str = item_kind_to_str(expected_kind);
        let found_kind_str = item_kind_to_str(item_kind);

        let msg = format!(
            "يُتوقع {} ولكن تم العثور على {}",
            expected_kind_str, found_kind_str
        );

        let file_info = &self.files_infos[at];
        let mut code_window = CodeWindow::new(file_info, at_span.start);
        code_window.mark_error(at_span, vec![]);

        let help = self.help_diagnostic_to_find_item(item);

        let mut diagnostic = Diagnostic::error(msg, vec![code_window]);
        diagnostic.chain(help);
        self.diagnostics.push(diagnostic);
    }

    fn add_unresolved_name_err(&mut self, at: FileKey, at_span: Span, id: IdKey) {
        let file_info = &self.files_infos[at];
        let name = &self.id_pool[id];
        let msg = format!("لم يتم العثور على الاسم `{}` في المسار", name);

        let mut code_window = CodeWindow::new(file_info, at_span.start);

        code_window.mark_error(
            at_span,
            vec!["هذا الاسم غير موجود داخل المسار المحدد".to_string()],
        );

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
        // FIXME: index is 0 and len is 0
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

// use nazmc_data_pool::{Built, DataPool, PoolIdx};
// use nazmc_diagnostics::{eprint_diagnostics, span::Span, CodeWindow, Diagnostic};
// use std::{collections::HashMap, process::exit};
// use thin_vec::ThinVec;

// #[derive(Clone)]
// pub struct ParsedFile {
//     pub path: String,
//     pub lines: Vec<String>,
//     pub ast: nazmc_ast::File,
// }

// #[derive(Default)]
// pub struct ASTItemsCounter {
//     pub unit_structs: usize,
//     pub tuple_structs: usize,
//     pub fields_structs: usize,
//     pub fns: usize,
// }

// #[derive(Default, Clone, Copy)]
// pub struct FileItemKindAndIdx(pub u64);

// impl FileItemKindAndIdx {
//     const KIND_BITS: u64 = 5;
//     const KIND_SHIFT: u64 = 64 - Self::KIND_BITS;
//     const KIND_MASK: u64 = 0b11 << Self::KIND_SHIFT;
//     const INDEX_MASK: u64 = !Self::KIND_MASK;

//     // Possible kinds
//     pub const UNIT_STRUCT: u64 = 0 << Self::KIND_SHIFT;
//     pub const TUPLE_STRUCT: u64 = 1 << Self::KIND_SHIFT;
//     pub const FIELDS_STRUCT: u64 = 2 << Self::KIND_SHIFT;
//     pub const FN: u64 = 3 << Self::KIND_SHIFT;
//     pub const CONST: u64 = 4 << Self::KIND_SHIFT;
//     pub const STATIC: u64 = 5 << Self::KIND_SHIFT;

//     // Create a new encoded value for a given kind and index
//     pub fn new(kind: u64, index: usize) -> Self {
//         Self(kind | index as u64)
//     }

//     // Decode the kind of the expression
//     pub fn kind(self) -> u64 {
//         self.0 & Self::KIND_MASK
//     }

//     // Decode the index of the expression
//     pub fn index(self) -> usize {
//         (self.0 & Self::INDEX_MASK) as usize
//     }
// }

// #[derive(Clone, Copy)]
// pub struct ItemInFile {
//     /// The kind and index in the NIR
//     pub kind_and_idx: FileItemKindAndIdx,
//     /// The file index where the item is defined
//     pub file_idx: usize,
//     /// The item index in the list of file items
//     pub item_idx: usize,
// }

// #[derive(Clone, Copy)]
// pub struct ResolvedImport {
//     /// The pkg idx of the resolved item
//     pub pkg_idx: usize,
//     /// The resolved item
//     pub item: ItemInFile,
//     /// The span of the resolved item in its file
//     pub span: Span,
// }

// pub struct NameResolver<'a> {
//     /// The pool used to preserve ids string values
//     id_pool: &'a DataPool<Built>,
//     /// A map from pkgs ids segments to the pkgs indexes
//     packages: &'a HashMap<ThinVec<PoolIdx>, usize>,
//     /// A map from the pkgs indexes to their segments
//     packages_names: &'a [ThinVec<PoolIdx>],
//     /// A map from the pkgs indexes to the inner files indexes
//     packages_to_parsed_files: &'a [Vec<usize>],
//     /// The parsed filese array
//     parsed_files: &'a [ParsedFile],
//     /// The diagnostics which will be filled in different phases
//     diagnostics: Vec<Diagnostic<'a>>,
//     nrt: NameResolutionTree,
// }

// pub struct NameResolutionTree {
//     /// Each pkg has a map of ids to their occurrence in the package (the file index and the item index in that file)
//     pub packages_to_items: Vec<HashMap<PoolIdx, ItemInFile>>,
//     /// Each pkg will have HashMap<usize, Vec<ResolvedImport>>,
//     /// which is the map of file idx to its resolved imports
//     pub resolved_imports: Vec<HashMap<usize, HashMap<PoolIdx, ResolvedImport>>>,
//     /// Each pkg will have HashMap<usize, Vec<usize>>,
//     /// which is the map of file idx to its resolved pkgs indexes
//     pub resolved_star_imports: Vec<HashMap<usize, Vec<usize>>>,
//     /// The counter for items (used to construct NIR)
//     pub ast_counter: ASTItemsCounter,
// }

// impl<'a> NameResolver<'a> {
//     pub fn new(
//         id_pool: &'a DataPool<Built>,
//         packages: &'a HashMap<ThinVec<PoolIdx>, usize>,
//         packages_names: &'a [ThinVec<PoolIdx>],
//         packages_to_parsed_files: &'a [Vec<usize>],
//         parsed_files: &'a [ParsedFile],
//     ) -> Self {
//         Self {
//             id_pool,
//             packages,
//             packages_names,
//             packages_to_parsed_files,
//             parsed_files,
//             diagnostics: vec![],
//             nrt: NameResolutionTree {
//                 packages_to_items: vec![HashMap::new(); packages.len()],
//                 resolved_imports: vec![HashMap::new(); packages.len()],
//                 resolved_star_imports: vec![HashMap::new(); packages.len()],
//                 ast_counter: ASTItemsCounter::default(),
//             },
//         }
//     }

//     pub fn resolve(mut self) -> NameResolutionTree {
//         self.check_pkg_items_conflicts();

//         if !self.diagnostics.is_empty() {
//             eprint_diagnostics(self.diagnostics);
//             exit(1)
//         }

//         self.resolve_imports();

//         if !self.diagnostics.is_empty() {
//             eprint_diagnostics(self.diagnostics);
//             exit(1)
//         }

//         self.nrt
//     }

//     fn check_pkg_items_conflicts(&mut self) {
//         let mut conflicts: HashMap<(usize, PoolIdx), HashMap<usize, Vec<Span>>> = HashMap::new();
//         //                          ^^^^^  ^^^^^^^           ^^^^^  ^^^^^^^^^
//         //                          |      |                 |      |
//         //                          |      |                 |      span occurrences in the file
//         //                          |      |                 parsed file idx
//         //                          |      conflicting name
//         //                          package idx

//         for (pkg_idx, parsed_files_in_package) in self.packages_to_parsed_files.iter().enumerate() {
//             for parsed_file_idx in parsed_files_in_package {
//                 self.check_conflicts_in_file(*parsed_file_idx, pkg_idx, &mut conflicts);
//             }
//         }

//         for ((_pkg_idx, conflicting_name), name_conflicts_in_single_package) in conflicts {
//             let name = &self.id_pool[conflicting_name];
//             let msg = format!("يوجد أكثر من عنصر بنفس الاسم `{}` في نفس الحزمة", name);
//             let mut diagnostic = Diagnostic::error(msg, vec![]);
//             let mut occurrences = 1;

//             for (file_idx, spans) in name_conflicts_in_single_package {
//                 let parsed_file = &self.parsed_files[file_idx];
//                 let code_window = occurrences_code_window(parsed_file, &mut occurrences, spans);
//                 diagnostic.push_code_window(code_window);
//             }

//             self.diagnostics.push(diagnostic);
//         }
//     }

//     #[inline]
//     fn check_conflicts_in_file(
//         &mut self,
//         parsed_file_idx: usize,
//         pkg_idx: usize,
//         conflicts: &mut HashMap<(usize, PoolIdx), HashMap<usize, Vec<Span>>>,
//     ) {
//         let items_in_package = &mut self.nrt.packages_to_items[pkg_idx];
//         let parsed_file = &self.parsed_files[parsed_file_idx];

//         for (item_idx, item) in parsed_file.ast.items.iter().enumerate() {
//             match items_in_package.get(&item.name.id) {
//                 Some(first_occurrence) => {
//                     conflicts
//                         .entry((pkg_idx, item.name.id))
//                         .or_insert_with(|| {
//                             let first_occurrence_span =
//                                 self.parsed_files[first_occurrence.file_idx].ast.items
//                                     [first_occurrence.item_idx]
//                                     .name
//                                     .span;

//                             let mut h = HashMap::new();
//                             h.insert(first_occurrence.file_idx, vec![first_occurrence_span]);
//                             h
//                         })
//                         .entry(parsed_file_idx)
//                         .or_default()
//                         .push(item.name.span);
//                 }
//                 None => {
//                     let (kind, index) = match item.kind {
//                         nazmc_ast::ItemKind::UnitStruct => (
//                             FileItemKindAndIdx::UNIT_STRUCT,
//                             &mut self.nrt.ast_counter.unit_structs,
//                         ),
//                         nazmc_ast::ItemKind::TupleStruct(_) => (
//                             FileItemKindAndIdx::TUPLE_STRUCT,
//                             &mut self.nrt.ast_counter.tuple_structs,
//                         ),
//                         nazmc_ast::ItemKind::FieldsStruct(_) => (
//                             FileItemKindAndIdx::FIELDS_STRUCT,
//                             &mut self.nrt.ast_counter.fields_structs,
//                         ),
//                         nazmc_ast::ItemKind::Fn(_) => {
//                             (FileItemKindAndIdx::FN, &mut self.nrt.ast_counter.fns)
//                         }
//                     };

//                     let kind_and_idx = FileItemKindAndIdx::new(kind, *index);

//                     *index += 1;

//                     items_in_package.insert(
//                         item.name.id,
//                         ItemInFile {
//                             kind_and_idx,
//                             file_idx: parsed_file_idx,
//                             item_idx,
//                         },
//                     );
//                 }
//             }
//         }
//     }

//     fn resolve_imports(&mut self) {
//         let mut conflicts: HashMap<usize, HashMap<PoolIdx, Vec<Span>>> = HashMap::new();
//         //                         ^^^^^          ^^^^^^^  ^^^^^^^^^
//         //                         |              |        |
//         //                         |              |        span occurrences in the file
//         //                         |              conflicting name
//         //                         file idx

//         for (pkg_idx, parsed_files_in_package) in self.packages_to_parsed_files.iter().enumerate() {
//             for parsed_file_idx in parsed_files_in_package.iter() {
//                 self.resolve_file_imports(pkg_idx, *parsed_file_idx, &mut conflicts);
//                 self.resolve_file_star_imports(pkg_idx, *parsed_file_idx);
//             }
//         }

//         for (pkg_idx, files_in_pkg) in self.nrt.resolved_imports.iter().enumerate() {
//             for (parsed_file_idx, resolved_imports) in files_in_pkg.iter() {
//                 for (alias, resolved_import) in resolved_imports {
//                     let Some(item_with_same_id) = self.nrt.packages_to_items[pkg_idx].get(&alias)
//                     else {
//                         continue;
//                     };

//                     let parsed_file = &self.parsed_files[*parsed_file_idx];

//                     conflicts
//                         .entry(*parsed_file_idx)
//                         .or_default()
//                         .entry(*alias)
//                         .or_insert_with(|| {
//                             let first_occurrence_span =
//                                 parsed_file.ast.items[item_with_same_id.item_idx].name.span;

//                             vec![first_occurrence_span]
//                         })
//                         .push(resolved_import.span);
//                 }
//             }
//         }

//         for (file_idx, name_conflicts_in_single_file) in conflicts {
//             let parsed_file = &self.parsed_files[file_idx];

//             for (conflicting_name, spans) in name_conflicts_in_single_file {
//                 let name = &self.id_pool[conflicting_name];
//                 let msg = format!("يوجد أكثر من عنصر بنفس الاسم `{}` في نفس الملف", name);
//                 let mut diagnostic = Diagnostic::error(msg, vec![]);
//                 let mut occurrences = 1;
//                 let code_window = occurrences_code_window(parsed_file, &mut occurrences, spans);
//                 diagnostic.push_code_window(code_window);
//                 self.diagnostics.push(diagnostic);
//             }
//         }
//     }

//     #[inline]
//     fn resolve_file_star_imports(&mut self, pkg_idx: usize, parsed_file_idx: usize) {
//         let parsed_file = &self.parsed_files[parsed_file_idx];
//         for import in &parsed_file.ast.star_imports {
//             let Some(resolved_package_idx) = self.packages.get(&import.ids) else {
//                 self.add_pkg_path_err(&parsed_file, import.ids.clone(), import.spans.clone());
//                 continue;
//             };

//             self.nrt.resolved_star_imports[pkg_idx]
//                 .entry(parsed_file_idx)
//                 .or_default()
//                 .push(*resolved_package_idx);
//         }
//     }

//     #[inline]
//     fn resolve_file_imports(
//         &mut self,
//         pkg_idx: usize,
//         parsed_file_idx: usize,
//         conflicts: &mut HashMap<usize, HashMap<PoolIdx, Vec<Span>>>,
//     ) {
//         let parsed_file = &self.parsed_files[parsed_file_idx];
//         for (import, item_alias) in &parsed_file.ast.imports {
//             let Some(resolved_package_idx) = self.packages.get(&import.pkg_path.ids) else {
//                 self.add_pkg_path_err(
//                     &parsed_file,
//                     import.pkg_path.ids.clone(),
//                     import.pkg_path.spans.clone(),
//                 );
//                 continue;
//             };

//             let Some(resolved_item) =
//                 self.nrt.packages_to_items[*resolved_package_idx].get(&import.item.id)
//             else {
//                 self.add_unresolved_import_err(&parsed_file, import.item.id, import.item.span);
//                 continue;
//             };

//             let item_resolved_file = &self.parsed_files[resolved_item.file_idx];

//             let resolved_item_ast = &item_resolved_file.ast.items[resolved_item.item_idx];

//             if pkg_idx != *resolved_package_idx
//                 && matches!(resolved_item_ast.vis, nazmc_ast::VisModifier::Default)
//             {
//                 self.add_encapsulation_err(
//                     parsed_file,
//                     item_resolved_file,
//                     import,
//                     resolved_item_ast,
//                 );
//             } else {
//                 let imports = self.nrt.resolved_imports[pkg_idx]
//                     .entry(parsed_file_idx)
//                     .or_default();

//                 if let Some(import_with_same_id) = imports.get(&item_alias.id) {
//                     conflicts
//                         .entry(parsed_file_idx)
//                         .or_default()
//                         .entry(item_alias.id)
//                         .or_insert_with(|| vec![import_with_same_id.span])
//                         .push(item_alias.span);
//                 } else {
//                     imports.insert(
//                         item_alias.id,
//                         ResolvedImport {
//                             pkg_idx: *resolved_package_idx,
//                             item: *resolved_item,
//                             span: item_alias.span,
//                         },
//                     );
//                 }
//             }
//         }
//     }

//     fn add_encapsulation_err(
//         &mut self,
//         parsed_file: &'a ParsedFile,
//         item_resolved_file: &'a ParsedFile,
//         import: &nazmc_ast::ItemPath,
//         resolved_item_ast: &nazmc_ast::Item,
//     ) {
//         let name = &self.id_pool[import.item.id];
//         let item_kind_str = item_kind_to_str(&resolved_item_ast.kind);
//         let msg = match resolved_item_ast.kind {
//             nazmc_ast::ItemKind::UnitStruct
//             | nazmc_ast::ItemKind::TupleStruct(_)
//             | nazmc_ast::ItemKind::FieldsStruct(_) => {
//                 format!(
//                     "لا يمكن الوصول إلى هيكل `{}` لأنه خاص بالحزمة التابع لها",
//                     name
//                 )
//             }
//             nazmc_ast::ItemKind::Fn(_) => format!(
//                 "لا يمكن الوصول إلى دالة `{}` لأنها خاصة بالحزمة التابعة لها",
//                 name
//             ),
//         };

//         let mut code_window = CodeWindow::new(
//             &parsed_file.path,
//             &parsed_file.lines,
//             import.item.span.start,
//         );
//         code_window.mark_error(import.item.span, vec![]);
//         let mut diagnostic = Diagnostic::error(msg, vec![code_window]);

//         let help_msg = format!("تم العثور على {} هنا", item_kind_str);
//         let mut help_code_window = CodeWindow::new(
//             &item_resolved_file.path,
//             &item_resolved_file.lines,
//             resolved_item_ast.name.span.start,
//         );
//         help_code_window.mark_note(resolved_item_ast.name.span, vec![]);
//         let help = Diagnostic::note(help_msg, vec![help_code_window]);
//         diagnostic.chain(help);kind

//         self.diagnostics.push(diagnostic);
//     }

//     fn add_unresolved_import_err(&mut self, file: &'a ParsedFile, id: PoolIdx, span: Span) {
//         let name = &self.id_pool[id];
//         let msg = format!("لم يتم العثور على الاسم `{}` في المسار", name);

//         let mut code_window = CodeWindow::new(&file.path, &file.lines, span.start);

//         code_window.mark_error(
//             span,
//             vec!["هذا الاسم غير موجود داخل المسار المحدد".to_string()],
//         );

//         let mut diagnostic = Diagnostic::error(msg, vec![code_window]);

//         let mut possible_paths = vec![];

//         for (pkg_idx, pkg_to_items) in self.nrt.packages_to_items.iter().enumerate() {
//             if let Some(found_item) = pkg_to_items.get(&id) {
//                 let item_file = &self.parsed_files[found_item.file_idx];
//                 let item_ast = &item_file.ast.items[found_item.item_idx];
//                 let item_span_cursor = item_ast.name.span.start;
//                 let item_kind_str = item_kind_to_str(&item_ast.kind);
//                 let pkg_name = self.fmt_pkg_name(pkg_idx);
//                 let name = &self.id_pool[id];
//                 let item_path = format!(
//                     "{}:{}:{}",
//                     &item_file.path,
//                     item_span_cursor.line + 1,
//                     item_span_cursor.col + 1
//                 );
//                 let path = format!(
//                     "\t- {} {}::{} في {}",
//                     item_kind_str, pkg_name, name, item_path
//                 );

//                 possible_paths.push(path);
//             }
//         }

//         if !possible_paths.is_empty() {
//             let mut help = Diagnostic::help(
//                 format!("تم العثور على عناصر مشابهة بنفس الاسم في المسارات التالية:"),
//                 vec![],
//             );

//             for t in possible_paths {
//                 help.chain_free_text(t);
//             }

//             diagnostic.chain(help);
//         }

//         self.diagnostics.push(diagnostic);
//     }

//     fn add_pkg_path_err(
//         &mut self,
//         file: &'a ParsedFile,
//         mut pkg_path: ThinVec<PoolIdx>,
//         mut pkg_path_spans: ThinVec<Span>,
//     ) {
//         while let Some(first_invalid_seg) = pkg_path.pop() {
//             let first_invalid_seg_span = pkg_path_spans.pop().unwrap();

//             if self.packages.contains_key(&pkg_path) {
//                 self.add_unresolved_import_err(&file, first_invalid_seg, first_invalid_seg_span);
//             }
//         }
//     }

//     fn fmt_pkg_name(&self, pkg_idx: usize) -> String {
//         self.packages_names[pkg_idx]
//             .iter()
//             .map(|id| &self.id_pool[*id])
//             .collect::<Vec<_>>()
//             .join("::")
//     }
// }

// fn occurrences_code_window<'a>(
//     parsed_file: &'a ParsedFile,
//     occurrences: &mut usize,
//     mut spans: Vec<Span>,
// ) -> CodeWindow<'a> {
//     let mut code_window = CodeWindow::new(&parsed_file.path, &parsed_file.lines, spans[0].start);

//     nazmc_diagnostics::span::sort_spans(&mut spans);

//     for span in spans {
//         let occurrence_str = match *occurrences {
//             1 => "هنا تم العثور على أول عنصر بهذا الاسم".to_string(),
//             2 => "هنا تم العثور على نفس الاسم للمرة الثانية".to_string(),
//             3 => "هنا تم العثور على نفس الاسم للمرة الثالثة".to_string(),
//             4 => "هنا تم العثور على نفس الاسم للمرة الرابعة".to_string(),
//             5 => "هنا تم العثور على نفس الاسم للمرة الخامسة".to_string(),
//             6 => "هنا تم العثور على نفس الاسم للمرة السادسة".to_string(),
//             7 => "هنا تم العثور على نفس الاسم للمرة السابعة".to_string(),
//             8 => "هنا تم العثور على نفس الاسم للمرة الثامنة".to_string(),
//             9 => "هنا تم العثور على نفس الاسم للمرة التاسعة".to_string(),
//             10 => "هنا تم العثور على نفس الاسم للمرة العاشرة".to_string(),
//             o => format!("هنا تم العثور على نفس الاسم للمرة {}", o),
//         };

//         if *occurrences == 1 {
//             code_window.mark_error(span, vec![occurrence_str]);
//         } else {
//             code_window.mark_secondary(span, vec![occurrence_str]);
//         }

//         *occurrences += 1;
//     }

//     code_window
// }
