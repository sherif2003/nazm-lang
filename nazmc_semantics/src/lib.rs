use nazmc_data_pool::{
    typed_index_collections::{ti_vec, TiSlice, TiVec},
    IdKey,
};

use nazmc_diagnostics::{
    eprint_diagnostics, file_info::FileInfo, span::Span, CodeWindow, Diagnostic,
};

use std::{collections::HashMap, process::exit};
use thin_vec::ThinVec;

pub struct SemanticsAnalyzer<'a> {
    files_infos: &'a TiSlice<nazmc_ast::FileKey, FileInfo>,
    id_pool: &'a TiSlice<IdKey, String>,
    pkgs_names: &'a TiSlice<nazmc_ast::PkgKey, &'a ThinVec<IdKey>>,
    ast: nazmc_ast::AST<nazmc_ast::Resolved>,
    types: nazmc_ast::Types,
    diagnostics: Vec<Diagnostic<'a>>,
}

impl<'a> SemanticsAnalyzer<'a> {
    pub fn new(
        files_infos: &'a TiSlice<nazmc_ast::FileKey, FileInfo>,
        id_pool: &'a TiSlice<IdKey, String>,
        pkgs_names: &'a TiSlice<nazmc_ast::PkgKey, &'a ThinVec<IdKey>>,
        ast: nazmc_ast::AST<nazmc_ast::Resolved>,
    ) -> Self {
        let types = nazmc_ast::Types {
            all: TiVec::with_capacity(ast.types_exprs.all.len()),
            tuples: TiVec::with_capacity(ast.types_exprs.tuples.len()),
            arrays: TiVec::with_capacity(ast.types_exprs.arrays.len()),
            lambdas: TiVec::with_capacity(ast.types_exprs.lambdas.len()),
        };
        Self {
            files_infos,
            id_pool,
            pkgs_names,
            ast,
            types,
            diagnostics: vec![],
        }
    }
}

// Receive the analyzer this way to avoid the intend
pub fn analyze(analyzer: SemanticsAnalyzer) {
    // TODO
}
