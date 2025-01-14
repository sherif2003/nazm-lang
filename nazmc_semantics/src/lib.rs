mod consts;
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
use typed_ast::{Type, TypeKey, TypedAST};

#[derive(Default)]
pub struct SemanticsStack {
    pub consts: HashMap<ConstKey, ()>,
    pub tuple_structs: HashMap<TupleStructKey, ()>,
    pub fields_structs: HashMap<FieldsStructKey, ()>,
}

pub struct SemanticsAnalyzer<'a> {
    files_infos: &'a TiSlice<FileKey, FileInfo>,
    id_pool: &'a TiSlice<IdKey, String>,
    pkgs_names: &'a TiSlice<PkgKey, &'a ThinVec<IdKey>>,
    ast: AST<Resolved>,
    typed_ast: TypedAST,
    types_pool: DataPoolBuilder<TypeKey, Type>,
    semantics_stack: SemanticsStack,
    diagnostics: Vec<Diagnostic<'a>>,
}

impl<'a> SemanticsAnalyzer<'a> {
    pub fn new(
        files_infos: &'a TiSlice<FileKey, FileInfo>,
        id_pool: &'a TiSlice<IdKey, String>,
        pkgs_names: &'a TiSlice<PkgKey, &'a ThinVec<IdKey>>,
        ast: nazmc_ast::AST<Resolved>,
    ) -> Self {
        Self {
            files_infos,
            id_pool,
            pkgs_names,
            ast,
            typed_ast: Default::default(),
            types_pool: Default::default(),
            semantics_stack: Default::default(),
            diagnostics: vec![],
        }
    }

    pub fn analyze(self) {
        // TODO
    }
}
