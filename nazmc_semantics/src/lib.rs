mod consts;
mod exprs;
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
use typed_ast::{
    ArrayType, FnPtrType, FnPtrTypeKey, LambdaType, TupleType, Type, TypeKey, TypedAST,
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
    types_pool: DataPoolBuilder<TypeKey, Type>,
    tuple_types_pool: DataPoolBuilder<TupleTypeKey, TupleType>,
    array_types_pool: DataPoolBuilder<ArrayTypeKey, ArrayType>,
    lambda_types_pool: DataPoolBuilder<LambdaTypeKey, LambdaType>,
    fns_ptrs_types_pool: DataPoolBuilder<FnPtrTypeKey, FnPtrType>,
    semantics_stack: SemanticsStack,
    diagnostics: Vec<Diagnostic<'a>>,
    cycle_stack: Vec<Diagnostic<'a>>,
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
        for type_expr_key in self.ast.types_exprs.all.keys() {
            self.analyze_type_expr(type_expr_key);
        }

        for expr_key in self.ast.exprs.keys() {
            self.analyze_expr(expr_key);
        }

        if !self.diagnostics.is_empty() {
            eprint_diagnostics(self.diagnostics);
            exit(1);
        }

        // TODO
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

    fn is_subtype_of(&self, sub: Type, sup: Type) -> bool {
        match (sub, sup) {
            (Type::Unknown | Type::Never, _)
            | (
                Type::UnspecifiedUnsignedInt,
                Type::UnspecifiedSignedInt
                | Type::I
                | Type::I1
                | Type::I2
                | Type::I4
                | Type::I8
                | Type::UnspecifiedUnsignedInt
                | Type::U
                | Type::U1
                | Type::U2
                | Type::U4
                | Type::U8,
            )
            | (
                Type::UnspecifiedSignedInt,
                Type::UnspecifiedSignedInt | Type::I | Type::I1 | Type::I2 | Type::I4 | Type::I8,
            )
            | (Type::UnspecifiedFloat, Type::UnspecifiedFloat | Type::F4 | Type::F8)
            | (Type::I, Type::I)
            | (Type::I1, Type::I1)
            | (Type::I2, Type::I2)
            | (Type::I4, Type::I4)
            | (Type::I8, Type::I8)
            | (Type::U, Type::U)
            | (Type::U1, Type::U1)
            | (Type::U2, Type::U2)
            | (Type::U4, Type::U4)
            | (Type::U8, Type::U8)
            | (Type::F4, Type::F4)
            | (Type::F8, Type::F8) => true,
            (Type::UnitStruct(key1), Type::UnitStruct(key2)) if key1 == key2 => true,
            (Type::TupleStruct(key1), Type::TupleStruct(key2)) if key1 == key2 => true,
            (Type::FieldsStruct(key1), Type::FieldsStruct(key2)) if key1 == key2 => true,
            _ => false,
        }
    }
}
