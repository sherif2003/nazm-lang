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
use typed_ast::{ArrayType, FnPtrType, LambdaType, TupleType, Ty, Type, TypedAST};

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

        println!("Fns Len: {}", fns.len());
        for _fn in fns {
            self.current_file_key = _fn.info.file_key;
            self.analyze_scope(_fn.scope_key);
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
            .collect();

        let return_typ = self.analyze_type_expr(self.ast.fns[fn_key].return_type).0;

        self.typed_ast
            .fns_signatures
            .insert(fn_key, FnPtrType { params, return_typ });
    }

    fn analyze_scope(&mut self, scope_key: ScopeKey) {
        let stms = &self.ast.scopes[scope_key].stms.clone(); // TODO: Remove the clone

        println!("Stms Len: {}", stms.len());
        for stm in stms {
            match stm {
                Stm::Let(let_stm_key) => {
                    let let_stm_type = self.ast.lets[*let_stm_key]
                        .binding
                        .typ
                        .map_or(Ty::new(Type::Unknown), |type_expr_key| {
                            self.analyze_type_expr(type_expr_key).0
                        });

                    println!("{:?}", let_stm_type);

                    if let Some(expr_key) = self.ast.lets[*let_stm_key].assign {
                        self.infer_expr(&let_stm_type, expr_key);
                    }

                    // self.typed_ast
                    //     .lets
                    //     .insert(*let_stm_key, let_stm_type.clone());
                }
                Stm::While(expr_key, scope_key) => todo!(),
                Stm::If(if_expr) => todo!(),
                Stm::Expr(expr_key) => self.infer_expr(&Ty::new(Type::Unknown), *expr_key),
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

    fn fmt_item_name(&self, item_info: ItemInfo) -> String {
        let pkg = self.fmt_pkg_name(self.files_to_pkgs[item_info.file_key]);
        let name = &self.id_pool[item_info.id_key];
        if pkg.is_empty() {
            name.to_owned()
        } else {
            format!("{}::{}", pkg, name)
        }
    }

    fn fmt_type(&self, ty: &Ty) -> String {
        match ty.inner() {
            Type::Unknown => format!("{{غير معروف}}"),
            Type::Never => format!("!!"),
            Type::Unit => format!("()"),
            Type::UnspecifiedUnsignedInt => format!("{{عدد}}"),
            Type::UnspecifiedSignedInt => format!("{{عدد صحيح}}"),
            Type::UnspecifiedFloat => format!("{{عدد عشري}}"),
            Type::I => format!("ص"),
            Type::I1 => format!("ص1"),
            Type::I2 => format!("ص2"),
            Type::I4 => format!("ص4"),
            Type::I8 => format!("ص8"),
            Type::U => format!("ط"),
            Type::U1 => format!("ط1"),
            Type::U2 => format!("ط2"),
            Type::U4 => format!("ط4"),
            Type::U8 => format!("ط8"),
            Type::F4 => format!("ع4"),
            Type::F8 => format!("ع8"),
            Type::Bool => format!("شرط"),
            Type::Char => format!("حرف"),
            Type::Str => format!("متن"),
            Type::UnitStruct(unit_struct_key) => {
                let item_info = self.ast.unit_structs[unit_struct_key].info;
                self.fmt_item_name(item_info)
            }
            Type::TupleStruct(tuple_struct_key) => {
                let item_info = self.ast.tuple_structs[tuple_struct_key].info;
                self.fmt_item_name(item_info)
            }
            Type::FieldsStruct(fields_struct_key) => {
                let item_info = self.ast.fields_structs[fields_struct_key].info;
                self.fmt_item_name(item_info)
            }
            Type::Slice(rc_cell) => format!("[{}]", self.fmt_type(&rc_cell)),
            Type::Ptr(rc_cell) => format!("*{}", self.fmt_type(&rc_cell)),
            Type::Ref(rc_cell) => format!("#{}", self.fmt_type(&rc_cell)),
            Type::PtrMut(rc_cell) => format!("*متغير {}", self.fmt_type(&rc_cell)),
            Type::RefMut(rc_cell) => format!("#متغير {}", self.fmt_type(&rc_cell)),
            Type::Array(array_type) => format!(
                "[{}؛ {}]",
                self.fmt_type(&array_type.underlying_typ),
                array_type.size
            ),
            Type::Tuple(tuple_type) => {
                format!(
                    "({})",
                    tuple_type
                        .types
                        .into_iter()
                        .map(|field_info| self.fmt_type(&field_info.typ))
                        .collect::<Vec<_>>()
                        .join("، ")
                )
            }
            Type::Lambda(lambda_type) => format!(
                "({}) -> {}",
                lambda_type
                    .params_types
                    .into_iter()
                    .map(|param_ty| self.fmt_type(&param_ty))
                    .collect::<Vec<_>>()
                    .join("، "),
                self.fmt_type(&lambda_type.return_type)
            ),
            Type::FnPtr(fn_ptr_type) => format!(
                "دالة({}) -> {}",
                fn_ptr_type
                    .params
                    .into_iter()
                    .map(|param_ty| self.fmt_type(&param_ty))
                    .collect::<Vec<_>>()
                    .join("، "),
                self.fmt_type(&fn_ptr_type.return_typ)
            ),
        }
    }

    fn add_type_mismatch_err(&mut self, expected_ty: &Ty, found_ty: &Ty, span: Span) {
        let mut code_window = CodeWindow::new(&self.files_infos[self.current_file_key], span.start);
        code_window.mark_error(
            span,
            vec![format!(
                "يُتوقّع النوع `{}` ولكن تم العثور على النوع `{}`",
                self.fmt_type(expected_ty),
                self.fmt_type(found_ty)
            )],
        );
        let diagnostic = Diagnostic::error("أنواع غير متطابقة".into(), vec![code_window]);
        self.diagnostics.push(diagnostic);
    }

    fn is_subtype_of(&self, sub: &Ty, sup: &Ty) -> bool {
        match (sub.inner(), sup.inner()) {
            (Type::Unknown, _)
            | (
                Type::UnspecifiedUnsignedInt,
                Type::UnspecifiedSignedInt
                | Type::I
                | Type::I1
                | Type::I2
                | Type::I4
                | Type::I8
                | Type::U
                | Type::U1
                | Type::U2
                | Type::U4
                | Type::U8,
            )
            | (Type::UnspecifiedSignedInt, Type::I | Type::I1 | Type::I2 | Type::I4 | Type::I8)
            | (Type::UnspecifiedFloat, Type::UnspecifiedFloat | Type::F4 | Type::F8) => {
                *sub.borrow_mut() = sup.inner();
                true
            }
            (Type::UnspecifiedUnsignedInt, Type::UnspecifiedUnsignedInt)
            | (Type::UnspecifiedSignedInt, Type::UnspecifiedSignedInt)
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
            | (Type::F8, Type::F8)
            | (Type::Never, _) => true,
            (Type::UnitStruct(key1), Type::UnitStruct(key2)) if key1 == key2 => true,
            (Type::TupleStruct(key1), Type::TupleStruct(key2)) if key1 == key2 => true,
            (Type::FieldsStruct(key1), Type::FieldsStruct(key2)) if key1 == key2 => true,
            (Type::Slice(sub), Type::Slice(sup))
            | (Type::Ptr(sub), Type::Ptr(sup))
            | (Type::Ref(sub), Type::Ref(sup) | Type::Ptr(sup))
            | (Type::PtrMut(sub), Type::PtrMut(sup))
            | (Type::RefMut(sub), Type::RefMut(sup) | Type::PtrMut(sup))
            | (
                Type::Array(ArrayType {
                    underlying_typ: sub,
                    size: _,
                }),
                Type::Array(ArrayType {
                    underlying_typ: sup,
                    size: _,
                })
                | Type::Slice(sup),
            ) => self.is_subtype_of(&sub, &sup),
            (Type::Tuple(sub), Type::Tuple(sup)) => sub
                .types
                .into_iter()
                .zip(sup.types)
                .all(|(sub, sup)| self.is_subtype_of(&sub.typ, &sup.typ)),
            (Type::Lambda(sub), Type::Lambda(sup)) => {
                sub.params_types
                    .into_iter()
                    .zip(sup.params_types)
                    .all(|(sub, sup)| self.is_subtype_of(&sup, &sub))
                    && self.is_subtype_of(&sub.return_type, &sup.return_type)
            }
            (Type::FnPtr(sub), Type::FnPtr(sup)) => {
                sub.params
                    .into_iter()
                    .zip(sup.params)
                    .all(|(sub, sup)| self.is_subtype_of(&sup, &sub))
                    && self.is_subtype_of(&sub.return_typ, &sup.return_typ)
            }
            _ => false,
        }
    }
}
