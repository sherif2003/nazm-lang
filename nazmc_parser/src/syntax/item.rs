use super::*;

#[derive(NazmcParse, Debug)]
pub(crate) enum FileItem {
    ImportStm(ImportStm),
    WithVisModifier(ItemWithVisibility),
    WithoutModifier(Item),
}

#[derive(NazmcParse, Debug)]
pub(crate) struct ImportStm {
    pub(crate) import_keyword: ImportKeyword,
    pub(crate) top: ParseResult<Id>,
    pub(crate) sec: ParseResult<DoubleColonsWithPathSegInImportStm>,
    pub(crate) segs: Vec<DoubleColonsWithPathSegInImportStm>,
    pub(crate) alias: Option<ImportAlias>,
    pub(crate) semicolon: ParseResult<SemicolonSymbol>,
}

#[derive(NazmcParse, Debug)]
pub(crate) struct DoubleColonsWithPathSegInImportStm {
    pub(crate) double_colons: DoubleColonsSymbol,
    pub(crate) seg: ParseResult<PathSegInImportStm>,
}

#[derive(NazmcParse, Debug)]
pub(crate) enum PathSegInImportStm {
    Id(Id),
    Star(StarSymbol),
}

#[derive(NazmcParse, Debug)]
pub(crate) struct ImportAlias {
    /// TODO: Change the keyword
    pub(crate) on: OnKeyword,
    pub(crate) id: ParseResult<Id>
}

#[derive(NazmcParse, Debug)]
pub(crate) struct ItemWithVisibility {
    pub(crate) visibility: VisModifier,
    pub(crate) item: ParseResult<Item>,
}

#[derive(NazmcParse, Debug)]
pub(crate) enum Item {
    Struct(Struct),
    Fn(Fn),
}

#[derive(NazmcParse, Debug)]
pub(crate) struct Struct {
    pub(crate) struct_keyword: StructKeyword,
    pub(crate) name: ParseResult<Id>,
    pub(crate) kind: ParseResult<StructKind>,
}

#[derive(NazmcParse, Debug)]
pub(crate) enum StructKind {
    Unit(SemicolonSymbol),
    Tuple(TupleStructFields),
    Fields(StructFields),
}

#[derive(NazmcParse, Debug)]
pub(crate) struct TupleStructField {
    pub(crate) visibility: Option<VisModifier>,
    pub(crate) typ: ParseResult<Type>,
}

generatePunctuatedItem!(TupleStructField);

generateDelimitedPunctuated!(
    TupleStructFields,
    OpenParenthesisSymbol,
    TupleStructField,
    CloseParenthesisSymbol
);

#[derive(NazmcParse, Debug)]
pub(crate) struct StructField {
    pub(crate) visibility: Option<VisModifier>,
    pub(crate) name: Id,
    pub(crate) typ: ParseResult<ColonWithType>,
}

generatePunctuatedItem!(StructField);

generateDelimitedPunctuated!(
    StructFields,
    OpenCurlyBraceSymbol,
    StructField,
    CloseCurlyBraceSymbol
);

#[derive(NazmcParse, Debug)]
pub(crate) struct Fn {
    pub(crate) fn_keyword: FnKeyword,
    pub(crate) name: ParseResult<Id>,
    pub(crate) params_decl: ParseResult<FnParams>,
    pub(crate) return_type: Option<ColonWithType>,
    /// This must be checked that it doesn't have a lambda arrow
    pub(crate) body: ParseResult<LambdaExpr>,
}

#[derive(NazmcParse, Debug)]
pub(crate) struct FnParam {
    pub(crate) name: Id,
    pub(crate) typ: ParseResult<ColonWithType>,
}

generatePunctuatedItem!(FnParam);

generateDelimitedPunctuated!(
    FnParams,
    OpenParenthesisSymbol,
    FnParam,
    CloseParenthesisSymbol
);
