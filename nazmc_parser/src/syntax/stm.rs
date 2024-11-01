use super::*;

#[derive(NazmcParse, Debug)]
pub(crate) enum Stm {
    Semicolon(SemicolonSymbol),
    Let(LetStm),
    While(WhileStm),
    If(IfExpr),
    When(WhenExpr),
    Expr(ExprStm),
}

#[derive(NazmcParse, Debug)]
pub(crate) struct LetStm {
    pub(crate) let_keyword: LetKeyword,
    pub(crate) binding: ParseResult<Binding>,
    pub(crate) let_assign: Option<LetAssign>,
    pub(crate) semicolon: ParseResult<SemicolonSymbol>,
}

#[derive(NazmcParse, Debug)]
pub(crate) struct Binding {
    pub(crate) kind: BindingKind,
    pub(crate) typ: Option<ColonWithType>,
}

#[derive(NazmcParse, Debug)]
pub(crate) enum BindingKind {
    Id(Id),
    MutId(MutIdBinding),
    Destructed(Box<DestructedTuple>), // Box for the large size
}

#[derive(NazmcParse, Debug)]
pub(crate) struct MutIdBinding {
    pub(crate) mut_keyword: MutKeyword,
    pub(crate) id: ParseResult<Id>,
}

generatePunctuatedItem!(Binding);

generatePunctuatedItem!(BindingKind);

generateDelimitedPunctuated!(
    DestructedTuple,
    OpenParenthesisSymbol,
    BindingKind,
    CloseParenthesisSymbol
);

#[derive(NazmcParse, Debug)]
pub(crate) struct ColonWithType {
    pub(crate) colon: ColonSymbol,
    pub(crate) typ: ParseResult<Type>,
}

#[derive(NazmcParse, Debug)]
pub(crate) struct LetAssign {
    pub(crate) equal: EqualSymbol,
    pub(crate) expr: ParseResult<Expr>,
}

#[derive(NazmcParse, Debug)]
pub(crate) struct WhileStm {
    pub(crate) while_keyword: WhileKeyword,
    pub(crate) conditional_block: ConditionalBlock,
}

#[derive(NazmcParse, Debug)]
pub struct DoWhileStm {
    // TODO
    pub(crate) do_keyword: DoKeyword,
    /// This must be checked that it doesn't have a lambda arrow
    pub(crate) block: ParseResult<LambdaExpr>,
    pub(crate) while_keyword: ParseResult<WhileKeyword>,
    pub(crate) condition: ParseResult<Expr>,
}

#[derive(NazmcParse, Debug)]
pub(crate) struct ExprStm {
    pub(crate) expr: Expr,
    pub(crate) semicolon: ParseResult<SemicolonSymbol>,
}
