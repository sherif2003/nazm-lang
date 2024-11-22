use crate::*;

#[derive(Clone, Default)]
pub struct TypesExprs {
    pub parens: TiVec<ParenTypeExprKey, ParenTypeExpr>,
    pub slices: TiVec<SliceTypeExprKey, SliceTypeExpr>,
    pub ptrs: TiVec<PtrTypeExprKey, PtrTypeExpr>,
    pub refs: TiVec<RefTypeExprKey, RefTypeExpr>,
    pub ptrs_mut: TiVec<PtrMutTypeExprKey, PtrMutTypeExpr>,
    pub refs_mut: TiVec<RefMutTypeExprKey, RefMutTypeExpr>,
    pub tuples: TiVec<TupleTypeExprKey, TupleTypeExpr>,
    pub arrays: TiVec<ArrayTypeExprKey, ArrayTypeExpr>,
    pub lambdas: TiVec<LambdaTypeExprKey, LambdaTypeExpr>,
}

#[derive(Clone)]
pub enum Type {
    Path(PathTypeExprKey),
    Paren(ParenTypeExprKey),
    Slice(SliceTypeExprKey),
    Ptr(PtrTypeExprKey),
    Ref(RefTypeExprKey),
    PtrMut(PtrMutTypeExprKey),
    RefMut(RefMutTypeExprKey),
    Tuple(TupleTypeExprKey),
    Array(ArrayTypeExprKey),
    Lambda(LambdaTypeExprKey),
}

#[derive(Clone)]
pub struct ParenTypeExpr {
    pub underlying_typ: Type,
    pub span: Span,
}

#[derive(Clone)]
pub struct SliceTypeExpr {
    pub underlying_typ: Type,
    pub span: Span,
}

#[derive(Clone)]
pub struct PtrTypeExpr {
    pub underlying_typ: Type,
    pub span: Span,
}

#[derive(Clone)]
pub struct RefTypeExpr {
    pub underlying_typ: Type,
    pub span: Span,
}

#[derive(Clone)]
pub struct PtrMutTypeExpr {
    pub underlying_typ: Type,
    pub span: Span,
}

#[derive(Clone)]
pub struct RefMutTypeExpr {
    pub underlying_typ: Type,
    pub span: Span,
}

#[derive(Clone)]
pub struct TupleTypeExpr {
    pub types: ThinVec<Type>,
    pub span: Span,
}

#[derive(Clone)]
pub struct ArrayTypeExpr {
    pub underlying_typ: Type,
    pub size_expr: Expr,
    pub span: Span,
}

#[derive(Clone)]
pub struct LambdaTypeExpr {
    pub params_types: ThinVec<Type>,
    pub return_type: Type,
    pub span: Span,
}
