use crate::*;

#[derive(Clone, Default)]
pub struct TypesExprs {
    pub all: TiVec<TypeExprKey, TypeExpr>,
    pub paths: TiVec<PathTypeExprKey, ItemPath>,
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
pub enum TypeExpr {
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
    pub underlying_typ: TypeExprKey,
    pub span: Span,
}

#[derive(Clone)]
pub struct SliceTypeExpr {
    pub underlying_typ: TypeExprKey,
    pub span: Span,
}

#[derive(Clone)]
pub struct PtrTypeExpr {
    pub underlying_typ: TypeExprKey,
    pub span: Span,
}

#[derive(Clone)]
pub struct RefTypeExpr {
    pub underlying_typ: TypeExprKey,
    pub span: Span,
}

#[derive(Clone)]
pub struct PtrMutTypeExpr {
    pub underlying_typ: TypeExprKey,
    pub span: Span,
}

#[derive(Clone)]
pub struct RefMutTypeExpr {
    pub underlying_typ: TypeExprKey,
    pub span: Span,
}

#[derive(Clone)]
pub struct TupleTypeExpr {
    pub types: ThinVec<TypeExprKey>,
    pub span: Span,
}

#[derive(Clone)]
pub struct ArrayTypeExpr {
    pub underlying_typ: TypeExprKey,
    pub size_expr: Expr,
    pub span: Span,
}

#[derive(Clone)]
pub struct LambdaTypeExpr {
    pub params_types: ThinVec<TypeExprKey>,
    pub return_type: TypeExprKey,
    pub span: Span,
}

#[derive(Clone, Default)]
pub struct Types {
    pub all: TiVec<TypeKey, Type>,
    pub tuples: TiVec<TupleTypeKey, TupleType>,
    pub arrays: TiVec<ArrayTypeKey, ArrayType>,
    pub lambdas: TiVec<LambdaTypeKey, LambdaType>,
}

#[derive(Clone)]
pub enum Type {
    UnitStruct(UnitStructKey),
    TupleStruct(TupleStructKey),
    FieldsStruct(FieldsStructKey),
    Slice(TypeKey),
    Ptr(TypeKey),
    Ref(TypeKey),
    PtrMut(TypeKey),
    RefMut(TypeKey),
    Tuple(TupleTypeKey),
    Array(ArrayTypeKey),
    Lambda(LambdaTypeKey),
}

#[derive(Clone)]
pub struct TupleType {
    pub types: ThinVec<TypeKey>,
}

#[derive(Clone, Copy)]
pub struct ArrayType {
    pub underlying_typ: TypeKey,
    pub size: u32,
}

#[derive(Clone)]
pub struct LambdaType {
    pub params_types: ThinVec<TypeKey>,
    pub return_type: TypeKey,
}
