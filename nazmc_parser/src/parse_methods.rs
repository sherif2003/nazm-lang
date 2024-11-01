/// This module defines the core components and traits required for parsing an Abstract Syntax Tree (AST)
/// in the Nazmc language parser. It provides the foundational structures and parsing logic for different
/// AST node types, ensuring that the syntax is correctly interpreted and processed.
use super::tokens_iter::TokensIter;

/// The `NazmcParse` trait must be implemented by all AST nodes. It defines a `parse` method that takes
/// a mutable reference to a `TokensIter` and returns an instance of the implementing type.
pub(crate) trait NazmcParse
where
    Self: Sized,
{
    fn parse(iter: &mut TokensIter) -> Self;
}

pub type ParseResult<T> = Result<T, ParseErr>;

#[derive(Clone, Debug)]
pub struct ParseErr {
    pub found_token_index: usize,
}

impl ParseErr {
    pub fn eof<T>() -> ParseResult<T> {
        Err(ParseErr {
            found_token_index: usize::MAX,
        })
    }
}

/// Parses a sequence of items where the number of items can vary from zero to many.
///
/// The `ZeroOrMany` parser continues to parse items until a terminator or an unexpected token is encountered.
/// It handles variable-length sequences robustly and attempts to recover from errors by backtracking and
/// continuing parsing if possible.
///
/// # Parsing Logic
///
/// 1. **Initial Parsing:** Begins by parsing items and tracking both successfully parsed items and unexpected tokens.
/// 2. **Continue Parsing:** Continues to parse items until a terminator (specific end token) is found or an unexpected token is encountered.
/// 3. **Error Handling:** When an unexpected token is encountered, it attempts to recover by backtracking and re-parsing the terminator. If recovery fails, it treats the unexpected token as part of the results, skips it, and resumes parsing.
/// 4. **Return Results:** Returns the collected items and any terminator information or handles unexpected tokens as needed.
///
/// You can think of it as a loop that processes two variants: items (trees) and terminators.
/// The loop continues parsing items, and if parsing an item fails, it backtracks and tries to parse the terminator.
/// Once the terminator is found, the loop ends.
///
/// # Parameters
/// - `item_parser`: A parser for individual items in the sequence.
/// - `terminator_parser`: A parser for the terminator that signifies the end of the sequence.
///
/// # Returns
/// A result containing the successfully parsed items, terminator information if present, or details about any errors encountered during parsing.
///
/// # Errors
/// The parser will handle unexpected tokens by attempting recovery or including them in the results if recovery fails.
#[derive(Debug)]
pub struct ZeroOrMany<Tree, Terminator>
where
    ParseResult<Tree>: NazmcParse,
    ParseResult<Terminator>: NazmcParse,
{
    pub items: Vec<ParseResult<Tree>>,
    pub terminator: ParseResult<Terminator>,
}

/// `OneOrMany` represents a sequence that starts with at least one occurrence of a specific AST node type, followed by a terminator.
/// It ensures that at least the first item is successfully parsed. The implementation may change in the future and might be rewritten in terms of other components.
#[derive(Debug)]
pub struct OneOrMany<Tree, Terminator>
where
    ParseResult<Tree>: NazmcParse,
    ParseResult<Terminator>: NazmcParse,
{
    pub first: ParseResult<Tree>,
    pub rest: Vec<ParseResult<Tree>>,
    pub terminator: ParseResult<Terminator>,
}

/// Implementations of the `NazmcParse` trait for different parsing structures.

impl<ParseMethod> NazmcParse for Box<ParseMethod>
where
    ParseMethod: NazmcParse,
{
    fn parse(iter: &mut TokensIter) -> Self {
        let parsed = ParseMethod::parse(iter);
        Box::new(parsed)
    }
}

impl<Tree> NazmcParse for Option<Tree>
where
    ParseResult<Tree>: NazmcParse,
{
    fn parse(iter: &mut TokensIter) -> Self {
        match ParseResult::parse(iter) {
            Ok(tree) => Some(tree),
            Err(_) => None,
        }
    }
}

impl<Tree> NazmcParse for Vec<Tree>
where
    ParseResult<Tree>: NazmcParse,
{
    fn parse(iter: &mut TokensIter) -> Self {
        // Parses multiple AST nodes into a `Vec`. It continues parsing until no more valid nodes are found.
        let mut items = vec![];
        loop {
            let peek_idx = iter.peek_idx;
            match ParseResult::parse(iter) {
                Ok(tree) => items.push(tree),
                _ => {
                    iter.peek_idx = peek_idx; // Backtrack
                    break;
                }
            }
        }
        items
    }
}

impl<Tree, Terminator> NazmcParse for ZeroOrMany<Tree, Terminator>
where
    ParseResult<Tree>: NazmcParse,
    ParseResult<Terminator>: NazmcParse,
{
    /// see [ZeroOrMany]
    fn parse(iter: &mut TokensIter) -> Self {
        let mut items = vec![];

        loop {
            // No more tokens
            if iter.recent().is_none() {
                return Self {
                    items,
                    terminator: ParseErr::eof(),
                };
            }
            let old_peek_idx = iter.peek_idx;
            match ParseResult::<Tree>::parse(iter) {
                parsed_node @ Ok(_) => {
                    items.push(parsed_node);
                }
                unexpected_token @ Err(_) => {
                    let new_peek_idx = iter.peek_idx;

                    iter.peek_idx = old_peek_idx; // Try to backtrack and parse the terminator

                    // Check for terminator
                    if let terminator @ Ok(_) = ParseResult::<Terminator>::parse(iter) {
                        return Self { items, terminator };
                    }

                    // Backtracking doesn't work either
                    // so add this unexpected result to items
                    // then reset to failure and skip this unexpected token
                    items.push(unexpected_token);
                    iter.peek_idx = new_peek_idx;
                    iter.next_non_space_or_comment();
                }
            }
        }
    }
}

impl<Tree, Terminator> NazmcParse for OneOrMany<Tree, Terminator>
where
    ParseResult<Tree>: NazmcParse,
    ParseResult<Terminator>: NazmcParse,
{
    fn parse(iter: &mut TokensIter) -> Self {
        let first = match ParseResult::parse(iter) {
            parsed_node @ Ok(_) => parsed_node,
            unexpected_node @ Err(_) => {
                return Self {
                    first: unexpected_node,
                    rest: vec![],
                    terminator: ParseErr::eof(),
                };
            }
        };

        let zero_or_many = ZeroOrMany::parse(iter);

        Self {
            first,
            rest: zero_or_many.items,
            terminator: zero_or_many.terminator,
        }
    }
}

#[cfg(test)]
mod tests {

    use nazmc_data_pool::DataPool;
    use nazmc_parse_derive::{NazmcParse, SpannedAndCheck};

    use super::super::*;
    use super::*;

    #[derive(NazmcParse)]
    pub enum TermBinOp {
        Plus(PlusSymbol),
        Minus(Box<MinusSymbol>),
    }

    #[test]
    fn test_enum() {
        let mut id_pool = DataPool::new();
        let mut str_pool = DataPool::new();
        let content = "+-  /** */ - +";
        let (tokens, ..) = LexerIter::new(content, &mut id_pool, &mut str_pool).collect_all();
        let mut tokens_iter = TokensIter::new(&tokens, content);
        tokens_iter.next(); // Init recent

        let parse_result = <ParseResult<TermBinOp>>::parse(&mut tokens_iter);
        let op = parse_result.unwrap();
        assert!(matches!(op, TermBinOp::Plus(_)));

        let parse_result = <ParseResult<TermBinOp>>::parse(&mut tokens_iter);
        let op = parse_result.unwrap();
        assert!(matches!(op, TermBinOp::Minus(_)));

        let parse_result = <ParseResult<TermBinOp>>::parse(&mut tokens_iter);
        let op = parse_result.unwrap();
        assert!(matches!(op, TermBinOp::Minus(_)));

        let parse_result = <ParseResult<TermBinOp>>::parse(&mut tokens_iter);
        let op = parse_result.unwrap();
        assert!(matches!(op, TermBinOp::Plus(_)));
    }

    #[derive(NazmcParse, Debug)]
    pub struct SimpleFn {
        pub _fn: FnKeyword,
        pub _id: ParseResult<Id>,
        pub _params_decl: ParseResult<FnParamsDecl>,
    }

    #[derive(SpannedAndCheck, Debug)]
    pub struct FnParamsDecl {
        pub _open_paren: OpenParenthesisSymbol,
        pub _params: Option<FnParams>,
        pub _close_paren: ParseResult<CloseParenthesisSymbol>,
    }

    impl NazmcParse for ParseResult<FnParamsDecl> {
        fn parse(iter: &mut TokensIter) -> Self {
            let parse_result = ParseResult::<FnParamsDeclImpl>::parse(iter);

            let decl_impl_node = match parse_result {
                Ok(decl_impl) => decl_impl,
                Err(err) => return Err(err),
            };

            let open_paren = decl_impl_node._open_paren;

            // The unexpected case is unreachable as it will be include in WithParams case, so we can safely unwrap it
            let close = decl_impl_node._fn_param_close.unwrap();

            let fn_decl_with_params = match close {
                CloseFnParamsDecl::NoParams(close_paren) => {
                    return Ok(FnParamsDecl {
                        _open_paren: open_paren,
                        _params: Option::None,
                        _close_paren: Ok(close_paren),
                    })
                }
                CloseFnParamsDecl::WithParams(fn_decl_with_params) => fn_decl_with_params,
            };

            let first_param = fn_decl_with_params._first_param;

            let rest_params = fn_decl_with_params._params.items;

            let (trailing_comma, close_paren) = match fn_decl_with_params._params.terminator {
                Ok(node) => (node._comma, Ok(node._close_paren)),
                Err(err) => (Option::None, Err(err)),
            };

            let params = FnParams {
                _first_param: first_param,
                _rest_params: rest_params,
                _trailing_comma: trailing_comma,
            };

            Ok(FnParamsDecl {
                _open_paren: open_paren,
                _params: Option::Some(params),
                _close_paren: close_paren,
            })
        }
    }

    #[derive(SpannedAndCheck, Debug)]
    pub struct FnParams {
        pub _first_param: ParseResult<FnParam>,
        pub _rest_params: Vec<ParseResult<CommaWithFnParam>>,
        pub _trailing_comma: Option<CommaSymbol>,
    }

    impl NazmcParse for ParseResult<FnParams> {
        fn parse(_iter: &mut TokensIter) -> Self {
            unreachable!() // Just added to usee it as Optional
        }
    }

    #[derive(NazmcParse, Debug)]
    pub struct FnParamsDeclImpl {
        pub _open_paren: OpenParenthesisSymbol,
        pub _fn_param_close: ParseResult<CloseFnParamsDecl>,
    }

    #[derive(NazmcParse, Debug)]
    pub enum CloseFnParamsDecl {
        NoParams(CloseParenthesisSymbol),
        WithParams(Box<FnDeclWithParams>),
    }

    #[derive(NazmcParse, Debug)]
    pub struct FnDeclWithParams {
        pub _first_param: ParseResult<FnParam>,
        pub _params: ZeroOrMany<CommaWithFnParam, CommaWithCloseParenthesis>,
    }

    #[derive(NazmcParse, Debug)]
    pub struct CommaWithFnParam {
        _comma: CommaSymbol,
        _fn_param: FnParam,
    }

    #[derive(NazmcParse, Debug)]
    pub struct CommaWithCloseParenthesis {
        _comma: Option<CommaSymbol>,
        _close_paren: CloseParenthesisSymbol,
    }

    #[derive(NazmcParse, Debug)]
    pub struct FnParam {
        pub _name: Id,
        pub _colon: ParseResult<ColonSymbol>,
        pub _type: ParseResult<Id>,
    }

    #[test]
    fn test_wrong_params() {
        let mut id_pool = DataPool::new();
        let mut str_pool = DataPool::new();
        let content = "دالة البداية(123 دالة، ت: ح 444، س: ص، ع: ك،) {}";
        let (tokens, ..) = LexerIter::new(content, &mut id_pool, &mut str_pool).collect_all();
        let mut tokens_iter = TokensIter::new(&tokens, content);
        tokens_iter.next(); // Init recent

        let parse_result = <ParseResult<SimpleFn>>::parse(&mut tokens_iter);
        let fn_node = parse_result.unwrap();

        assert!(fn_node.is_broken());

        let params_decl = fn_node._params_decl.unwrap();
        assert!(params_decl.is_broken());
        assert!(params_decl._close_paren.is_ok());

        let params = params_decl._params.unwrap();
        assert!(params.is_broken());

        assert!(params._first_param.is_err());
        println!("{:#?}\n----------", params._first_param.unwrap_err());
        assert!(params._trailing_comma.is_some());

        for param in params._rest_params {
            println!("{:#?}", param)
        }
    }

    #[test]
    fn test_zero_params() {
        let mut id_pool = DataPool::new();
        let mut str_pool = DataPool::new();
        let content = "دالة البداية() {}";
        let (tokens, ..) = LexerIter::new(content, &mut id_pool, &mut str_pool).collect_all();
        let mut tokens_iter = TokensIter::new(&tokens, content);
        tokens_iter.next(); // Init recent

        let fn_node = <ParseResult<SimpleFn>>::parse(&mut tokens_iter).unwrap();
        assert!(!fn_node.is_broken());

        let params_decl = fn_node._params_decl.unwrap();
        assert!(!params_decl.is_broken());
        assert!(!params_decl._open_paren.is_broken());
        assert!(!params_decl._close_paren.unwrap().is_broken());

        assert!(params_decl._params.is_none());
    }

    #[test]
    fn test_one_param_no_trailing_comma() {
        let mut id_pool = DataPool::new();
        let mut str_pool = DataPool::new();
        let content = "دالة البداية(س: ص8) {}";
        let (tokens, ..) = LexerIter::new(content, &mut id_pool, &mut str_pool).collect_all();
        let mut tokens_iter = TokensIter::new(&tokens, content);
        tokens_iter.next(); // Init recent

        let fn_node = <ParseResult<SimpleFn>>::parse(&mut tokens_iter).unwrap();
        assert!(!fn_node.is_broken());

        let params_decl = fn_node._params_decl.unwrap();
        assert!(!params_decl.is_broken());
        assert!(!params_decl._open_paren.is_broken());
        assert!(!params_decl._close_paren.unwrap().is_broken());

        let params = params_decl._params.unwrap();
        assert!(!params.is_broken());

        assert!(!params._first_param.unwrap().is_broken());
        assert!(params._rest_params.is_empty());
        assert!(params._trailing_comma.is_none());
    }

    #[test]
    fn test_one_param_with_trailing_comma() {
        let mut id_pool = DataPool::new();
        let mut str_pool = DataPool::new();
        let content = "دالة البداية(س: ص8،) {}";
        let (tokens, ..) = LexerIter::new(content, &mut id_pool, &mut str_pool).collect_all();
        let mut tokens_iter = TokensIter::new(&tokens, content);
        tokens_iter.next(); // Init recent

        let fn_node = <ParseResult<SimpleFn>>::parse(&mut tokens_iter).unwrap();
        assert!(!fn_node.is_broken());

        let params_decl = fn_node._params_decl.unwrap();
        assert!(!params_decl.is_broken());
        assert!(!params_decl._open_paren.is_broken());
        assert!(!params_decl._close_paren.unwrap().is_broken());

        let params = params_decl._params.unwrap();
        assert!(!params.is_broken());

        assert!(!params._first_param.unwrap().is_broken());
        assert!(params._rest_params.is_empty());
        assert!(params._trailing_comma.is_some());
    }

    #[test]
    fn test_two_params_no_trailing_comma() {
        let mut id_pool = DataPool::new();
        let mut str_pool = DataPool::new();
        let content = "دالة البداية(س: ص8، ك: م) {}";
        let (tokens, ..) = LexerIter::new(content, &mut id_pool, &mut str_pool).collect_all();
        let mut tokens_iter = TokensIter::new(&tokens, content);
        tokens_iter.next(); // Init recent

        let fn_node = <ParseResult<SimpleFn>>::parse(&mut tokens_iter).unwrap();
        assert!(!fn_node.is_broken());

        let params_decl = fn_node._params_decl.unwrap();
        assert!(!params_decl.is_broken());
        assert!(!params_decl._open_paren.is_broken());
        assert!(!params_decl._close_paren.unwrap().is_broken());

        let params = params_decl._params.unwrap();
        assert!(!params.is_broken());

        assert!(!params._first_param.unwrap().is_broken());
        assert!(params._rest_params.len() == 1);
        assert!(!params._rest_params[0].is_broken());
        assert!(params._trailing_comma.is_none());
    }

    #[test]
    fn test_two_params_with_trailing_comma() {
        let mut id_pool = DataPool::new();
        let mut str_pool = DataPool::new();
        let content = "دالة البداية(س: ص8، ك: م،) {}";
        let (tokens, ..) = LexerIter::new(content, &mut id_pool, &mut str_pool).collect_all();
        let mut tokens_iter = TokensIter::new(&tokens, content);
        tokens_iter.next(); // Init recent

        let fn_node = <ParseResult<SimpleFn>>::parse(&mut tokens_iter).unwrap();
        assert!(!fn_node.is_broken());

        let params_decl = fn_node._params_decl.unwrap();
        assert!(!params_decl.is_broken());
        assert!(!params_decl._open_paren.is_broken());
        assert!(!params_decl._close_paren.unwrap().is_broken());

        let params = params_decl._params.unwrap();
        assert!(!params.is_broken());

        assert!(!params._first_param.unwrap().is_broken());
        assert!(params._rest_params.len() == 1);
        assert!(!params._rest_params[0].is_broken());
        assert!(params._trailing_comma.is_some());
    }
}
