use super::*;
use paste::paste;

pub(crate) mod punctuated;
use punctuated::*;

pub(crate) mod terminal;
pub(crate) use terminal::*;

pub(crate) mod item;
pub(crate) use item::*;

pub(crate) mod path;
pub(crate) use path::*;

pub(crate) mod typ;
pub(crate) use typ::*;

pub(crate) mod stm;
pub(crate) use stm::*;

pub(crate) mod expr;
pub(crate) use expr::*;

generateTrailingCommaWithCloseDelimiter!(CloseParenthesisSymbol);

generateTrailingCommaWithCloseDelimiter!(CloseAngleBracketOrGreaterSymbol);

generateTrailingCommaWithCloseDelimiter!(CloseCurlyBraceSymbol);

generateTrailingCommaWithCloseDelimiter!(CloseSquareBracketSymbol);

generateTrailingCommaWithCloseDelimiter!(RArrowSymbol);

#[derive(NazmcParse, Debug)]
pub(crate) struct File {
    pub(crate) content: ZeroOrMany<FileItem, Eof>,
}
