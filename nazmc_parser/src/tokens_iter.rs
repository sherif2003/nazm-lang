use nazmc_lexer::{Token, TokenKind};

pub(crate) struct TokensIter<'a> {
    pub(crate) peek_idx: usize,
    pub(crate) tokens: &'a [Token],
    pub(crate) content: &'a str,
}

impl<'a> TokensIter<'a> {
    pub(crate) fn new(tokens: &'a [Token], content: &'a str) -> Self {
        Self {
            peek_idx: 0,
            tokens,
            content,
        }
    }

    #[inline]
    pub(crate) fn peek(&self) -> Option<&Token> {
        self.peek_nth(0)
    }

    #[inline]
    pub(crate) fn peek_nth(&self, nth: usize) -> Option<&Token> {
        self.nth(self.peek_idx + nth)
    }

    #[inline]
    pub(crate) fn recent(&self) -> Option<&Token> {
        self.recent_nth(0)
    }

    #[inline]
    pub(crate) fn recent_nth(&self, nth: usize) -> Option<&Token> {
        if self.peek_idx > nth {
            self.nth(self.peek_idx - nth - 1)
        } else {
            None
        }
    }

    #[inline]
    pub(crate) fn nth(&self, nth: usize) -> Option<&Token> {
        if nth < self.tokens.len() {
            Some(&self.tokens[nth])
        } else {
            None
        }
    }

    #[inline]
    pub(crate) fn next(&mut self) -> Option<&Token> {
        if self.peek_idx > self.tokens.len() {
            return None;
        }
        self.peek_idx += 1;
        self.recent()
    }

    pub(crate) fn next_non_space_or_comment(&mut self) -> Option<&Token> {
        while let Some(Token {
            kind:
                TokenKind::Eol | TokenKind::DelimitedComment | TokenKind::LineComment | TokenKind::Space,
            ..
        }) = self.next()
        {}

        self.peek()
    }
}
