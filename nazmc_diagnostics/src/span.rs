use std::{fmt::Debug, usize};

#[derive(Clone, Copy, PartialEq, Default, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    pub start: SpanCursor,
    pub end: SpanCursor,
}

#[derive(Clone, Copy, PartialEq, Default, Eq, PartialOrd, Ord, Hash)]
pub struct SpanCursor {
    /// The line index
    pub line: usize,
    /// The column index
    pub col: usize,
}

impl Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "From {:?} to {:?}", self.start, self.end)
    }
}

impl Debug for SpanCursor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line + 1, self.col)
    }
}

impl Span {
    #[inline]
    pub fn new(start: (usize, usize), end: (usize, usize)) -> Self {
        Self {
            start: SpanCursor {
                line: start.0,
                col: start.1,
            },
            end: SpanCursor {
                line: end.0,
                col: end.1,
            },
        }
    }

    #[inline]
    pub fn merged_with(&self, with: &Span) -> Self {
        Self {
            start: self.start,
            end: with.end,
        }
    }

    /// Returns a zero-column span located after given the span
    #[inline]
    pub fn after(given: &Span) -> Self {
        Self {
            start: given.end,
            end: given.end,
        }
    }

    /// Returns a zero-column span located after given the span
    #[inline]
    pub fn len_after(given: &Span, len: usize) -> Self {
        Self {
            start: given.end,
            end: SpanCursor {
                line: given.end.line,
                col: given.end.col + len,
            },
        }
    }
}

#[inline]
pub fn sort_spans(spans: &mut [Span]) {
    spans.sort_by_key(|a| a.start.line);

    spans
        .chunk_by_mut(|a, b| a.start.line == b.start.line)
        .for_each(|f| {
            f.sort_by_key(|a| a.start.col);
        });
}
