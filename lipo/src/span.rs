use std::fmt::{self, Debug};
use std::ops::Range;


pub trait Spanned {
    fn span(&self) -> FreeSpan;
}


#[derive(Clone, Copy, Default, Hash, PartialEq, Eq)]
pub struct FreeSpan {
    pub start: u32,
    pub end: u32,
}

impl From<Range<usize>> for FreeSpan {
    fn from(range: Range<usize>) -> Self {
        let start = range.start.try_into().expect("out of span range");
        let end = range.end.try_into().expect("out of span range");
        FreeSpan { start, end }
    }
}

impl FreeSpan {
    pub fn range(self) -> Range<usize> {
        (self.start as usize)..(self.end as usize)
    }

    pub fn anchor<'src>(self, source: &'src str) -> Span<'src> {
        let FreeSpan { start, end } = self;

        assert!(
            source.is_char_boundary(start as usize) &&
            source.is_char_boundary(end as usize),
            "span boundaries are not valid char boundaries",
        );

        Span {
            source,
            start,
            end,
        }
    }

    pub fn join(a: FreeSpan, b: FreeSpan) -> FreeSpan {
        FreeSpan {
            start: Ord::min(a.start, b.start),
            end: Ord::max(a.end, b.end),
        }
    }
}

impl Debug for FreeSpan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Span")
            .field(&self.range())
            .finish()
    }
}


#[derive(Clone, Copy)]
pub struct Span<'src> {
    source: &'src str,
    start: u32,
    end: u32,
}

impl<'src> Debug for Span<'src> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (l1, l2) = self.lines();
        let (c1, c2) = self.columns();
        f.debug_tuple("Span")
            .field(&((l1, c1) .. (l2, c2)))
            .field(&self.as_str())
            .finish()
    }
}

impl<'src> Span<'src> {
    fn range(&self) -> Range<usize> {
        (self.start as usize)..(self.end as usize)
    }

    pub fn as_str(&self) -> &'src str {
        &self.source[self.range()]
    }

    pub fn is_multiline(&self) -> bool {
        self.as_str().as_bytes().contains(&b'\n')
    }

    pub fn line_parts(&self) -> Option<(&'src str, &'src str, &'src str)> {
        if self.is_multiline() {
            return None;
        }

        let Range { start, end } = self.range();

        let before = &self.source[..start];
        let before = if before.ends_with('\n') {
            // `str::lines` ignores the trailing newline, if we don't special case it here
            // we get the previous line instead
            ""
        } else {
            before.lines()
                .next_back()
                .unwrap_or("")
        };
        let after = self.source[end..]
            .lines()
            .next()
            .unwrap_or("");
        let this = self.as_str();

        Some((before, this, after))
    }

    /// Returns on which line does the span start and on which it ends
    ///
    /// Lines are numbered starting from 1. Lines are delimited by a single `\n` (`0x0a`) byte.
    pub fn lines(&self) -> (u32, u32) {
        fn lines(src: &str, count_trailing: bool) -> usize {
            if src.is_empty() {
                return 1;
            }

            // `str::lines` ignores the trailing newline however we want to include it
            let buff = (count_trailing && src.ends_with('\n')) as usize;

            src.lines().count() + buff
        }

        let Range { start, end } = self.range();
        (
            lines(&self.source[..start], true) as u32,
            lines(&self.source[..end], false) as u32,
        )
    }

    /// Returns columns for the start and end position of the span.
    ///
    /// Columns are numbered starting from 1.
    pub fn columns(&self) -> (u32, u32) {
        fn column(src: &str) -> usize {
            let last_line = src.lines()
                .next_back().unwrap_or("");

            #[cfg(feature = "full-unicode")] {
                unicode_width::UnicodeWidthStr::width(last_line)
            }

            #[cfg(not(feature = "full-unicode"))] {
                last_line.as_bytes().len()
            }
        }

        let Range { start, end } = self.range();
        (
            column(&self.source[..start]) as u32 + 1,
            column(&self.source[..end]).max(1) as u32,
        )
    }
}

#[cfg(test)]
mod test {
    use super::{FreeSpan, Span};
    use std::ops::Range;

    /// Creates a Span from `source` between marker characters in the text.
    /// Resulting span does not include the markers.
    fn span<'src>(
        source: &'src str,
        range: Range<usize>,
    ) -> Span<'src> {
        FreeSpan::from(range).anchor(source)
    }

    #[test]
    fn test_single_line() {
        let sp = span("hi", 0..2);
        //             ^^
        assert_eq!(
            sp.as_str(),
            "hi",
        );
        assert!(!sp.is_multiline());
    }

    #[test]
    fn test_multi_line() {
        let sp = span("lorem ipsum\ndolor sit amet", 6..21);
        //                   ^^^^^^^^^^^^^^^^
        assert_eq!(
            sp.as_str(),
            "ipsum
dolor sit",
        );
        assert!(sp.is_multiline());
    }

    #[test]
    fn test_lines() {
        let sp = span("", 0..0);
        assert_eq!(sp.as_str(), "");
        assert_eq!(sp.lines(), (1, 1));

        let sp = span("\n", 0..1);
        //             ^^
        assert_eq!(sp.as_str(), "\n");
        assert_eq!(sp.lines(), (1, 1));

        let sp = span("1", 0..1);
        //             ^
        assert_eq!(sp.as_str(), "1");
        assert_eq!(sp.lines(), (1, 1));

        let sp = span("1\n2", 2..3);
        //                ^
        assert_eq!(sp.as_str(), "2");
        assert_eq!(sp.lines(), (2, 2));

        let sp = span("1\n2\n3", 2..5);
        //                ^^^^
        assert_eq!(sp.as_str(), "2\n3");
        assert_eq!(sp.lines(), (2, 3));

        let sp = span("1\n2\n3", 2..4);
        //                ^^^
        assert_eq!(sp.as_str(), "2\n");
        assert_eq!(sp.lines(), (2, 2));
    }

    #[test]
    fn test_columns() {
        let sp = span("", 0..0);
        assert_eq!(sp.columns(), (1, 1));

        let sp = span("1", 0..1);
        //             ^
        assert_eq!(sp.columns(), (1, 1));

        let sp = span("1234", 0..4);
        //             ^^^^
        assert_eq!(sp.columns(), (1, 4));

        let sp = span("1\n1", 0..3);
        //             ^^^^
        assert_eq!(sp.columns(), (1, 1));

        let sp = span("1\n1", 0..2);
        //             ^^^
        assert_eq!(sp.columns(), (1, 1));
    }

    #[cfg_attr(not(feature = "full-unicode"), ignore)]
    #[test]
    fn test_columns_unicode() {
        // Single emoji is a double-width character and gets properly counted as such.
        let sp = span("👩", 0..4);
        //             ^^
        assert_eq!(sp.columns(), (1, 2));

        // Compound emoji get counted as N separate emoji, the zero-width separator(s) don't get
        // counted in the width.
        //
        // This should theoretically get rendered as one double-width glyph so the result of 4 is
        // incorrect, however not every terminal and every configuration is going to be capable of
        // that anyway so we're just going to go with what unicode_width says and accept the
        // slightly broken underlines when compound emoji are involved.
        let sp = span("👩‍🔬", 0..11);
        //             ^^ (supposed to underline the single "woman scientist" emoji, your text
        //                 editor rendering might not reflect that)
        assert_eq!(sp.columns(), (1, 4));
    }
}
