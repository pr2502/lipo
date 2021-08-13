use std::ops::Range;
use std::fmt::{self, Debug};

#[derive(Clone, Copy, Default)]
pub struct FreeSpan {
    pub offset: u32,
    pub len: u32,
}

impl From<Range<usize>> for FreeSpan {
    fn from(range: Range<usize>) -> Self {
        let offset = range.start.try_into()
            .expect("file size limit exceeded");
        let len = (range.end - range.start).try_into()
            .expect("file size limit exceeded");
        FreeSpan { offset, len }
    }
}

impl FreeSpan {
    pub fn anchor(self, source: &str) -> Span<'_> {
        let FreeSpan { offset, len } = self;
        assert!((offset + len) as usize <= source.len(), "span is out bounds of backing text");
        Span { source, offset, len }
    }
}

impl Debug for FreeSpan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Span")
            .field(&(self.offset..(self.offset+self.len)))
            .finish()
    }
}

#[derive(Clone)]
pub struct Span<'src> {
    source: &'src str,
    offset: u32,
    len: u32,
}

impl<'src> Debug for Span<'src> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let offset = self.offset as usize;
        let len = self.len as usize;
        let (line, _) = self.lines();
        let (column, _) = self.columns();
        f.debug_tuple("Span")
            .field(&line)
            .field(&column)
            .field(&&self.source[offset..][..len])
            .finish()
    }
}

impl<'src> Span<'src> {
    /// Returns on which line does the span start and on which it ends
    ///
    /// Lines are numbered starting from 1. Lines are delimited by a single `\n` (`0x0a`) byte.
    pub fn lines(&self) -> (u32, u32) {
        let bytes = self.source.as_bytes();
        let offset = self.offset as usize;
        let len = self.len as usize;

        let before = bytes[..offset]
            .iter()
            .filter(|byte| **byte == b'\n')
            .count() as u32;
        let within = bytes[offset..][..len]
            .iter()
            .filter(|byte| **byte == b'\n')
            .count() as u32;

        (1 + before, 1 + before + within)
    }

    /// Returns 
    pub fn columns(&self) -> (u32, u32) {
        let column = |line: &[u8]| {
            line.iter().rev().take_while(|byte| **byte != b'\n').count() as u32
        };
        let bytes = self.source.as_bytes();
        let start = self.offset as usize;
        let end = start + (self.len as usize);
        (column(&bytes[..start]), column(&bytes[..end]))
    }

    pub fn slice(&self) -> &str {
        let offset = self.offset as usize;
        let len = self.len as usize;
        &self.source[offset..][..len]
    }
}
