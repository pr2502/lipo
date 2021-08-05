
#[derive(Clone, Copy, Default)]
pub struct FreeSpan {
    pub offset: u32,
    pub len: u32,
}

impl FreeSpan {
    pub fn anchor<'src>(self, source: &'src str) -> Span<'src> {
        let FreeSpan { offset, len } = self;
        assert!((offset + len) as usize <= source.len(), "span is out bounds of backing text");
        Span { source, offset, len }
    }
}

#[derive(Clone)]
pub struct Span<'src> {
    source: &'src str,
    offset: u32,
    len: u32,
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
}
