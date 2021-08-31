use std::fmt;


/// Debug formatting for types that require the original source code.
pub trait SourceDebug {
    fn fmt(&self, source: &str, f: &mut fmt::Formatter<'_>) -> fmt::Result;

    fn wrap<'src>(&'src self, source: &'src str) -> SourceWrap<'src, Self>
    where
        Self: Sized,
    {
        SourceWrap {
            source,
            inner: self,
        }
    }
}

pub struct SourceWrap<'src, T> {
    source: &'src str,
    inner: &'src T,
}

impl<'src, T> fmt::Debug for SourceWrap<'src, T>
where
    T: SourceDebug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        SourceDebug::fmt(self.inner, self.source, f)
    }
}
