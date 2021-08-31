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


mod util {
    use std::fmt::{self, Write};


    struct PadAdapter<'a, 'b: 'a> {
        fmt: &'a mut fmt::Formatter<'b>,
        on_newline: bool,
    }

    impl<'a, 'b: 'a> PadAdapter<'a, 'b> {
        fn wrap(fmt: &'a mut fmt::Formatter<'b>) -> PadAdapter<'a, 'b> {
            PadAdapter {
                fmt,
                on_newline: true,
            }
        }
    }

    impl fmt::Write for PadAdapter<'_, '_> {
        fn write_str(&mut self, mut s: &str) -> fmt::Result {
            while !s.is_empty() {
                if self.on_newline {
                    self.fmt.write_str("    ")?;
                }

                let split = match s.find('\n') {
                    Some(pos) => {
                        self.on_newline = true;
                        pos + 1
                    }
                    None => {
                        self.on_newline = false;
                        s.len()
                    }
                };
                self.fmt.write_str(&s[..split])?;
                s = &s[split..];
            }

            Ok(())
        }
    }


    #[must_use = "must eventually call `finish()` on Debug builders"]
    pub struct DebugSexpr<'a, 'b: 'a> {
        fmt: &'a mut fmt::Formatter<'b>,
        result: fmt::Result,
        fields: usize,
    }

    pub fn debug_sexpr<'a, 'b>(
        fmt: &'a mut fmt::Formatter<'b>,
        name: &'a str,
    ) -> DebugSexpr<'a, 'b> {
        let result = fmt.write_str("(")
            .and_then(|_| fmt.write_str(name));
        DebugSexpr { fmt, result, fields: 0 }
    }

    impl<'a, 'b: 'a> DebugSexpr<'a, 'b> {
        pub fn opt_kw_atom<D: fmt::Debug>(&mut self, kw: &str, value: Option<D>) -> &mut Self {
            if let Some(value) = value {
                self.kw_atom(kw, &value);
            }
            self
        }

        pub fn kw_atom(&mut self, kw: &str, value: &dyn fmt::Debug) -> &mut Self {
            self.result = self.result
                .and_then(|_| self.fmt.write_str(":"))
                .and_then(|_| self.fmt.write_str(kw))
                .and_then(|_| self.fmt.write_str(" "));
            self.atom(value);
            self
        }

        pub fn atom(&mut self, value: &dyn fmt::Debug) -> &mut Self {
            self.result = self.result.and_then(|_| {
                if self.is_pretty() {
                    self.fmt.write_str("\n")?;
                    let mut writer = PadAdapter::wrap(&mut self.fmt);
                    write!(&mut writer, "{:#?}", value)
                } else {
                    self.fmt.write_str(" ")?;
                    value.fmt(self.fmt)
                }
            });
            self.fields += 1;
            self
        }

        pub fn finish(&mut self) -> fmt::Result {
            self.result.and_then(|_| self.fmt.write_str(")"))
        }

        fn is_pretty(&self) -> bool {
            self.fmt.alternate()
        }
    }
}

pub use util::debug_sexpr;
