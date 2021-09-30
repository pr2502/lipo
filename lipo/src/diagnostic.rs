use crate::span::FreeSpan;
use codespan_reporting::diagnostic;
use std::any::Any;
use std::fmt::{Debug, Display};
use std::io::Write;


/// User-facing reporting for Diagnostics
pub trait Report: Debug {
    /// Reports the error to stdout with the default settings
    fn report(&self, source: &str);
}

pub trait Diagnostic: Any + Debug {
    fn severity(&self) -> Severity;

    fn message(&self) -> String;

    fn labels(&self) -> Vec<Label>;

    fn notes(&self) -> Vec<String>;
}


impl<D> Report for D
where
    D: Diagnostic,
{
    fn report(&self, source: &str) {
        use codespan_reporting::files::SimpleFile;
        use codespan_reporting::term;
        use diagnostic::Diagnostic;
        use termcolor::{StandardStream, ColorChoice};

        let file = SimpleFile::new("<script>", source);

        let diagnostic = Diagnostic {
            severity: self.severity().into_codespan_severity(),
            code: None,
            message: self.message(),
            labels: self.labels().into_iter().map(Label::into_codespan_label).collect(),
            notes: self.notes(),
        };

        let stdout = StandardStream::stdout(ColorChoice::Auto);
        let mut stdout = stdout.lock();
        writeln!(&mut stdout).unwrap();
        term::emit(&mut stdout, &term::Config::default(), &file, &diagnostic).unwrap();
    }
}


pub enum Severity {
    Error,
}

pub struct Label {
    span: FreeSpan,
    message: String,
    style: diagnostic::LabelStyle,
}

impl Severity {
    fn into_codespan_severity(self) -> diagnostic::Severity {
        match self {
            Severity::Error => diagnostic::Severity::Error,
        }
    }
}

impl Label {
    /// Create a primary label for the diagnostic, there should be exactly one per Report
    pub fn primary(span: impl AsRef<FreeSpan>, message: impl Display) -> Label {
        Label {
            span: *span.as_ref(),
            message: message.to_string(),
            style: diagnostic::LabelStyle::Primary,
        }
    }

    /// Create a secondary label for the diagnostic, there can be any number per Report
    pub fn secondary(span: impl AsRef<FreeSpan>, message: impl Display) -> Label {
        Label {
            span: *span.as_ref(),
            message: message.to_string(),
            style: diagnostic::LabelStyle::Secondary,
        }
    }

    fn into_codespan_label(self) -> diagnostic::Label<()> {
        diagnostic::Label {
            style: self.style,
            file_id: (),
            range: self.span.range(),
            message: self.message,
        }
    }
}
