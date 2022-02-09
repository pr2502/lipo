use std::any::Any;
use std::fmt::{Debug, Display};
use std::io::Write;

use codespan_reporting::diagnostic;

use crate::span::FreeSpan;


/// User-facing reporting for Diagnostics
pub trait Report: Debug {
    /// Reports the error to stdout with the default settings
    fn report(&self, source: &str);
}

pub trait Diagnostic: Any + Debug {
    /// Diagnostic severity level
    ///
    /// See [`Severity`].
    fn severity(&self) -> Severity;

    /// Concise description shown in the header of the report
    fn message(&self) -> String;

    /// Optional labeled snippets of code with further details
    ///
    /// Usually there will be one primary Label and zero or more secondary
    /// Labels.
    fn labels(&self) -> Vec<Label> {
        Vec::new()
    }

    /// Further details of the diagnostic
    ///
    /// Usually start with `note: ` or `help: `.
    fn notes(&self) -> Vec<String> {
        Vec::new()
    }
}


impl<D> Report for D
where
    D: Diagnostic,
{
    fn report(&self, source: &str) {
        use codespan_reporting::files::SimpleFile;
        use codespan_reporting::term;
        use diagnostic::Diagnostic;
        use termcolor::{ColorChoice, StandardStream};

        let file = SimpleFile::new("<script>", source);

        let diagnostic = Diagnostic {
            severity: self.severity().into_codespan_severity(),
            code: None,
            message: self.message(),
            labels: self
                .labels()
                .into_iter()
                .map(Label::into_codespan_label)
                .collect(),
            notes: self.notes(),
        };

        let stdout = StandardStream::stdout(ColorChoice::Auto);
        let mut stdout = stdout.lock();
        writeln!(stdout).unwrap();
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
    /// Create a primary label for the diagnostic, there should be exactly one
    /// per Report
    pub fn primary(span: impl AsRef<FreeSpan>, message: impl Display) -> Label {
        Label {
            span: *span.as_ref(),
            message: message.to_string(),
            style: diagnostic::LabelStyle::Primary,
        }
    }

    /// Create a secondary label for the diagnostic, there can be any number per
    /// Report
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
