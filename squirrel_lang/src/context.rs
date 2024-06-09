use std::{
    backtrace::{Backtrace, BacktraceStatus},
    cmp::{max, min},
    env,
    fmt::{self, Display},
    ops::{BitOr, Range},
    rc::Rc,
};

use ariadne::{Label, Report, ReportKind, Source};
use once_cell::sync::Lazy;

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn empty() -> Self {
        Self { start: 0, end: 0 }
    }

    pub fn in_file(self, file_name: &str) -> ContextSpan {
        ContextSpan {
            file_name,
            span: self,
        }
    }
}

impl BitOr for Span {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self {
        (min(self.start, rhs.start)..max(self.end, rhs.end)).into()
    }
}

impl From<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Self {
        Self {
            start: range.start,
            end: range.end,
        }
    }
}

impl Into<Range<usize>> for Span {
    fn into(self) -> Range<usize> {
        self.start..self.end
    }
}

impl ariadne::Span for Span {
    type SourceId = ();

    fn source(&self) -> &Self::SourceId {
        &()
    }

    fn start(&self) -> usize {
        self.start
    }

    fn end(&self) -> usize {
        self.end
    }
}

pub struct ContextSpan<'a> {
    pub file_name: &'a str,
    pub span: Span,
}

impl<'a> ariadne::Span for ContextSpan<'a> {
    type SourceId = &'a str;

    fn source(&self) -> &Self::SourceId {
        &self.file_name
    }

    fn start(&self) -> usize {
        self.span.start
    }

    fn end(&self) -> usize {
        self.span.end
    }
}

static SQ_BACKTRACE: Lazy<bool> = Lazy::new(|| {
    env::var("SQ_BACKTRACE")
        .map(|val| val == "1")
        .unwrap_or(false)
});

#[derive(Debug, Clone)]
pub struct SqBacktrace(Rc<Backtrace>);

impl SqBacktrace {
    pub fn new() -> Self {
        SqBacktrace(Rc::new(if *SQ_BACKTRACE {
            Backtrace::force_capture()
        } else {
            Backtrace::disabled()
        }))
    }
}

impl Display for SqBacktrace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0.status() == BacktraceStatus::Captured {
            let bt_string = format!("{}", self.0);
            for line in bt_string.lines() {
                if line.contains("./") || line.contains("squirrel") {
                    writeln!(f, "{}", line)?;
                }
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct SquirrelError {
    pub file_name: String,
    pub token: Span,
    pub message: String,
    pub backtrace: SqBacktrace,
}

impl SquirrelError {
    pub fn new(file_name: String, token: Span, message: String, backtrace: SqBacktrace) -> Self {
        Self {
            file_name,
            token,
            message,
            backtrace,
        }
    }

    pub fn write(&self, f: &mut fmt::Formatter<'_>, source: &str) -> fmt::Result {
        let mut report_vec = Vec::new();
        Report::build(ReportKind::Error, self.file_name.as_str(), self.token.start)
            .with_message(self.message.as_str())
            .with_label(
                Label::new((self.file_name.as_str(), self.token.into()))
                    .with_message("Here")
                    .with_color(ariadne::Color::Red),
            )
            .finish()
            .write(
                (self.file_name.as_str(), Source::from(source)),
                &mut report_vec,
            )
            .unwrap();
        write!(
            f,
            "{}{}",
            String::from_utf8(report_vec).expect("Unable to convert report to utf-8"),
            &self.backtrace
        )?;
        Ok(())
    }
}

pub struct DisplayReport<'a> {
    report: &'a Report<'a, ContextSpan<'a>>,
    file_name: &'a str,
    source: &'a str,
}

impl DisplayReport<'_> {
    pub fn new<'a>(
        report: &'a Report<'a, ContextSpan<'a>>,
        file_name: &'a str,
        source: &'a str,
    ) -> DisplayReport<'a> {
        DisplayReport {
            report,
            file_name,
            source,
        }
    }
}

impl<'a> Display for DisplayReport<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut report_bytes = Vec::new();
        // TODO: Don't clone
        self.report
            .write(
                (self.file_name, Source::from(self.source)),
                &mut report_bytes,
            )
            .map_err(|_| fmt::Error)?;
        write!(
            f,
            "{}",
            String::from_utf8(report_bytes).map_err(|_| fmt::Error)?
        )
    }
}

#[derive(Debug, Clone)]
pub struct SquirrelErrorContext<'s>(SquirrelError, &'s str);

impl Display for SquirrelErrorContext<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.write(f, self.1)?;
        Ok(())
    }
}

pub trait IntoSquirrelErrorContext {
    fn with_context<'s>(self, source: &'s str) -> SquirrelErrorContext<'s>;
}

impl IntoSquirrelErrorContext for SquirrelError {
    fn with_context<'s>(self, source: &'s str) -> SquirrelErrorContext<'s> {
        SquirrelErrorContext(self, source)
    }
}
