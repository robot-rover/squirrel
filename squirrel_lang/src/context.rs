use std::{
    any::{type_name, type_name_of_val},
    backtrace::{self, Backtrace, BacktraceStatus},
    cmp::{max, min},
    env,
    fmt::{self, Display},
    io,
    ops::{BitOr, Range},
    rc::Rc,
};

use ariadne::{Cache, Color, Label, Report, ReportKind, Source};
use once_cell::sync::Lazy;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    // TODO: Should really get rid of this
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

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Self {
        span.start..span.end
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

    pub fn empty() -> Self {
        SqBacktrace(Rc::new(Backtrace::disabled()))
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
    file_id: usize,
    message: String,
    labels: Vec<(Span, String, Color)>,
    backtrace: SqBacktrace,
}

impl SquirrelError {
    pub fn new(file_id: usize, token: Span, message: String) -> Self {
        Self::new_bt(file_id, token, message, SqBacktrace::empty())
    }

    pub fn new_bt(file_id: usize, token: Span, message: String, backtrace: SqBacktrace) -> Self {
        Self {
            file_id,
            message,
            labels: vec![(token, "Here".to_string(), Color::Red)],
            backtrace,
        }
    }

    pub fn new_labels(file_id: usize, message: String, labels: Vec<(Span, String, Color)>) -> Self {
        Self::new_labels_bt(file_id, message, labels, SqBacktrace::empty())
    }

    pub fn new_labels_bt(
        file_id: usize,
        message: String,
        labels: Vec<(Span, String, Color)>,
        backtrace: SqBacktrace,
    ) -> Self {
        Self {
            file_id,
            message,
            labels,
            backtrace,
        }
    }

    pub fn render<C: Cache<usize>>(&self, cache: C) -> SquirrelErrorRendered {
        let mut report_vec = Vec::new();
        Report::build(ReportKind::Error, self.file_id, self.labels[0].0.start)
            .with_message(self.message.as_str())
            .with_labels(self.labels.iter().map(|(span, label, color)| {
                Label::new((self.file_id, (*span).into()))
                    .with_message(label.as_str())
                    .with_color(*color)
            }))
            .finish()
            .write(cache, &mut report_vec)
            .unwrap();
        SquirrelErrorRendered(format!(
            "{}{}",
            String::from_utf8(report_vec).expect("Unable to convert report to utf-8"),
            &self.backtrace
        ))
    }
}

#[derive(Clone)]
pub struct SquirrelErrorRendered(String);

impl Display for SquirrelErrorRendered {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub fn render_report<'a>(
    report: &'a Report<'a, ContextSpan<'a>>,
    file_name: &'a str,
    source: &'a str,
) -> String {
    let mut report_bytes = Vec::new();
    report
        .write((file_name, Source::from(source)), &mut report_bytes)
        .expect("Unable to write report");
    String::from_utf8(report_bytes).expect("Unable to convert report to utf-8")
}
