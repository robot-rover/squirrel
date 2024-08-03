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
pub struct RsBacktrace(Rc<Backtrace>);

impl RsBacktrace {
    pub fn new() -> Self {
        RsBacktrace(Rc::new(if *SQ_BACKTRACE {
            Backtrace::force_capture()
        } else {
            Backtrace::disabled()
        }))
    }

    pub fn empty() -> Self {
        RsBacktrace(Rc::new(Backtrace::disabled()))
    }
}

impl Display for RsBacktrace {
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
pub struct SqStacktrace(Vec<SqTraceFrame>);
#[derive(Debug, Clone)]
pub struct SqTraceFrame {
    pub file: String,
    pub line: u32,
}

impl SqStacktrace {
    pub fn new(trace: Vec<SqTraceFrame>) -> Self {
        SqStacktrace(trace)
    }

    pub fn iter(&self) -> impl Iterator<Item = &SqTraceFrame> {
        self.0.iter()
    }
}

#[derive(Debug, Clone)]
pub struct SquirrelError {
    file_id: usize,
    message: String,
    labels: Vec<(Span, String, Color)>,
    backtrace: RsBacktrace,
    stacktrace: SqStacktrace,
}

impl SquirrelError {
    pub fn new(file_id: usize, token: Span, message: String, backtrace: RsBacktrace) -> Self {
        Self::new_st(
            file_id,
            token,
            message,
            backtrace,
            SqStacktrace::new(Vec::new()),
        )
    }

    pub fn new_st(
        file_id: usize,
        token: Span,
        message: String,
        backtrace: RsBacktrace,
        stacktrace: SqStacktrace,
    ) -> Self {
        Self {
            file_id,
            message,
            labels: vec![(token, "Here".to_string(), Color::Red)],
            backtrace,
            stacktrace,
        }
    }

    pub fn new_labels(
        file_id: usize,
        message: String,
        labels: Vec<(Span, String, Color)>,
        backtrace: RsBacktrace,
    ) -> Self {
        Self::new_labels_st(
            file_id,
            message,
            labels,
            backtrace,
            SqStacktrace::new(Vec::new()),
        )
    }

    pub fn new_labels_st(
        file_id: usize,
        message: String,
        labels: Vec<(Span, String, Color)>,
        backtrace: RsBacktrace,
        stacktrace: SqStacktrace,
    ) -> Self {
        Self {
            file_id,
            message,
            labels,
            backtrace,
            stacktrace,
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
