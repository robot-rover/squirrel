use std::ops::Range;

use ariadne::{Cache, Color};

use crate::sq_error::{RsBacktrace, Span, SquirrelError};

use super::value::Value;

pub enum Metamethod {
    Set,
    Get,
    NewSlot,
    DelSlot,
    Add,
    Sub,
    Mul,
    Div,
    Modulo,
    Unm,
    TypeOf,
    Cmp,
    Call,
    Cloned,
    Nexti,
    ToString,
    // TODO
    // Inherited,
    // NewMember,
}

impl Metamethod {
    fn name(&self) -> &'static str {
        match self {
            Metamethod::Set => "_set",
            Metamethod::Get => "_get",
            Metamethod::NewSlot => "_newslot",
            Metamethod::DelSlot => "_delslot",
            Metamethod::Add => "_add",
            Metamethod::Sub => "_sub",
            Metamethod::Mul => "_mul",
            Metamethod::Div => "_div",
            Metamethod::Modulo => "_modulo",
            Metamethod::Unm => "_unm",
            Metamethod::TypeOf => "_typeof",
            Metamethod::Cmp => "_cmp",
            Metamethod::Call => "_call",
            Metamethod::Cloned => "_cloned",
            Metamethod::Nexti => "_nexti",
            Metamethod::ToString => "_tostring",
        }
    }

    fn op_name(&self) -> &'static str {
        match self {
            Metamethod::Set => "=",
            Metamethod::Get => ".",
            Metamethod::NewSlot => "<-",
            Metamethod::DelSlot => "delete",
            Metamethod::Add => "+",
            Metamethod::Sub => "-",
            Metamethod::Mul => "*",
            Metamethod::Div => "/",
            Metamethod::Modulo => "%",
            Metamethod::Unm => "-",
            Metamethod::TypeOf => "typeof",
            Metamethod::Cmp => "<=>",
            Metamethod::Call => "()",
            Metamethod::Cloned => "clone",
            Metamethod::Nexti => "foreach",
            Metamethod::ToString => "tostring()",
        }
    }
}

#[derive(Debug)]
pub struct CallInfo {
    pub func_span: Span,
    pub call_span: Span,
    pub arg_spans: Vec<Span>,
}

pub type ExecResult = Result<(), ExecError>;

#[derive(Debug)]
pub enum ExecError {
    General(Span, String, RsBacktrace),
    UndefinedVariable(Span, RsBacktrace),
    UndefinedField {
        parent_span: Span,
        field_span: Span,
        field: Value,
        bt: RsBacktrace,
    },
    IllegalKeyword(Span, &'static str, RsBacktrace),
    WrongArgCount {
        call_info: CallInfo,
        expected: Range<usize>,
        def_span: Option<Span>,
        bt: RsBacktrace,
    },
    UnhashableType {
        value: Value,
        span: Span,
        bt: RsBacktrace,
    },
    UniterableType {
        value: Value,
        span: Span,
        bt: RsBacktrace,
    },
    IllegalOperation {
        op_name: &'static str,
        op_span: Span,
        lhs: (Value, Span),
        rhs: Option<(Value, Span)>,
        bt: RsBacktrace,
    },
    WrongArgType {
        call_info: CallInfo,
        arg_index: usize,
        expected: &'static str,
        got: Value,
        bt: RsBacktrace,
    },
    WrongThisType {
        call_info: CallInfo,
        expected: &'static str,
        got: Value,
        bt: RsBacktrace,
    },
    AssertionFailed(Span, Option<String>, RsBacktrace),
    NumberParseError(Span, String, RsBacktrace),
    IndexOutOfBounds {
        index: i64,
        len: usize,
        span: Span,
        bt: RsBacktrace,
    },
    MutatingInstantiatedClass {
        class_span: Span,
        assign_span: Span,
        bt: RsBacktrace,
    },
    ExtendingNonClass {
        span: Span,
        non_class: Value,
        bt: RsBacktrace,
    },
    MissingMetamethod {
        obj_span: Span,
        op_span: Span,
        op_name: &'static str,
        mm_name: &'static str,
        bt: RsBacktrace,
    },
    WrongMetamethodReturnType {
        obj_span: Span,
        op_span: Span,
        mm_name: &'static str,
        expected: &'static str,
        got: Value,
        bt: RsBacktrace,
    },
    UncallableType {
        call_info: CallInfo,
        not_fn: Value,
        bt: RsBacktrace,
    },
    WrongIndexType {
        span: Span,
        expected: &'static str,
        got: Value,
        bt: RsBacktrace,
    },
    CannotModifyType {
        span: Span,
        this: Value,
        bt: RsBacktrace,
    },
}

impl ExecError {
    fn get_source_literal<C: Cache<usize>>(cache: &mut C, file_id: usize, span: Span) -> &str {
        cache
            .fetch(&file_id)
            .expect("Illegal file ID")
            .text()
            .get(Range::<usize>::from(span))
            .expect("Span out of range")
    }

    pub fn with_context<C: Cache<usize>>(self, file_id: usize, mut cache: C) -> SquirrelError {
        match self {
            ExecError::UndefinedVariable(ident, bt) => SquirrelError::new(
                file_id,
                ident,
                format!(
                    "Undefined variable: '{}'",
                    ExecError::get_source_literal(&mut cache, file_id, ident),
                ),
                bt,
            ),
            ExecError::IllegalKeyword(span, kw, bt) => SquirrelError::new(
                file_id,
                span,
                format!("The heyword '{}' is not valid in this context", kw),
                bt,
            ),
            ExecError::WrongArgCount {
                call_info,
                expected,
                def_span,
                bt,
            } => {
                let CallInfo {
                    func_span,
                    call_span,
                    arg_spans,
                } = call_info;
                let mut labels = vec![
                    (
                        func_span,
                        if expected.start == expected.end {
                            format!("Function expected {} arguments", expected.start)
                        } else {
                            format!(
                                "Function expected {} to {} arguments",
                                expected.start, expected.end
                            )
                        },
                        Color::Blue,
                    ),
                    (
                        call_span,
                        format!("Call has {} arguments", arg_spans.len()),
                        Color::Red,
                    ),
                ];
                // TODO: Support different file
                if let Some(dspan) = def_span {
                    labels.push((dspan, "Function defined here".to_string(), Color::Green));
                }
                SquirrelError::new_labels(
                    file_id,
                    format!("Function called with incorrect number of arguments"),
                    labels,
                    bt,
                )
            }
            ExecError::UndefinedField {
                parent_span,
                field_span,
                field,
                bt,
            } => SquirrelError::new_labels(
                file_id,
                format!("Undefined field: '{}'", field),
                vec![
                    (parent_span, "Parent object".to_string(), Color::Blue),
                    (field_span, "Field".to_string(), Color::Red),
                ],
                bt,
            ),
            ExecError::UnhashableType { value, span, bt } => SquirrelError::new(
                file_id,
                span,
                format!("A value of type '{}' is not hashable", value.type_str()),
                bt,
            ),
            ExecError::IllegalOperation {
                op_name,
                op_span,
                lhs,
                rhs,
                bt,
            } => {
                let is_binary = rhs.is_some();
                let mut labels = vec![
                    (lhs.1, format!("Operand of type '{}'", lhs.0), Color::Red),
                    (op_span, format!("Operator"), Color::Blue),
                ];
                if let Some(rhs) = rhs {
                    labels.push((rhs.1, format!("Operand of type '{}'", rhs.0), Color::Red));
                }
                SquirrelError::new_labels(
                    file_id,
                    format!(
                        "Illegal type{} for '{}' operator",
                        if is_binary { "s" } else { "" },
                        op_name
                    ),
                    labels,
                    bt,
                )
            }
            ExecError::WrongArgType {
                call_info,
                arg_index,
                expected,
                got,
                bt,
            } => {
                let CallInfo {
                    func_span,
                    call_span,
                    arg_spans,
                } = call_info;
                SquirrelError::new_labels(
                    file_id,
                    format!("Argument {} has the wrong type", arg_index),
                    vec![
                        (
                            call_span,
                            format!("Expected type: '{}'", expected),
                            Color::Red,
                        ),
                        (call_span, format!("Got type: '{}'", got), Color::Red),
                    ],
                    bt,
                )
            }
            ExecError::AssertionFailed(span, message, bt) => SquirrelError::new(
                file_id,
                span,
                if let Some(message) = message {
                    format!("Assertion failed: {}", message)
                } else {
                    "Assertion failed".to_string()
                },
                bt,
            ),
            ExecError::NumberParseError(span, message, bt) => SquirrelError::new(
                file_id,
                span,
                format!("Failed to parse number: {}", message),
                bt,
            ),
            ExecError::IndexOutOfBounds {
                index,
                len,
                span,
                bt,
            } => SquirrelError::new(
                file_id,
                span,
                format!("Index '{}' out of bounds for length '{}'", index, len),
                bt,
            ),
            ExecError::WrongThisType {
                call_info,
                expected,
                got,
                bt,
            } => SquirrelError::new_labels(
                file_id,
                format!(
                    "Expected 'this' to be of type '{}', got '{}'",
                    expected, got
                ),
                vec![
                    (call_info.call_span, "This call".to_string(), Color::Blue),
                    (call_info.func_span, "This function".to_string(), Color::Red),
                ],
                bt,
            ),
            ExecError::UniterableType { span, value, bt } => SquirrelError::new(
                file_id,
                span,
                format!("A value of type '{}' is not iterable", value.type_str()),
                bt,
            ),
            ExecError::MutatingInstantiatedClass {
                class_span,
                assign_span,
                bt,
            } => SquirrelError::new_labels(
                file_id,
                "Cannot mutate an instantiated class".to_string(),
                vec![
                    (class_span, "Class".to_string(), Color::Blue),
                    (assign_span, "Assignment".to_string(), Color::Red),
                ],
                bt,
            ),
            ExecError::ExtendingNonClass {
                span,
                non_class,
                bt,
            } => SquirrelError::new_labels(
                file_id,
                "Cannot extend a non-class".to_string(),
                vec![(
                    span,
                    format!("Value (of type {})", non_class.type_str()),
                    Color::Red,
                )],
                bt,
            ),
            ExecError::MissingMetamethod {
                obj_span,
                op_span,
                op_name,
                mm_name,
                bt,
            } => SquirrelError::new_labels(
                file_id,
                format!(
                    "Could not find metamethod '{}', which is needed to perform operation '{}'",
                    mm_name, op_name
                ),
                vec![
                    (
                        obj_span,
                        format!("Object (missing '{}')", mm_name),
                        Color::Red,
                    ),
                    (op_span, "Operator".to_string(), Color::Blue),
                ],
                bt,
            ),
            ExecError::General(span, message, bt) => SquirrelError::new(file_id, span, message, bt),
            ExecError::WrongMetamethodReturnType {
                obj_span,
                op_span,
                mm_name,
                expected,
                got,
                bt,
            } => SquirrelError::new_labels(
                file_id,
                format!(
                    "Metamethod '{}' must return '{}', got '{}'",
                    mm_name,
                    expected,
                    got.type_str()
                ),
                vec![
                    (obj_span, format!("Object"), Color::Red),
                    (op_span, format!("Operator"), Color::Blue),
                ],
                bt,
            ),
            ExecError::UncallableType {
                call_info,
                not_fn,
                bt,
            } => SquirrelError::new(
                file_id,
                call_info.func_span,
                format!("Value of type '{}' is not callable", not_fn.type_str()),
                bt,
            ),
            ExecError::WrongIndexType {
                span,
                expected,
                got,
                bt,
            } => SquirrelError::new(
                file_id,
                span,
                format!(
                    "Expected index of type '{}', got '{}'",
                    expected,
                    got.type_str()
                ),
                bt,
            ),
            ExecError::CannotModifyType { span, this, bt } => SquirrelError::new(
                file_id,
                span,
                format!("Cannot modify the type '{}'", this.type_str()),
                bt,
            ),
        }
    }
}
