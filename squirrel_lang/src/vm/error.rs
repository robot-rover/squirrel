use std::ops::Range;

use crate::context::{Span, SquirrelError};

use super::value::Value;

#[derive(Debug)]
pub struct CallInfo {
    func_span: Span,
    call_span: Span,
    arg_spans: Vec<Span>,
}

pub type ExecResult = Result<(), ExecError>;

#[derive(Debug)]
pub enum ExecError {
    General(Span, String),
    UndefinedVariable(Span, String),
    UndefinedField {
        parent_span: Span,
        field_span: Span,
        field: Value,
    },
    IllegalKeyword(Span, String),
    WrongArgCount {
        call_info: CallInfo,
        expected: Range<usize>,
        got: usize,
        definition_span: Option<Span>,
    },
    UnhashableType {
        kind: String,
        span: Span,
    },
    UniterableType {
        kind: String,
        span: Span,
    },
    IllegalOperation {
        op: String,
        op_span: Span,
        lhs: (String, Span),
        rhs: Option<(String, Span)>,
    },
    WrongArgType {
        call_info: CallInfo,
        arg_index: usize,
        expected: String,
        got: String,
    },
    WrongThisType {
        call_info: CallInfo,
        expected: String,
        got: String,
    },
    AssertionFailed(Span, Option<String>),
    NumberParseError(Span, String),
    IndexOutOfBounds {
        index: i64,
        len: usize,
        span: Span,
    },
    MutatingInstantiatedClass(Span),
    ExtendingNonClass {
        span: Span,
        non_class_ty: String,
    },
    MissingMetamethod {
        obj_span: Span,
        op_span: Span,
        op: String,
        mm_name: String,
    },
    WrongMetamethodReturnType { obj_span: Span, op_span: Span, mm_name: String, expected: String, got: String },
}

impl ExecError {
    // fn with_context(self, file_name: String) -> SquirrelError {
    //     match self {
    //         ExecError::UndefinedVariable(ident, bt) => SquirrelError::new(
    //             file_name,
    //             ident.1,
    //             format!("Undefined variable: '{}'", ident.0),
    //             bt,
    //         ),
    //         ExecError::IllegalKeyword(span, bt) => SquirrelError::new(
    //             file_name,
    //             span,
    //             format!("Keyword is not valid in this context"),
    //             bt,
    //         ),
    //         ExecError::WrongArgCount {
    //             call_info,
    //             expected,
    //             got,
    //             definition_span,
    //             bt,
    //         } => {
    //             let CallInfo {
    //                 func_span,
    //                 call_span,
    //             } = call_info;
    //             let mut labels = vec![
    //                 (
    //                     func_span,
    //                     if expected.start == expected.end {
    //                         format!("Function expected {} arguments", expected.start)
    //                     } else {
    //                         format!(
    //                             "Function expected {} to {} arguments",
    //                             expected.start, expected.end
    //                         )
    //                     },
    //                     Color::Blue,
    //                 ),
    //                 (call_span, format!("Call has {} arguments", got), Color::Red),
    //             ];
    //             if let Some(dspan) = definition_span {
    //                 labels.push((dspan, "Function defined here".to_string(), Color::Green));
    //             }
    //             SquirrelError::new_labels(
    //                 file_name,
    //                 format!("Function called with incorrect number of arguments"),
    //                 labels,
    //                 bt,
    //             )
    //         }
    //         ExecError::UndefinedField(span, val, bt) => {
    //             SquirrelError::new(file_name, span, format!("Undefined field: {}", val), bt)
    //         }
    //         ExecError::UnhashableType { kind, span, bt } => SquirrelError::new(
    //             file_name,
    //             span,
    //             format!("A value of type '{}' is not hashable", kind),
    //             bt,
    //         ),
    //         ExecError::IllegalOperation {
    //             op,
    //             op_span,
    //             lhs,
    //             rhs,
    //             bt,
    //         } => {
    //             let is_binary = rhs.is_some();
    //             let mut labels = vec![
    //                 (lhs.1, format!("Operand of type '{}'", lhs.0), Color::Red),
    //                 (op_span, format!("Operator"), Color::Blue),
    //             ];
    //             if let Some(rhs) = rhs {
    //                 labels.push((rhs.1, format!("Operand of type '{}'", rhs.0), Color::Red));
    //             }
    //             SquirrelError::new_labels(
    //                 file_name,
    //                 format!(
    //                     "Illegal type{} for '{}' operator",
    //                     if is_binary { "s" } else { "" },
    //                     op
    //                 ),
    //                 labels,
    //                 bt,
    //             )
    //         }
    //         ExecError::WrongArgType {
    //             call_info,
    //             arg_index,
    //             expected,
    //             got,
    //             bt,
    //         } => {
    //             let CallInfo {
    //                 func_span,
    //                 call_span,
    //             } = call_info;
    //             SquirrelError::new_labels(
    //                 file_name,
    //                 format!("Argument {} has the wrong type", arg_index),
    //                 vec![
    //                     (
    //                         call_span,
    //                         format!("Expected type: '{}'", expected),
    //                         Color::Red,
    //                     ),
    //                     (call_span, format!("Got type: '{}'", got), Color::Red),
    //                 ],
    //                 bt,
    //             )
    //         }
    //         ExecError::AssertionFailed(span, message, bt) => SquirrelError::new(
    //             file_name,
    //             span,
    //             if let Some(message) = message {
    //                 format!("Assertion failed: {}", message)
    //             } else {
    //                 "Assertion failed".to_string()
    //             },
    //             bt,
    //         ),
    //         ExecError::NumberParseError(span, message, bt) => SquirrelError::new(
    //             file_name,
    //             span,
    //             format!("Failed to parse number: {}", message),
    //             bt,
    //         ),
    //         ExecError::IndexOutOfBounds {
    //             index,
    //             len,
    //             span,
    //             bt,
    //         } => SquirrelError::new(
    //             file_name,
    //             span,
    //             format!("Index '{}' out of bounds for length '{}'", index, len),
    //             bt,
    //         ),
    //         ExecError::WrongThisType {
    //             call_info,
    //             expected,
    //             got,
    //             bt,
    //         } => SquirrelError::new_labels(
    //             file_name,
    //             format!(
    //                 "Expected 'this' to be of type '{}', got '{}'",
    //                 expected, got
    //             ),
    //             vec![
    //                 (call_info.call_span, "This call".to_string(), Color::Blue),
    //                 (call_info.func_span, "This function".to_string(), Color::Red),
    //             ],
    //             bt,
    //         ),
    //         ExecError::UniterableType { kind, span, bt } => SquirrelError::new(
    //             file_name,
    //             span,
    //             format!("A value of type '{}' is not iterable", kind),
    //             bt,
    //         ),
    //         ExecError::MutatingInstantiatedClass(span, bt) => SquirrelError::new(
    //             file_name, span, "Cannot mutate an instantiated class".to_string(), bt
    //         ),
    //         ExecError::ExtendingNonClass(span, bt) => SquirrelError::new(
    //             file_name, span, "Cannot extend a non-class".to_string(), bt
    //         ),
    //         ExecError::MissingMetamethod { obj_span, op_span, name, op, bt } => SquirrelError::new_labels(
    //             file_name,
    //             format!("Could not find metamethod '{}', which is needed to perform operation '{}'", name, op),
    //             vec![
    //                 (obj_span, format!("Object (missing '{}')", name), Color::Red),
    //                 (op_span, "Operator".to_string(), Color::Blue),
    //             ],
    //             bt,
    //         ),
    //         ExecError::General(span, message, bt) => SquirrelError::new(
    //             file_name,
    //             span,
    //             message,
    //             bt
    //         ),
    //         ExecError::WrongMetamethodReturnType { obj_span, op_span, mm_name, expected, got, bt } => SquirrelError::new_labels(
    //             file_name,
    //             format!("Metamethod '{}' must return '{}', got '{}'", mm_name, expected, got),
    //             vec![
    //                 (obj_span, format!("Object"), Color::Red),
    //                 (op_span, format!("Operator"), Color::Blue),
    //             ],
    //             bt
    //         ),
    //     }
    // }
}
