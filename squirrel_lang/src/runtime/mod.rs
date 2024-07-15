use hashbrown::HashMap;
use std::{
    borrow::Borrow,
    cell::RefCell,
    io,
    ops::{Deref, Range},
    rc::Rc,
};

use ariadne::Color;
use value::{Closure, Object, Value};

use crate::{
    context::{Span, SqBacktrace, SquirrelError},
    parser::ast::{Ident, Literal},
};

pub mod argparse;
pub mod builtins;
pub mod value;
pub mod walker;

macro_rules! rc_hash_eq {
    ($t:ty, $value:tt, $ptr:tt) => {
        impl PartialEq for $t {
            fn eq(&self, other: &Self) -> bool {
                std::ptr::eq($ptr::as_ptr(&self.0), $ptr::as_ptr(&other.0))
            }
        }
        impl Eq for $t {}
        impl std::hash::Hash for $t {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                $ptr::as_ptr(&self.0).hash(state)
            }
        }

        impl From<$t> for Value {
            fn from(rc: $t) -> Self {
                Value::$value(rc)
            }
        }
    };
}

type ExprResult = Result<Value, ExecError>;

#[derive(Debug)]
pub enum ExecError {
    UndefinedVariable(Ident, SqBacktrace),
    UndefinedField(Span, Value, SqBacktrace),
    IllegalKeyword(Span, SqBacktrace),
    WrongArgCount {
        call_info: CallInfo,
        expected: Range<usize>,
        got: usize,
        definition_span: Option<Span>,
        bt: SqBacktrace,
    },
    UnhashableType {
        kind: String,
        span: Span,
        bt: SqBacktrace,
    },
    UniterableType {
        kind: String,
        span: Span,
        bt: SqBacktrace,
    },
    IllegalOperation {
        op: String,
        op_span: Span,
        lhs: (String, Span),
        rhs: Option<(String, Span)>,
        bt: SqBacktrace,
    },
    WrongArgType {
        call_info: CallInfo,
        arg_index: usize,
        expected: String,
        got: String,
        bt: SqBacktrace,
    },
    WrongThisType {
        call_info: CallInfo,
        expected: String,
        got: String,
        bt: SqBacktrace,
    },
    AssertionFailed(Span, Option<String>, SqBacktrace),
    NumberParseError(Span, String, SqBacktrace),
    IndexOutOfBounds {
        index: i64,
        len: usize,
        span: Span,
        bt: SqBacktrace,
    },
    MutatingInstantiatedClass(Span, SqBacktrace),
    ExtendingNonClass(Span, SqBacktrace),
    MissingMetamethod {
        span: Span,
        name: String,
        op: String,
        bt: SqBacktrace,
    },
}

macro_rules! variant_constructor {
    ($variant:ident $name:ident($($arg:ident : $argty:ty),*)) => {
        pub fn $name($($arg : $argty),*) -> Self {
            ExecError::$variant($($arg),*, SqBacktrace::new())
        }
    };
    ($variant:ident $name:ident{$($arg:ident : $argty:ty),*}) => {
        pub fn $name($($arg : $argty),*) -> Self {
            ExecError::$variant { $($arg),*, bt: SqBacktrace::new() }
        }
    };
}

#[derive(Debug, Clone)]
pub struct CallInfo {
    func_span: Span,
    call_span: Span,
}

impl ExecError {
    variant_constructor!(UndefinedVariable undefined_variable(ident: Ident));
    variant_constructor!(UndefinedField undefined_field(span: Span, value: Value));
    variant_constructor!(IllegalKeyword illegal_keyword(span: Span));
    variant_constructor!(WrongArgCount wrong_arg_count { call_info: CallInfo, expected: Range<usize>, got: usize, definition_span: Option<Span> });
    variant_constructor!(UnhashableType unhashable_type { kind: String, span: Span });
    variant_constructor!(UniterableType uniterable_type { kind: String, span: Span });
    variant_constructor!(WrongArgType wrong_arg_type { call_info: CallInfo, arg_index: usize, expected: String, got: String });
    variant_constructor!(WrongThisType wrong_this_type { call_info: CallInfo, expected: String, got: String });
    variant_constructor!(AssertionFailed assertion_failed(span: Span, message: Option<String>));
    variant_constructor!(NumberParseError number_parse_error(span: Span, message: String));
    variant_constructor!(IndexOutOfBounds index_out_of_bounds { index: i64, len: usize, span: Span });
    variant_constructor!(MutatingInstantiatedClass mutating_instantiated_class(span: Span));
    variant_constructor!(ExtendingNonClass extending_non_class(span: Span));
    variant_constructor!(MissingMetamethod missing_metamethod { span: Span, name: String, op: String });

    fn illegal_binary_op(
        op: &str,
        op_span: Span,
        lhs: (Value, Span),
        rhs: (Value, Span),
    ) -> ExecError {
        ExecError::IllegalOperation {
            op: op.to_string(),
            op_span,
            lhs: (lhs.0.type_str().to_string(), lhs.1),
            rhs: Some((rhs.0.type_str().to_string(), rhs.1)),
            bt: SqBacktrace::new(),
        }
    }

    fn illegal_unary_op(op: &str, op_span: Span, data: (&Value, Span)) -> ExecError {
        ExecError::IllegalOperation {
            op: op.to_string(),
            op_span,
            lhs: (data.0.type_str().to_string(), data.1),
            rhs: None,
            bt: SqBacktrace::new(),
        }
    }

    fn with_context(self, file_name: String) -> SquirrelError {
        match self {
            ExecError::UndefinedVariable(ident, bt) => SquirrelError::new(
                file_name,
                ident.1,
                format!("Undefined variable: '{}'", ident.0),
                bt,
            ),
            ExecError::IllegalKeyword(span, bt) => SquirrelError::new(
                file_name,
                span,
                format!("Keyword is not valid in this context"),
                bt,
            ),
            ExecError::WrongArgCount {
                call_info,
                expected,
                got,
                definition_span,
                bt,
            } => {
                let CallInfo {
                    func_span,
                    call_span,
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
                    (call_span, format!("Call has {} arguments", got), Color::Red),
                ];
                if let Some(dspan) = definition_span {
                    labels.push((dspan, "Function defined here".to_string(), Color::Green));
                }
                SquirrelError::new_labels(
                    file_name,
                    format!("Function called with incorrect number of arguments"),
                    labels,
                    bt,
                )
            }
            ExecError::UndefinedField(span, val, bt) => {
                SquirrelError::new(file_name, span, format!("Undefined field: {}", val), bt)
            }
            ExecError::UnhashableType { kind, span, bt } => SquirrelError::new(
                file_name,
                span,
                format!("A value of type '{}' is not hashable", kind),
                bt,
            ),
            ExecError::IllegalOperation {
                op,
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
                    file_name,
                    format!(
                        "Illegal type{} for '{}' operator",
                        if is_binary { "s" } else { "" },
                        op
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
                } = call_info;
                SquirrelError::new_labels(
                    file_name,
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
                file_name,
                span,
                if let Some(message) = message {
                    format!("Assertion failed: {}", message)
                } else {
                    "Assertion failed".to_string()
                },
                bt,
            ),
            ExecError::NumberParseError(span, message, bt) => SquirrelError::new(
                file_name,
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
                file_name,
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
                file_name,
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
            ExecError::UniterableType { kind, span, bt } => SquirrelError::new(
                file_name,
                span,
                format!("A value of type '{}' is not iterable", kind),
                bt,
            ),
            ExecError::MutatingInstantiatedClass(span, bt) => SquirrelError::new(
                file_name, span, "Cannot mutate an instantiated class".to_string(), bt
            ),
            ExecError::ExtendingNonClass(span, bt) => SquirrelError::new(
                file_name, span, "Cannot extend a non-class".to_string(), bt
            ),
            ExecError::MissingMetamethod { span, name, op, bt } => SquirrelError::new(
                file_name,
                span,
                format!("Missing metamethod '{}' on lhs operand, needed to perform operation '{}'", name, op),
                bt,
            ),
        }
    }
}

pub enum WriteOption<'a> {
    Dyn(&'a mut dyn io::Write),
    Stdout(io::Stdout),
    Stderr(io::Stderr),
}

impl<'a> io::Write for &mut WriteOption<'a> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        match self {
            WriteOption::Dyn(write) => write.write(buf),
            WriteOption::Stdout(stdout) => stdout.write(buf),
            WriteOption::Stderr(stderr) => stderr.write(buf),
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        match self {
            WriteOption::Dyn(write) => write.flush(),
            WriteOption::Stdout(stdout) => stdout.flush(),
            WriteOption::Stderr(stderr) => stderr.flush(),
        }
    }
}

impl<'a> From<Option<&'a mut dyn io::Write>> for WriteOption<'a> {
    fn from(value: Option<&'a mut dyn io::Write>) -> Self {
        value
            .map(|write| WriteOption::Dyn(write))
            .unwrap_or_else(|| WriteOption::Stdout(io::stdout()))
    }
}

struct VMState<'a> {
    root_table: Rc<RefCell<Object>>,
    stdout: WriteOption<'a>,
    stderr: WriteOption<'a>,
}

pub struct Context<'a, 'b> {
    infunc: FuncRuntime,
    vm_state: &'a mut VMState<'b>,
}

#[derive(Debug)]
pub struct FuncRuntime {
    locals: Vec<Rc<RefCell<Value>>>,
    env: Value,
    closure: Rc<RefCell<Closure>>,
}

impl FuncRuntime {
    fn new(
        func_ref: Rc<RefCell<Closure>>,
        arg_vals: Vec<Value>,
        env: Option<Value>,
        func_span: Span,
        call_span: Span,
    ) -> Result<FuncRuntime, ExecError> {
        let func_borrow = func_ref.deref().borrow();
        let ast_func = unsafe { func_borrow.ast_fn.as_ref() };

        let n_arguments = arg_vals.len();
        let n_parameters = ast_func.num_args as usize;
        let n_default = func_borrow.default_vals.len();
        assert!(n_parameters >= n_default);

        let mut arg_iter = arg_vals.into_iter();
        let zip_iter = arg_iter
            .by_ref()
            .map(Option::Some)
            .chain(std::iter::repeat(None));
        let mut locals = (0..ast_func.num_args as usize)
            .zip(zip_iter)
            .map(|(arg_idx, arg_val)| {
                let val = if let Some(arg_val) = arg_val {
                    // There is an argument
                    arg_val
                } else if arg_idx >= n_parameters - n_default {
                    // No given argument, use default
                    func_borrow.default_vals[arg_idx - (n_parameters - n_default)].clone()
                } else {
                    // No default available, not enough arguments
                    return Err(ExecError::wrong_arg_count(
                        CallInfo {
                            func_span,
                            call_span,
                        },
                        (n_parameters - n_default)..n_parameters,
                        n_arguments,
                        Some(ast_func.arg_span),
                    ));
                };
                Ok(Rc::new(RefCell::new(val)))
            })
            .collect::<Result<Vec<_>, _>>()?;

        // TODO: Can we use default args and varargs at once?
        if ast_func.is_varargs {
            let vararg_vec = if n_arguments > n_parameters {
                arg_iter.collect()
            } else {
                Vec::new()
            };
            locals.push(Rc::new(RefCell::new(Value::array(vararg_vec))));
        } else {
            if arg_iter.count() > 0 {
                return Err(ExecError::wrong_arg_count(
                    CallInfo {
                        func_span,
                        call_span,
                    },
                    (n_parameters - n_default)..n_parameters,
                    n_arguments,
                    Some(ast_func.arg_span),
                ));
            }
        }
        locals.resize_with(ast_func.num_locals as usize, || {
            Rc::new(RefCell::new(Value::Null))
        });

        for ((_parent_idx, this_idx), upvalue) in ast_func
            .upvalues
            .iter()
            .cloned()
            .zip(func_borrow.upvalues.iter())
        {
            locals[this_idx as usize] = upvalue.clone()
        }

        let env = env
            .or_else(|| func_borrow.env.clone())
            .unwrap_or(Value::Null);
        drop(func_borrow);
        Ok(FuncRuntime {
            closure: func_ref,
            locals,
            env,
        })
    }

    fn set_local(&mut self, idx: u32, val: Value) {
        *self.locals[idx as usize].deref().borrow_mut() = val;
    }

    fn get_local(&self, idx: u32) -> Value {
        self.locals[idx as usize].deref().borrow().clone()
    }
}

impl From<&Literal> for Value {
    fn from(literal: &Literal) -> Self {
        match literal {
            Literal::Integer(integer) => Value::Integer(*integer),
            Literal::Number(float) => Value::Float((*float).into()),
            Literal::String(string) => Value::string(string),
            Literal::Null => Value::Null,
            Literal::Boolean(boolean) => Value::Boolean(*boolean),
        }
    }
}
