use std::{borrow::Borrow, cell::RefCell, collections::HashMap, io, ops::{Deref, Range}, rc::Rc};

use ariadne::Color;
use value::{Closure, Object, Value};

use crate::{
    context::{Span, SqBacktrace, SquirrelError},
    parser::ast::{Ident, Literal},
};

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

#[derive(Debug)]
pub enum ExecError {
    UndefinedVariable(Ident, SqBacktrace),
    UndefinedField(Span, Value, SqBacktrace),
    IllegalKeyword(Span, SqBacktrace),
    WrongArgCount {
        func_span: Span,
        call_span: Span,
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
    IllegalOperation {
        op: String,
        op_span: Span,
        lhs_ty: String,
        lhs_span: Span,
        rhs_ty: String,
        rhs_span: Span,
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

impl ExecError {
    variant_constructor!(UndefinedVariable undefined_variable(ident: Ident));
    variant_constructor!(UndefinedField undefined_field(span: Span, value: Value));
    variant_constructor!(IllegalKeyword illegal_keyword(span: Span));
    variant_constructor!(WrongArgCount wrong_arg_count { func_span: Span, call_span: Span, expected: Range<usize>, got: usize, definition_span: Option<Span> });
    variant_constructor!(UnhashableType unhashable_type { kind: String, span: Span });
    variant_constructor!(IllegalOperation illegal_operation { op: String, op_span: Span, lhs_ty: String, lhs_span: Span, rhs_ty: String, rhs_span: Span });

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
                func_span,
                call_span,
                expected,
                got,
                definition_span,
                bt,
            } => {
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
            ExecError::UndefinedField(span, val, bt) => SquirrelError::new(
                file_name,
                span,
                // TODO: This should use value to_string
                format!("Undefined field: {:?}", val),
                bt,
            ),
            ExecError::UnhashableType { kind, span, bt } => SquirrelError::new(
                file_name,
                span,
                format!("A value of type '{}' is not hashable", kind),
                bt,
            ),
            ExecError::IllegalOperation {
                op,
                op_span,
                lhs_ty,
                lhs_span,
                rhs_ty,
                rhs_span,
                bt,
            } => SquirrelError::new_labels(
                file_name,
                format!("Illegal types for '{}' operator", op),
                vec![
                    (lhs_span, format!("Operand of type '{}'", lhs_ty), Color::Red),
                    (op_span, format!("Operator"), Color::Blue),
                    (rhs_span, format!("Operand of type '{}'", rhs_ty), Color::Red)],
                bt,
            ),
        }
    }
}

pub enum WriteOption<'a> {
    DYN(&'a mut dyn io::Write),
    DEFAULT(io::Stdout),
}

impl<'a> io::Write for &mut WriteOption<'a> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        match self {
            WriteOption::DYN(write) => write.write(buf),
            WriteOption::DEFAULT(stdout) => stdout.write(buf),
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        match self {
            WriteOption::DYN(write) => write.flush(),
            WriteOption::DEFAULT(stdout) => stdout.flush(),
        }
    }
}

impl<'a> From<Option<&'a mut dyn io::Write>> for WriteOption<'a> {
    fn from(value: Option<&'a mut dyn io::Write>) -> Self {
        value
            .map(|write| WriteOption::DYN(write))
            .unwrap_or_else(|| WriteOption::DEFAULT(io::stdout()))
    }
}

struct VMState<'a> {
    root_table: Rc<RefCell<Object>>,
    stdout: WriteOption<'a>,
}

pub struct Context<'a, 'b> {
    infunc: FuncRuntime,
    vm_state: &'a mut VMState<'b>,
}

#[derive(Debug)]
struct FuncRuntime {
    locals: HashMap<String, Value>,
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
        let mut locals = HashMap::new();
        let func_borrow = func_ref.deref().borrow();
        let ast_func = unsafe { func_borrow.ast_fn.as_ref() };

        let n_arguments = arg_vals.len();
        let n_parameters = ast_func.args.len();
        let n_default = func_borrow.default_vals.len();
        assert!(n_parameters >= n_default);

        let mut arg_iter = arg_vals.into_iter();
        let zip_iter = arg_iter
            .by_ref()
            .map(Option::Some)
            .chain(std::iter::repeat(None));
        for (arg_idx, ((param_name, _), arg_val)) in ast_func.args.iter().zip(zip_iter).enumerate()
        {
            let val = if let Some(arg_val) = arg_val {
                arg_val
            } else if arg_idx >= n_parameters - n_default {
                func_borrow.default_vals[arg_idx - (n_parameters - n_default)].clone()
            } else {
                return Err(ExecError::wrong_arg_count(
                    func_span,
                    call_span,
                    (n_parameters - n_default)..n_parameters,
                    n_arguments,
                    Some(ast_func.arg_span),
                ));
            };
            locals.insert(param_name.clone(), val);
        }

        if ast_func.is_varargs {
            let vararg_vec = if n_arguments > n_parameters {
                arg_iter.collect()
            } else {
                Vec::new()
            };
            locals.insert("vargv".to_string(), Value::array(vararg_vec));
        } else {
            let extra_args = arg_iter.count();
            if extra_args > 0 {
                return Err(ExecError::wrong_arg_count(
                    func_span,
                    call_span,
                    (n_parameters - n_default)..n_parameters,
                    n_arguments,
                    Some(ast_func.arg_span),
                ));
            }
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
}

impl From<&Literal> for Value {
    fn from(literal: &Literal) -> Self {
        match literal {
            Literal::Integer(integer) => Value::Integer(*integer),
            Literal::Number(float) => Value::Float((*float).into()),
            Literal::String(string) => Value::string(string),
            Literal::Null => Value::Null,
        }
    }
}
