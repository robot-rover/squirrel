use std::{
    borrow::Borrow, cell::RefCell, collections::HashMap, f32::consts::E, io, ops::{Range, RangeInclusive}, ptr::NonNull, rc::{Rc, Weak}
};

use ariadne::Color;

use crate::{
    context::{Span, SqBacktrace, SquirrelError},
    parser::ast::{self, Ident, Literal},
};

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
    IllegalKeyword(Span, SqBacktrace),
    WrongArgCount { func_span: Span, call_span: Span, expected: Range<usize>, got: usize, definition_span: Option<Span>, bt: SqBacktrace},
}

impl ExecError {
    fn undefined_variable(ident: Ident) -> Self {
        ExecError::UndefinedVariable(ident, SqBacktrace::new())
    }

    fn illegal_keyword(span: Span) -> Self {
        ExecError::IllegalKeyword(span, SqBacktrace::new())
    }

    fn wrong_arg_count(func_span: Span, call_span: Span, expected: Range<usize>, got: usize, definition_span: Option<Span>) -> Self {
        ExecError::WrongArgCount { func_span, call_span, expected, got, definition_span, bt: SqBacktrace::new() }
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
            ExecError::WrongArgCount { func_span, call_span, expected, got, definition_span, bt} => {
                let mut labels = vec![
                    (func_span, if expected.start == expected.end {
                        format!("Function expected {} arguments", expected.start)
                    } else {
                        format!("Function expected {} to {} arguments", expected.start, expected.end)
                    }, Color::Blue),
                    (call_span, format!("Call has {} arguments", got), Color::Red),
                ];
                if let Some(dspan) = definition_span {
                    labels.push((dspan, "Function defined here".to_string(), Color::Green));
                }
                SquirrelError::new_labels(file_name, format!("Function called with incorrect number of arguments"), labels, bt)
            },
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
    root_table: ObjectRef,
    stdout: WriteOption<'a>,
}

struct Context<'a, 'b> {
    infunc: FuncRuntime,
    vm_state: &'a mut VMState<'b>,
}

#[derive(Debug, Clone)]
struct H64(f64);
impl PartialEq for H64 {
    fn eq(&self, other: &Self) -> bool {
        self.0.to_bits() == other.0.to_bits()
    }
}
impl Eq for H64 {}
impl std::hash::Hash for H64 {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.to_bits().hash(state)
    }
}

impl From<f64> for H64 {
    fn from(float: f64) -> Self {
        H64(float)
    }
}

impl From<H64> for f64 {
    fn from(h64: H64) -> Self {
        h64.0
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
enum Value {
    Float(H64),
    Integer(i64),
    // TODO: This should be an Rc
    String(StringRef),
    Null,
    Object(ObjectRef),
    Array(ArrayRef),
    Weak(WeakRef),
    Function(ClosureRef),
    NativeFunction(fn(*mut Context, Vec<Value>) -> Result<Value, ExecError>),
    // Class(/* TODO */),
    // Generator(/* TODO */),
    // UserData(/* TODO */),
    // Thread(/* TODO */),
}

impl Value {
    fn truthy(&self) -> bool {
        match self {
            Value::Integer(val) if *val == 0 => false,
            Value::Float(b) if b.0 == 0.0 => false,
            Value::Null => false,
            _ => true,
        }
    }

    fn boolean(val: bool) -> Self {
        Value::Integer(if val { 1 } else { 0 })
    }

    fn float(val: f64) -> Self {
        Value::Float(H64::from(val))
    }

    fn string(val: String) -> Self {
        Value::String(StringRef(Rc::new(val)))
    }
}

#[derive(Debug, Clone)]
struct ArrayRef(Rc<RefCell<Vec<Value>>>);
rc_hash_eq!(ArrayRef, Array, Rc);
impl ArrayRef {
    fn new(vec: Vec<Value>) -> Self {
        ArrayRef(Rc::new(RefCell::new(vec)))
    }
}

#[derive(Debug, Clone)]
struct ClosureRef(Rc<Closure>);
rc_hash_eq!(ClosureRef, Function, Rc);

#[derive(Debug, Clone)]
struct ObjectRef(Rc<RefCell<Object>>);
rc_hash_eq!(ObjectRef, Object, Rc);
impl ObjectRef {
    fn new(obj: Object) -> Self {
        ObjectRef(Rc::new(RefCell::new(obj)))
    }
}

#[derive(Debug, Clone)]
struct WeakRef(Weak<RefCell<Object>>);
rc_hash_eq!(WeakRef, Weak, Weak);
impl WeakRef {
    fn new(obj: &ObjectRef) -> Self {
        WeakRef(Rc::downgrade(&obj.0))
    }

    fn empty() -> Self {
        WeakRef(Weak::new())
    }
}

#[derive(Debug, Clone, Eq)]
struct StringRef(Rc<String>);
impl PartialEq for StringRef {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_str() == other.0.as_str()
    }
}
impl std::hash::Hash for StringRef {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.as_str().hash(state)
    }
}

#[derive(Debug, Clone)]

struct Object {
    delegate: Option<ObjectRef>,
    slots: HashMap<Value, Value>,
    is_class_inst: bool,
}

impl Object {
    fn get_field(&self, key: &Value) -> Option<Value> {
        // TODO: This shouldn't be recursive
        if let Some(value) = self.slots.get(key) {
            return Some(value.clone());
        }
        match &self.delegate {
            Some(delegate) => {
                let parent = (*delegate.0).borrow();
                parent.get_field(key)
            }
            None => return None,
        }
    }
}

#[derive(Debug)]
struct Closure {
    ast_fn: NonNull<ast::Function>,
    default_vals: Vec<Value>,
    root: WeakRef,
    env: Option<WeakRef>,
}

impl ClosureRef {
    fn new(ast_fn: &ast::Function, default_vals: Vec<Value>, root: WeakRef) -> ClosureRef {
        ClosureRef(Rc::new(Closure {
            ast_fn: NonNull::from(ast_fn),
            default_vals,
            root,
            env: None,
        }))
    }
}

#[derive(Debug)]
struct FuncRuntime {
    locals: HashMap<String, Value>,
    env: Option<ObjectRef>,
    closure: ClosureRef,
}

impl FuncRuntime {
    fn new(func_ref: ClosureRef, arg_vals: Vec<Value>, env: &Option<ObjectRef>, func_span: Span, call_span: Span) -> Result<FuncRuntime, ExecError> {
        let mut locals = HashMap::new();
        let ast_func = unsafe { func_ref.0.ast_fn.as_ref() };

        let n_arguments = arg_vals.len();
        let n_parameters = ast_func.args.len();
        let n_default = func_ref.0.default_vals.len();
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
                func_ref.0.default_vals[arg_idx - (n_parameters - n_default)].clone()
            } else {
                return Err(ExecError::wrong_arg_count(
                    func_span,
                    call_span,
                    (n_parameters - n_default)..n_parameters,
                    n_arguments,
                    Some(unsafe { func_ref.0.ast_fn.as_ref() }.arg_span),
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
            locals.insert(
                "vargv".to_string(),
                Value::Array(ArrayRef::new(vararg_vec)),
            );
        } else {
            let extra_args = arg_iter.count();
            if extra_args > 0 {
                return Err(ExecError::wrong_arg_count(
                    func_span,
                    call_span,
                    (n_parameters - n_default)..n_parameters,
                    n_arguments,
                    Some(unsafe { func_ref.0.ast_fn.as_ref() }.arg_span),
                ));
            }
        }
        let env = match func_ref.0.env.as_ref() {
            Some(closure_ref) => closure_ref.0.upgrade().map(|rc| ObjectRef(rc)),
            None => env.clone(),
        };
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
            Literal::String(string) => Value::String(StringRef(Rc::new(string.clone()))),
            Literal::Null => Value::Null,
        }
    }
}
