use std::{
    borrow::Borrow,
    cell::{RefCell},
    collections::HashMap,
    ptr::NonNull,
    rc::{Rc, Weak},
};

use crate::{
    context::{Span, SqBacktrace},
    parser::ast::{self, Ident, Literal},
};

mod walker;

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

enum ExecError {
    UndefinedVariable(Ident, SqBacktrace),
    IllegalKeyword(Span, SqBacktrace),
}

impl ExecError {
    fn undefined_variable(ident: Ident) -> Self {
        ExecError::UndefinedVariable(ident, SqBacktrace::new())
    }

    fn illegal_keyword(span: Span) -> Self {
        ExecError::IllegalKeyword(span, SqBacktrace::new())
    }
}

struct Context<'a> {
    infunc: &'a mut FuncRuntime,
}

impl<'a> Context<'a> {
    pub fn new(infunc: &'a mut FuncRuntime) -> Context {
        Context { infunc }
    }
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
    String(String),
    Null,
    Object(ObjectRef),
    Array(ArrayRef),
    Weak(WeakRef),
    Function(FunctionRef),
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
struct FunctionRef(Rc<FuncLoad>);
rc_hash_eq!(FunctionRef, Function, Rc);

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
}

#[derive(Debug, Clone)]

struct Object {
    delegate: Option<ObjectRef>,
    slots: HashMap<Value, Value>,
    is_class_inst: bool,
}

impl Object {
    fn get_field(&self, key: &Value) -> Value {
        // TODO: This shouldn't be recursive
        if let Some(value) = self.slots.get(key) {
            return value.clone();
        }
        match &self.delegate {
            Some(delegate) => {
                let parent = (*delegate.0).borrow();
                parent.get_field(key)
            }
            None => panic!("Undefined variable: {:?}", key),
        }
    }
}

#[derive(Debug)]
struct FuncLoad {
    ast_fn: NonNull<ast::Function>,
    default_vals: Vec<Value>,
}

impl FunctionRef {
    fn new(ast_fn: &ast::Function, default_vals: Vec<Value>) -> FunctionRef {
        FunctionRef(Rc::new(FuncLoad {
            ast_fn: NonNull::from(ast_fn),
            default_vals,
        }))
    }
}

#[derive(Debug)]
struct FuncRuntime {
    root: WeakRef,
    env: ObjectRef,
    locals: HashMap<String, Value>,
}

impl FuncRuntime {
    fn new(
        func_ref: &FunctionRef,
        arg_vals: Vec<Value>,
        env: ObjectRef,
        root: WeakRef,
    ) -> FuncRuntime {
        let mut locals = HashMap::new();
        let ast_func = unsafe { func_ref.0.ast_fn.as_ref() };

        let n_arguments = arg_vals.len();
        let n_parameters = ast_func.args.len();
        let n_default = func_ref.0.default_vals.len();

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
                panic!("Not enough arguments")
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
                "varargs".to_string(),
                Value::Array(ArrayRef::new(vararg_vec)),
            );
        } else {
            assert_eq!(arg_iter.next(), None, "Too many arguments");
        }
        FuncRuntime { root, env, locals }
    }
}

impl From<&Literal> for Value {
    fn from(literal: &Literal) -> Self {
        match literal {
            Literal::Integer(integer) => Value::Integer(*integer),
            Literal::Number(float) => Value::Float((*float).into()),
            Literal::String(string) => Value::String(string.clone()),
            Literal::Null => Value::Null,
        }
    }
}
