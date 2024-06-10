use std::{borrow::Borrow, cell::{Ref, RefCell}, collections::HashMap, rc::{self, Rc, Weak}};

use crate::parser::ast::{self, Expr, Ident, Literal, StateRef, Statement};

mod walker;

macro_rules! rc_hash_eq {
    ($t:ty, $value:tt) => {
        impl PartialEq for $t {
            fn eq(&self, other: &Self) -> bool {
                std::ptr::eq(self.0.as_ptr(), other.0.as_ptr())
            }
        }
        impl Eq for $t {}
        impl std::hash::Hash for $t {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                self.0.as_ptr().hash(state)
            }
        }

        impl From<$t> for Value {
            fn from(rc: $t) -> Self {
                Value::$value(rc)
            }
        }
    }

}

struct Context<'a> {
    infunc: &'a mut Function
}

impl<'a> Context<'a> {
    pub fn new(infunc: &'a mut Function) -> Context {
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
rc_hash_eq!(ArrayRef, Array);
impl ArrayRef {
    fn new(vec: Vec<Value>) -> Self {
        ArrayRef(Rc::new(RefCell::new(vec)))
    }
}

#[derive(Debug, Clone)]
struct FunctionRef(Rc<RefCell<ast::Function>>);
rc_hash_eq!(FunctionRef, Function);
impl FunctionRef {
    fn new(func: ast::Function) -> Self {
        FunctionRef(Rc::new(RefCell::new(func)))
    }
}

#[derive(Debug, Clone)]
struct ObjectRef(Rc<RefCell<Object>>);
rc_hash_eq!(ObjectRef, Object);
impl ObjectRef {
    fn new(obj: Object) -> Self {
        ObjectRef(Rc::new(RefCell::new(obj)))
    }
}

#[derive(Debug, Clone)]
struct WeakRef(Weak<RefCell<Object>>);
rc_hash_eq!(WeakRef, Weak);
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
        let mut current = self;
        let mut refs: Vec<Ref<Object>> = Vec::new();
        // TODO: This shouldn't be recursive
        if let Some(value) = current.slots.get(key) {
            return value.clone();
        }
        match &current.delegate {
            Some(delegate) => {
                let parent = (*delegate.0).borrow();
                parent.get_field(key)
            },
            None => panic!("Undefined variable: {:?}", key),
        }
    }
}

#[derive(Debug)]
struct Function {
    args: Vec<(Ident, Option<Expr>)>,
    is_varargs: bool,
    body: Vec<Statement>,
    root: WeakRef,
    env: ObjectRef,
    locals: HashMap<String, Value>,
}


impl Function {
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